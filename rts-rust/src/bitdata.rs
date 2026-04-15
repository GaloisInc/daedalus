

#[macro_export]
macro_rules! bitdata {
    ($name: ident, $width: literal) => {
      #[repr(transparent)]
      #[derive(Clone,Copy,PartialEq,Eq,PartialOrd,Ord)]
      pub struct $name { rep: $crate::U<$width> }

      $crate::by_value!($name);

      impl $name {
        pub const WIDTH: u32 = $width;
        pub fn from_bits_unchecked(x: $crate::U<$width>) -> $name {
          $name { rep: x }
        }

        pub fn to_bits(self) -> $crate::U<$width> { self.rep }
      }
    };
}




#[macro_export]
macro_rules! bitdata_case {
    ( $ty:ty, $case_ty:ident
      $(, ($mask:literal $(, ($p:literal, $con:ident, $t:ty))*) )*
    ) => {

      impl $ty {

        #[inline]
        pub fn to_enum(self) -> $case_ty {
          #[inline]
          fn mask<T>(mask_val: T, bs: T) -> T
          where T: std::ops::BitAnd<Output=T> { mask_val & bs }
          let bs = self.to_bits();
          $(
            match mask($mask, bs.into()) {
              $(
                $p => return $case_ty::$con(<$t>::from_bits_unchecked(bs)),
               )*
               _ => {}
            };
          )*
          return $case_ty::Junk
        }
      }
    };
}

#[macro_export]
macro_rules! bitdata_field {
    ($ty: ty, $fname: ident, $ftype: ty, $offset: literal) => {
    impl $ty {
      pub fn $fname(self) -> $ftype {
        let r    = self.rep;
        let mask = !(!(r ^ r) << (<$ftype>::WIDTH as usize));
        let v    = (r >> $offset) & mask;
        <$ftype>::from_bits_unchecked(v.cast_to())
      }
    }
    };
}

#[macro_export]
macro_rules! bitdata_con {
    ($tname: ident $(, ($offset: literal, $value: expr))*) => {
        { const W: u32 = $tname::WIDTH;
          $tname { rep: <$crate::U<{W}>>::from(0u8)
                        $( | ($value.to_bits().cast_to::<false,{W}>() << $offset)
                         )*
                  }
        }
    };
}

#[macro_export]
macro_rules! bitdata_struct_serialize {
    ($ty:ty $(, ($label:literal, $method:ident))*) => {
        impl serde::Serialize for $ty {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                use serde::ser::SerializeStruct;
                const FIELD_COUNT: usize = <[&str]>::len(&[$(stringify!($label)),*]);
                let mut state = serializer.serialize_struct(stringify!($ty), FIELD_COUNT)?;
                $(
                    state.serialize_field($label, &self.$method())?;
                )*
                state.end()
            }
        }
    };
}

#[macro_export]
macro_rules! bitdata_union_serialize {
    ($ty:ty, $case_ty:ident $(,($con:ident, $label:literal))*) => {
        impl serde::Serialize for $ty {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
              use serde::ser::SerializeMap;
              match self.to_enum() {
                  $($case_ty::$con(x) => {
                      let mut map = serializer.serialize_map(Some(1))?;
                      map.serialize_entry($label, &x)?;
                      map.end()
                  },)*
                  $case_ty::Junk => unreachable!()
              }
            }
        }
    };
}

