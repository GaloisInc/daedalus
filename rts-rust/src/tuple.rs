use crate::{AsDDL, Clo, DDLSerialize, Type};

struct TupleElements<'a, T>(&'a T);

macro_rules! tuple_type {
  ($(($ty:ident, $field:tt)),+) => {
    impl<$($ty: Type),+> Type for ($($ty,)+) {
      // Borrow each field independently.
      type B<'a> = ($(<$ty as Type>::B<'a>,)+);

      fn bor(&self) -> Self::B<'_> {
        ($(self.$field.bor(),)+)
      }
    }

    impl<$($ty: Clo),+> Clo for ($($ty,)+) {
      type O = ($(<$ty as Clo>::O,)+);

      fn clo(self) -> Self::O {
        ($(self.$field.clo(),)+)
      }
    }

    impl<$($ty: DDLSerialize),+> serde::Serialize
      for TupleElements<'_, ($($ty,)+)> {
      fn serialize<S: serde::Serializer>(&self, serializer: S)
        -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(None)?;
        $(
          seq.serialize_element(&AsDDL(&self.0.$field))?;
        )+
        seq.end()
      }
    }

    impl<$($ty: DDLSerialize),+> DDLSerialize for ($($ty,)+) {
      fn ddl_serialize<S: serde::Serializer>(&self, serializer: S)
        -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("$$tuple", &TupleElements(self))?;
        map.end()
      }
    }
  }
}

tuple_type!((A, 0), (B, 1));
tuple_type!((A, 0), (B, 1), (C, 2));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10), (L, 11));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10), (L, 11), (M, 12));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10), (L, 11), (M, 12), (N, 13));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10), (L, 11), (M, 12), (N, 13), (O, 14));
tuple_type!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7), (I, 8), (J, 9), (K, 10), (L, 11), (M, 12), (N, 13), (O, 14), (P, 15));
