/// Macro for generating Serialize implementations for enums.
///
/// Usage:
/// ```ignore
/// serialize_enum!(<T>, EnumType<T>,
///   (EnumType::UnitVariant, "UnitVariant", &EmptyObject),
///   (EnumType::DataVariant(x), "DataVariant", x)
/// );
/// ```
///
/// The first parameter specifies the type parameters in angle brackets.
/// All variants require: (Pattern, "Label", value)
#[macro_export]
macro_rules! serialize_enum {
  (<$($tp:ident),*>, $type:ty, $(($pat:pat, $label:literal, $val:expr)),* $(,)?) => {
    impl<$($tp: serde::Serialize),*> serde::Serialize for $type {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: serde::Serializer,
      {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(1))?;
        match self {
          $(
            $pat => {
              map.serialize_entry(concat!("$", $label), $val)?;
            }
          )*
        }
        map.end()
      }
    }
  };
}

/// Macro for generating Serialize implementations for structs.
///
/// Usage:
/// ```ignore
/// serialize_struct!(<T>, StructType<T>,
///   (field1, "field1"),
///   (field2, "field2")
/// );
/// ```
///
/// The first parameter specifies the type parameters in angle brackets.
/// Each subsequent tuple specifies:
/// - The field name (without &self. prefix)
/// - The JSON field label
#[macro_export]
macro_rules! serialize_struct {
  (<$($tp:ident),*>, $type:ty, $(($field:ident, $label:literal)),* $(,)?) => {
    impl<$($tp: serde::Serialize),*> serde::Serialize for $type {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: serde::Serializer,
      {
        use serde::ser::SerializeMap;
        let field_count = [$($label),*].len();
        let mut map = serializer.serialize_map(Some(field_count))?;
        $(
          map.serialize_entry($label, &self.$field)?;
        )*
        map.end()
      }
    }
  };
}
