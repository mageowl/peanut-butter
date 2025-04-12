#[macro_export]
macro_rules! generate_ty {
    (str) => {
        $crate::types::Type::String
    };
    (num) => {
        $crate::types::Type::Number
    };
    (bool) => {
        $crate::types::Type::Boolean
    };
    ([]) => {
        $crate::types::Type::Unit
    };
    ([$($idx: literal: $idx_ty: tt),*,$($key: ident: $ty: tt),*]) => {
        $crate::types::Type::Table(
            HashMap::from([
                $(($crate::value::Key::Index($idx), $crate::generate_ty!($idx_ty))),*,
                $(($crate::value::Key::Named(String::from(stringify!($key))), $crate::generate_ty!($ty))),*
            ])
        )
    };
    (fn($($param: tt),*) -> $return: tt) => {
        $crate::types::Type::Fn {
            parameters: vec![$($crate::generate_ty!($param)),*],
            return_type: Box::new($crate::generate_ty!($return)),
        }
    };
    (ref $ty: tt) => {
        $crate::generate_ty!((ref $ty))
    };
    (list $ty: tt) => {
        $crate::generate_ty!((list $ty))
    };
    (map $ty: tt) => {
        $crate::generate_ty!((map $ty))
    };
    (generic($name: literal: $idx: literal)) => {
        $crate::types::Type::Generic {
            id: $idx,
            name: String::from($name),
        }
    };
    ($init: tt | $($variant: tt)|*) => {
        $crate::types::Type::Union(vec![$crate::generate_ty!($init),$($crate::generate_ty!($variant)),*])
    };
    ({$($t: tt)*}) => {
        $crate::generate_ty!($($t)*)
    };
}
