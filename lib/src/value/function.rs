use std::{marker::PhantomData, rc::Rc};

use variadics_please::all_tuples;

use super::{Call, Value};
use crate::{
    error::Result,
    types::{IntoType, Type},
};

pub trait FFIFunction<Marker>: Sized {
    fn into_wrapper(self) -> FFIWrapper<Self, Marker>;
    fn parameters() -> Vec<Type>;
    fn return_ty() -> Type;
}

pub struct FFIWrapper<T, Marker> {
    function: T,
    parameters: Vec<Type>,
    return_ty: Type,
    phantom: PhantomData<Marker>,
}

pub struct PbFunction<Args, R> {
    rc: Rc<dyn Call>,
    phantom: PhantomData<(Args, R)>,
}

macro_rules! impl_fn {
    ($(($generic: ident, $var: ident)),*) => {
        impl<
            T: Fn($($generic),*) -> Result<R> + FFIFunction<(R, $($generic),*)>,
            R: IntoType + Into<Value>,
            $($generic: IntoType + From<Value>),*
        > Call for FFIWrapper<T, (R, $($generic),*)> {
            fn call(&self, args: Vec<Value>) -> Result<Value> {
                #[allow(unused)]
                let mut iter = args.into_iter();
                (self.function)($($generic::from(iter.next().unwrap().deref_implicit())),*).map(Into::into)
            }
            fn parameters(&self) -> &Vec<Type> {
                &self.parameters
            }
            fn return_ty(&self) -> &Type {
                &self.return_ty
            }
        }
        impl<
            T: Fn($($generic),*) -> Result<R>,
            R: IntoType, $($generic: IntoType),*
        > FFIFunction<(R, $($generic),*)> for T {
            fn into_wrapper(self)  -> FFIWrapper<T, (R, $($generic),*)> {
                FFIWrapper {
                    function: self,
                    parameters: Self::parameters(),
                    return_ty: Self::return_ty(),
                    phantom: PhantomData,
                }
            }
            fn parameters() -> Vec<Type> {
                vec![$($generic::into_type()),*]
            }
            fn return_ty() -> Type {
                R::into_type()
            }
        }

        impl<
            R: IntoType + From<Value>,
            $($generic: IntoType + Into<Value>),*
        > IntoType for PbFunction<($($generic,)*), R> {
            fn into_type() -> Type {
                Type::Fn {
                    parameters: vec![$($generic::into_type()),*],
                    return_type: Box::new(R::into_type())
                }
            }
        }
        impl<
            R: IntoType + From<Value>,
            $($generic: IntoType + Into<Value>),*
        > From<Value> for PbFunction<($($generic,)*), R> {
            fn from(value: Value) -> Self {
                match value {
                    Value::Function(rc) => Self {
                        rc,
                        phantom: PhantomData,
                    },
                    _ => panic!("Expected a function to pass across a language barrier."),
                }
            }
        }
        impl<
            R: IntoType + From<Value>,
            $($generic: IntoType + Into<Value>),*
        > PbFunction<($($generic,)*), R> {
            #[allow(clippy::too_many_arguments)]
            pub fn call(&self, $($var: $generic),*) -> Result<R> {
                self.rc.call(vec![$($var.into()),*]).map(|v| v.deref_implicit().into())
            }
        }
    };
}

all_tuples!(impl_fn, 0, 16, P, p);
