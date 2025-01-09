use std::marker::PhantomData;

use variadics_please::all_tuples;

use super::{Call, Value};
use crate::{
    error::Result,
    types::{IntoType, Type},
};

pub trait FFIFunction<Marker>: Sized {
    fn parameters() -> Vec<Type>;
    fn return_ty() -> Type;
    fn into_wrapper(self) -> FFIWrapper<Self, Marker>;
}

pub struct FFIWrapper<T, M> {
    function: T,
    phantom: PhantomData<M>,
}

macro_rules! impl_fn {
    ($($generic: ident),*) => {
        impl<T: Fn($($generic),*) -> Result<R>, R: Into<Value>, $($generic: From<Value>),*> Call for FFIWrapper<T, (R, $($generic),*)> {
            fn call(&self, args: Vec<Value>) -> Result<Value> {
                #[allow(unused)]
                let mut iter = args.into_iter();
                (self.function)($($generic::from(iter.next().unwrap().deref_implicit())),*).map(Into::into)
            }
        }
        impl<T: Fn($($generic),*) -> Result<R>, R: IntoType, $($generic: IntoType),*> FFIFunction<(R, $($generic),*)> for T {
            fn parameters() -> Vec<Type> {
                vec![$($generic::into_type()),*]
            }
            fn return_ty() -> Type {
                R::into_type()
            }
            fn into_wrapper(self)  -> FFIWrapper<T, (R, $($generic),*)> {
                FFIWrapper {
                    function: self,
                    phantom: PhantomData,
                }
            }
        }
    };
}

all_tuples!(impl_fn, 0, 16, P);
