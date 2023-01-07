pub (crate) mod bin_op;
pub (crate) mod comp_op;
pub (crate) mod statement;
pub (crate) mod expression;
pub (crate) mod unary_op;

use std::marker::PhantomData;

use crate::{Env, NativeFunction, Options, Value, NativeType, error::NativeError, NativeExecutor, NativeFunctionType};

#[derive(Debug)]
pub (crate) struct ExecuteContext<'a, X, C, T, F, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{
    pub (crate) executor: &'a mut X,
    pub (crate) native_context: &'a C,
    pub (crate) env:  &'a mut Env<Value<T>>,
    pub (crate) native_functions: &'a [NativeFunction<T, F>],
    pub (crate) options: Options,
    phantom: PhantomData<E>
}


impl<'a, X, C, T, F, E> ExecuteContext<'a, X, C, T, F, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{

    pub (crate) fn new(executor: &'a mut X, native_context: &'a C, env: &'a mut Env<Value<T>>, native_functions: &'a [NativeFunction<T, F>], options: Options) -> Self {
        Self {
            env,
            native_context,
            executor,
            native_functions,
            options,  
            phantom: PhantomData::default()   
        }
    }

}
