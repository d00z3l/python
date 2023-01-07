use std::{fmt::Display, collections::BTreeMap, marker::PhantomData};
use smol_str::SmolStr;
use crate::{Value, NativeFunction, Options, Env, NativeType, error::NativeError, NativeCompiler, NativeFunctionType};

#[derive(Debug)]
pub struct CompileContext<'a, C, F, T, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    pub (crate) env: &'a mut Env<CompiledValue<T>>,
    pub (crate) compiler: &'a mut C,
    pub (crate) options: Options,
    pub (crate) native_functions: &'a BTreeMap<SmolStr, NativeFunction<T, F>>,
    pub (crate) native_functions_in_use: Vec<NativeFunction<T, F>>,
    pub (crate) native_call_id: usize,
    pub (crate) variables: Vec<SmolStr>,
    phantom: PhantomData<E>
}

impl<'a, C, F, T, E> CompileContext<'a, C, F, T, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    pub fn new(env: &'a mut Env<CompiledValue<T>>, compiler: &'a mut C, native_functions: &'a BTreeMap<SmolStr, NativeFunction<T, F>>, options: Options) -> Self {
        Self {
            env,
            compiler,
            options,
            native_functions,
            native_functions_in_use: Vec::new(),
            native_call_id: 0,
            phantom: PhantomData::default(),
            variables: Vec::new()
        }
    }

    pub fn variable(&mut self, name: &str) -> u16 {
        if let Some(i) = self.variables.iter().position(|x|x.as_str() == name) {
            return i as u16
        }
        let i = self.variables.len() as u16;
        self.variables.push(name.into());
        i
    }

}

#[derive(Debug, Clone, PartialEq, Default)]
pub (crate) struct CompiledConstant<T> where T : NativeType<T> {
    pub (crate) name: SmolStr,
    pub (crate) line: usize,
    pub (crate) value: Value<T>,
}

impl<T> CompiledConstant<T> where T : NativeType<T> { 

    pub fn new(name: SmolStr, line: usize, value: Value<T>) -> Self {
        Self {
            name,
            line,
            value
        }
    }

}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct CompiledValue<T> where T : NativeType<T> {
    pub (crate) value: Value<T>,
    is_constant: bool
}

impl<T> CompiledValue<T> where T : NativeType<T> {

    pub fn new(value: Value<T>, is_constant: bool) -> Self {
        Self {
            value,
            is_constant
        }
    }

    pub fn new_constant(value: Value<T>) -> Self {
        Self {
            value,
            is_constant: true
        }
    }

    pub fn is_constant(&self) -> bool {
        self.is_constant
    }

    pub fn value(&self) -> &Value<T> {
        &self.value
    }

}

impl<T> Display for CompiledValue<T> where T : NativeType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value().to_string())
    }
}

impl<T> NativeType<CompiledValue<T>> for CompiledValue<T> where T : NativeType<T> {
    
    fn type_name(&self) -> &str {
        self.value().type_name()
    }

    fn get_at(&self, index: usize) -> Option<CompiledValue<T>> {
        if let Some(value) = self.value().get_at(index) {
            return Some(Self{
                is_constant: self.is_constant,
                value
            })
        }
        None
    }

    fn is_iterable(&self) -> bool {
        self.value().is_iterable()
    }

    fn new(type_name: &str) -> Option<CompiledValue<T>> {
        if let Some(val) = Value::new(type_name) {
            return Some(CompiledValue::new(val, false));
        }
        None
    }

    fn is_type(type_name: &str) -> bool {
        let val : Option<Value<T>> = Value::new(type_name);
        if let Some(_) = val {
            return true;
        }
        false
    }

}
