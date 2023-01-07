use std::{fmt::{self, Display}, collections::{BTreeMap}, error::Error};
use ast::{CompiledBlock};
use compiler::{CompileContext};
use execute::{ExecuteContext, statement::{root}};
use smol_str::SmolStr;

mod ast;
mod env;
mod parser;
mod compiler;
mod execute;
mod error;

pub use {ast::{Value, List, Dict, StrBoxed}, 
    env::Env, compiler::CompiledValue, error::{ScriptError, ScriptErrorKind, NativeError, NativeResult, TypeMismatch}};

pub type ScriptResult<T, E> = Result<T, ScriptError<E>>;

pub trait NativeCompiler<T, F, E> where T: NativeType<T>, F : NativeFunctionType, E: NativeError {
    fn compile(&mut self, function_type: F, ctx: CallCompileContext, env: &mut Env<CompiledValue<T>>) -> NativeResult<Option<CompiledValue<T>>, E>;
    fn get(&self, name: &str) -> Option<(u16, CompiledValue<T>)>;
}

pub trait NativeExecutor<T, C, F, E> where T: NativeType<T>, F : NativeFunctionType, E: NativeError {
    fn execute(&mut self, native_context: &C, function_type: F, ctx: &CallContext, env: &mut Env<Value<T>>) -> NativeResult<Value<T>, E>;
    fn get(&self, id: u16) -> Option<Value<T>>;
}

/// Use to iterate or not
pub trait NativeType<T> : Clone + PartialEq + Default + Display + fmt::Debug where T : NativeType<T>  {
    fn type_name(&self) -> &str;
    fn get_at(&self, index: usize) -> Option<T>;
    fn is_iterable(&self) -> bool;
    fn is_type(type_name: &str) -> bool;
    fn new(type_name: &str) -> Option<T>;
}

/// The type of function that is being executed, this should be an enum
pub trait NativeFunctionType : Clone + Copy + PartialEq + fmt::Debug + Display {}

/// Compile source code
pub fn compile<C, T, F, E>(
    script: &str,
    compiler: &mut C,
    env: &mut Env<CompiledValue<T>>, 
    native_functions: &BTreeMap<SmolStr, NativeFunction<T, F>>,
    options: Options) -> ScriptResult<Compiled<T, F>, E> 
    where C : NativeCompiler<T, F, E>, T : NativeType<T>, F: NativeFunctionType, E : NativeError {
    
    if options.debug_mode {
        println!("Compiling...")
    }

    // Validate the native functions
    for (key, val) in native_functions.iter() {
        if key != val.name() {
            return Err(ScriptError::new(0, 0, ScriptErrorKind::NameMismatch { expected: key.clone(), provided: val.name().clone() }));
        }
        // Check the parameters
        for (i, p) in val.parameters().iter().enumerate() {
            if p.options.args && i != val.parameters().len() - 1 {
                return Err(ScriptError::new(0, 0, ScriptErrorKind::VariableArgsNotLast { name: key.clone() }));
            }
        }
    }
    
    let mut ctx = CompileContext::new(env, compiler, native_functions, options);
    let ast = parser::compile(&mut ctx, script)?;
    Ok(Compiled{
        script: script.into(),
        ast: ast.into(),
        native_functions: ctx.native_functions_in_use
    })
}

/// Execute the compiled source
pub fn execute<X, C, T, F, E>(
    executor: &mut X,
    native_context: &C,
    env: &mut Env<Value<T>>, 
    options: Options, 
    compiled: &Compiled<T, F>) -> ScriptResult<Option<Value<T>>, E> 
    where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{
    
    if options.debug_mode {
        println!("Executing...")
    }
    
    let mut ctx = ExecuteContext::new(executor, native_context, env, &compiled.native_functions, options);
    root(&mut ctx, &compiled.ast)
}

#[derive(Debug, Clone, Copy)]
pub struct Options {
    debug_mode: bool
}

impl Options {

    pub fn new(debug_mode: bool) -> Self {
        Self {
            debug_mode
        }
    }

    pub fn debug_mode(&self) -> bool {
        self.debug_mode
    }

}

impl Default for Options {
    fn default() -> Self {
        Self { debug_mode: false }
    }
}

#[derive(Debug)]
pub struct CallCompileContext {
    id: usize,
    name: SmolStr,
    options: Options
}

impl CallCompileContext {

    pub fn new(id: usize, name: &SmolStr, options: Options) -> Self {
        Self {
            id,
            name: name.clone(),
            options
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn name(&self) -> &SmolStr {
        &self.name
    }

    pub fn options(&self) -> Options {
        self.options
    }

}

#[derive(Debug)]
pub struct CallContext {
    id: usize,
    options: Options
}

impl CallContext {

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn new(id: usize, options: Options) -> Self {
        Self {
            id,
            options
        }
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn id(&self) -> usize {
        self.id
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn options(&self) -> Options {
        self.options
    }

}

#[derive(Clone, Debug)]
pub struct NativeFunctionOptions {
    /// Whether to validate the parameters
    skip_validation: bool,
    /// Whether the function returns the same result given the same arguments and
    /// it has no side effects (to arguments or the environment)
    /// If true and arguments are constants the value will be inlined
    is_pure: bool,
}

impl Default for NativeFunctionOptions {
    fn default() -> Self {
        Self { skip_validation: false, is_pure: false }
    }
}

impl NativeFunctionOptions {

    pub fn is_pure(&self) -> Self {
        Self {
            skip_validation: self.skip_validation,
            is_pure: true
        }
    }

    pub fn skip_validation(&self) -> Self {
        Self {
            skip_validation: true,
            is_pure: self.is_pure
        }
    }

}

#[derive(Clone, Debug)]
pub enum NativeFunctionReturnType<T> where T : NativeType<T> {
    /// At compile time determine the return type
    /// The return type must be the same during execution
    CompileTime,
    /// Define a fixed return type and check at compile time
    Fixed(Value<T>)
}

impl<T> From<Value<T>> for NativeFunctionReturnType<T> where T : NativeType<T> {
    fn from(val: Value<T>) -> Self {
       Self::Fixed(val)
    }
}

#[derive(Clone, Debug)]
pub struct NativeFunction<T, F> where T : NativeType<T>, F : NativeFunctionType {
    /// The name of the function
    name: SmolStr,
    /// The parameters for the function to be validated
    parameters: Vec<NativeParameter>,
    /// The type of value that is returned
    return_type: NativeFunctionReturnType<T>,
    /// The options
    options: NativeFunctionOptions,
    /// The native Rust function to compile context
    function_type: F,
}

impl<T, F> NativeFunction<T, F> where T : NativeType<T>, F : NativeFunctionType {

    pub fn new(name: &str, parameters: Vec<NativeParameter>, return_type: NativeFunctionReturnType<T>, options: NativeFunctionOptions, function_type: F) -> Self {
        Self {
            name: name.into(),
            parameters,
            return_type,
            options,
            function_type
        }
    }

    pub fn name(&self) -> &SmolStr {
        &self.name
    }

    pub fn parameters(&self) -> &[NativeParameter] {
        &self.parameters
    }

    pub fn skip_validation(&self) -> bool {
        self.options.skip_validation
    }

    pub fn is_pure(&self) -> bool {
        self.options.is_pure
    }

    pub fn function_type(&self) -> F {
        self.function_type
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct NativeParameterOptions {
    required: bool,
    args: bool,
}

impl NativeParameterOptions {

    pub fn new(required: bool, args: bool) -> Self {
        Self {
            required,
            args
        }
    }

    pub fn required(&self) -> bool {
        self.required
    }

    pub fn args(&self) -> bool {
        self.args
    }

}

impl From<(bool, bool)> for NativeParameterOptions {
    fn from(values: (bool, bool)) -> Self {
        NativeParameterOptions::new(values.0, values.1)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NativeParameter {
    name: SmolStr,
    types: Vec<SmolStr>,
    options: NativeParameterOptions
}

impl NativeParameter {

    pub fn new(name: &str, types: &[&str], options: NativeParameterOptions) -> Self {
        Self {
            name: name.into(),
            types: types.iter().map(|x|x.into()).collect::<Vec<SmolStr>>(),
            options
        }
    }

    pub fn is_type(&self, value_type: &str) -> bool {
        self.types.iter().filter(|x|x.as_str() == value_type).count() > 0
    }

    pub fn types(&self) -> SmolStr {
        self.types.iter().map(|x|x.clone()).collect::<Vec<_>>().join(", ").into()
    }

}

#[derive(Debug, Clone)]
pub struct Compiled<T, F> where T : NativeType<T>, F : NativeFunctionType {
    script: SmolStr,
    ast: CompiledBlock<T>,
    native_functions: Vec<NativeFunction<T, F>>
}

impl<T, F> Compiled<T, F> where T : NativeType<T>, F : NativeFunctionType {

    pub fn script(&self) -> &SmolStr {
        &self.script
    }

    pub fn ast(&self) -> &CompiledBlock<T> {
        &self.ast
    }

    pub fn native_functions(&self) -> &[NativeFunction<T, F>]{
        &self.native_functions
    }

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EmptyFunction {
    None
}
impl fmt::Display for EmptyFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}
impl NativeFunctionType for EmptyFunction{}

pub struct EmptyCompiler {}
impl NativeCompiler<Empty, EmptyFunction, EmptyError> for EmptyCompiler {
    fn compile(&mut self, _function_type: EmptyFunction, _ctx: CallCompileContext, _env: &mut Env<CompiledValue<Empty>>) -> NativeResult<Option<CompiledValue<Empty>>, EmptyError> {
        Ok(None)
    }
    fn get(&self, _name: &str) -> Option<(u16, CompiledValue<Empty>)> {
        return None;
    }
}

pub struct EmptyExecutor {}
impl NativeExecutor<Empty, (), EmptyFunction, EmptyError> for EmptyExecutor {
    fn execute(&mut self, _native_contexts: &(), _function_type: EmptyFunction, _ctx: &CallContext, _env: &mut Env<Value<Empty>>) -> NativeResult<Value<Empty>, EmptyError> {
        todo!()
    }
    fn get(&self, _id: u16) -> Option<Value<Empty>> {
        return None;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmptyError {}
impl fmt::Display for EmptyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl Error for EmptyError {}
impl NativeError for EmptyError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Empty {}
impl Default for Empty {
    fn default() -> Self {
        Self {}
    }
}

impl NativeType<Empty> for Empty {

    fn type_name(&self) -> &str {
        "Empty".into()
    }

    fn get_at(&self, _index: usize) -> Option<Empty> {
        None
    }

    fn is_iterable(&self) -> bool {
        false
    }

    fn new(_type_name: &str) -> Option<Empty> {
        None
    }

    fn is_type(_type_name: &str) -> bool {
        false
    }

}

impl Display for Empty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("")
    }
}
