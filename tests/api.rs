use std::{sync::{Arc}, fmt};
use maplit::btreemap;
use python::{compile, Empty, Options, NativeFunction, Value, execute, NativeParameter, Env, 
    CompiledValue, CallContext, CallCompileContext, NativeParameterOptions, EmptyError, 
    NativeCompiler, NativeExecutor, NativeFunctionType};
use parking_lot::RwLock;
use indoc::indoc;
use smol_str::SmolStr;

#[derive(Debug, Clone)]
struct Api {
    tuple: Arc<RwLock<Vec<u32>>>
}

impl Api {
    fn new() -> Self {
        Self {
            tuple: Arc::new(RwLock::new(vec![0, 1, 2]))
        }
    }
    fn get(&self, index: i64) -> i64 {
        self.tuple.read()[index as usize].into()
    }
    fn set(&self, index: i64, val: i64) {
        self.tuple.write()[index as usize] = val as u32;
    }
}

impl NativeCompiler<Empty, FunctionKind, EmptyError> for Api {
    fn compile(&mut self, _function_type: FunctionKind, _ctx: CallCompileContext, _env: &mut Env<CompiledValue<Empty>>) -> python::NativeResult<Option<CompiledValue<Empty>>, EmptyError> {
        Ok(Some(CompiledValue::new(0.into(), false)))
    }
    fn get(&self, _name: &str) -> Option<(u16, python::CompiledValue<Empty>)> {
        None
    }
}

impl NativeExecutor<Empty, (), FunctionKind, EmptyError> for Api {
    fn execute(&mut self, _native_contexts: &(), _function_type: FunctionKind, _ctx: &CallContext, env: &mut Env<Value<Empty>>) -> python::NativeResult<Value<Empty>, EmptyError> {
        let index = env.argument(0).unwrap().to_int().unwrap();
        let val = self.get(index);
        return Ok(Value::Int(val));
    }
    fn get(&self, _id: u16) -> Option<Value<Empty>> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionKind {
    Get
}
impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Get => write!(f, "get")
        }
    }
}
impl NativeFunctionType for FunctionKind{}

#[derive(Debug, Clone)]
enum NativeContext {
    Element{ hierarchy: SmolStr },
}


#[derive(Debug, Clone)]
struct Context {
    api: Api,
    native_function_context: Vec<NativeContext>
}

impl Context {

    fn new(api: Api) -> Self {
        Self {
            api,
            native_function_context: Vec::new()
        }
    }

    fn api(&self) -> &Api {
        &self.api
    }

    fn native_function_context(&self, index: usize) -> Option<&NativeContext> {
        self.native_function_context.get(index)
    }

    fn native_function_push(&mut self, ctx: NativeContext) {
        self.native_function_context.push(ctx)
    }
    
}

impl NativeCompiler<Empty, FunctionKind, EmptyError> for Context {
    fn compile(&mut self, _function_type: FunctionKind, ctx: CallCompileContext, _env: &mut Env<CompiledValue<Empty>>) -> python::NativeResult<Option<CompiledValue<Empty>>, EmptyError> {
        let id = ctx.id();
        self.native_function_push(NativeContext::Element{ hierarchy:id.to_string().into() });
        return Ok(Some(CompiledValue::new(0.into(), false)));
    }
    fn get(&self, _name: &str) -> Option<(u16, python::CompiledValue<Empty>)> {
        None
    }
}

impl NativeExecutor<Empty, (), FunctionKind, EmptyError> for Context {
    fn execute(&mut self, _native_contexts: &(), _function_type: FunctionKind, ctx: &CallContext, env: &mut Env<Value<Empty>>) -> python::NativeResult<Value<Empty>, EmptyError> {
        match self.native_function_context(ctx.id()).unwrap() {
            NativeContext::Element { hierarchy } => {
                assert_eq!(hierarchy.as_str(), ctx.id().to_string().as_str())
            }
        }
        let index = env.argument(0).unwrap().to_int().unwrap();
        let val = self.api().get(index);
        return Ok(Value::Int(val));
    }
    fn get(&self, _id: u16) -> Option<Value<Empty>> {
        None
    }
}

#[test]
fn test_if() {

    let mut api = Api::new();

    let funcs = btreemap!{"get".into() => NativeFunction::new("get", 
        vec![NativeParameter::new("index", 
            &vec!["int"], 
            NativeParameterOptions::new(true, false))], 
            Value::Int(0).into(), 
            Default::default(), 
            FunctionKind::Get
        )
    };

    let script = indoc!{r#"
        if get(0) > 2000000:
            return get(1)
        else:
            return get(2)
    "#};

    let mut env = Env::new();

    let compiled = compile(
        script, 
        &mut api,
        &mut env,
        &funcs,
        Options::default()
    ).unwrap();

    let mut env = Env::new();

    let fake_id = 2_000_001;
    api.set(0, fake_id);
    let value = execute(&mut api, &(), &mut env, Options::default(), &compiled).unwrap();
    if fake_id > 2000000 {
        assert_eq!(value, criterion::black_box(Some(Value::Int(1))));
    } else {
        assert_eq!(value, criterion::black_box(Some(Value::Int(2))));
    }

    let fake_id = 10;
    api.set(0, fake_id);
    let value = execute(&mut api, &(), &mut env, Options::default(), &compiled).unwrap();
    if fake_id > 2000000 {
        assert_eq!(value, criterion::black_box(Some(Value::Int(1))));
    } else {
        assert_eq!(value, criterion::black_box(Some(Value::Int(2))));
    }

}

#[test]
fn test_ctx() {

    let mut ctx = Context::new(Api::new());
    
    let funcs = btreemap!{"get".into() => NativeFunction::new("get", 
        vec![NativeParameter::new("index", &vec!["int"], NativeParameterOptions::new(true, false))],
        Value::Int(0).into(), 
        Default::default(),
        FunctionKind::Get 
       )};

    let script = indoc!{r#"
        if get(0) > 2000000:
            return get(1)
        else:
            return get(2)
    "#};

    let mut env = Env::new();

    let compiled = compile(script, &mut ctx, &mut env, &funcs, Options::default()).unwrap();

    let mut env = Env::new();

    let fake_id = 2_000_001;
    ctx.api().set(0, fake_id);
    let value = execute(&mut ctx, &(), &mut env, Options::default(), &compiled).unwrap();
    if fake_id > 2000000 {
        assert_eq!(value, criterion::black_box(Some(Value::Int(1))));
    } else {
        assert_eq!(value, criterion::black_box(Some(Value::Int(2))));
    }

}
