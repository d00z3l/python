use std::{sync::Arc, collections::BTreeMap, fmt};
use maplit::btreemap;
use python::{compile, execute, Options, Value, Empty, NativeFunction, NativeParameter, Env, CallContext, 
    NativeParameterOptions, EmptyError, NativeCompiler, NativeFunctionType, NativeExecutor};
use indoc::indoc;
use parking_lot::RwLock;
use rand::Rng;
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

impl NativeCompiler<Empty, FunctionKind, EmptyError> for Api {
    fn compile(&mut self, _function_type: FunctionKind, _ctx: python::CallCompileContext, _env: &mut Env<python::CompiledValue<Empty>>) -> python::NativeResult<Option<python::CompiledValue<Empty>>, EmptyError> {
        Ok(None)
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

fn main()  { 

    let mut api = Api::new();

    let funcs : BTreeMap<SmolStr, NativeFunction<Empty, FunctionKind>> = btreemap!{"call".into() => 
        NativeFunction::new("get", 
        vec![NativeParameter::new("index", &vec!["int"], NativeParameterOptions::new(true, false))], 
        Value::Int(0).into(), 
        Default::default(), 
        FunctionKind::Get
    )};

    let script = indoc!{r#"
        if get(0) > 2000000:
            get(1)
        else:
            get(2)
    "#};

    let mut env = Env::new();

    let compiled = compile::<Api, Empty, FunctionKind, EmptyError>(
        script, 
        &mut api,
        &mut env,
        &funcs,
        Options::default()
    ).unwrap();
    
    let mut env = Env::new();
    let mut rng = rand::thread_rng();
    let opts = Options::default();

    for _ in 0..1_000_000_000 {
        let fake_id = rng.gen_range(1..(u32::MAX as i64));
        api.set(0, fake_id);

        let value = execute(&mut api, &(), &mut env, opts, &compiled).unwrap();
        if fake_id > 2000000 {
            assert_eq!(value, Some(Value::Int(1)), "Fake id is: {}", fake_id);
        } else {
            assert_eq!(value, Some(Value::Int(2)), "Fake id is: {}", fake_id);
        }
    }

}