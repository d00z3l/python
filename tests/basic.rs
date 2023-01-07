use maplit::btreemap;
use python::{NativeFunction, Empty, Value, compile, Options, execute, Env, NativeFunctionOptions, EmptyFunction, EmptyCompiler, EmptyExecutor};
use indoc::indoc;

#[test]
fn test_native1() {

    let test1 : NativeFunction<Empty, EmptyFunction> = NativeFunction::new("test1".into(), Vec::new(), Value::Int(0).into(), Default::default(), EmptyFunction::None);
    let test2 : NativeFunction<Empty, EmptyFunction> = NativeFunction::new("test2".into(), Vec::new(), Value::Int(0).into(), NativeFunctionOptions::default().is_pure(), EmptyFunction::None);

    let both = vec![test1, test2];
    assert_eq!(both.len(), 2);

}

#[test]
fn test_native() {

    let test1 : NativeFunction<Empty, EmptyFunction> = NativeFunction::new("test1", vec![], Value::None.into(), NativeFunctionOptions::default().is_pure(), EmptyFunction::None);
    let test2 : NativeFunction<Empty, EmptyFunction> = NativeFunction::new("test2", vec![], Value::None.into(), NativeFunctionOptions::default().is_pure(), EmptyFunction::None);
    let funcs = vec![test1, test2];

    assert_eq!(funcs.len(), 2);

}

#[test]
fn test_constant() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script =  r#"return 1.0"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Float(1.0)));

    let script = r#"return 1"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Int(1)));

    let script = r#"return -1"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Int(-1)));

    let script = r#"return "hello""#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::String("hello".into())));

    let script = r#"return True"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Bool(true)));

    let script = r#"return False"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Bool(false)));
}

#[test]
fn test_conversion() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script =  r#"return float(1)"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Float(1.0)));

    let script =  r#"return float("1")"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Float(1.0)));

    let script =  r#"return int(1.0)"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Int(1)));

    let script =  r#"return int("1")"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::Int(1)));

    let script =  r#"return str(1)"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::String("1".into())));

    let script =  r#"return str(1.0)"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let value = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(value, Some(Value::String("1.0".into())));

}

#[test]
fn test_print() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script =  r#"print(1.0)"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output()[0].as_str(), "1.0");

    env.reset();
    let script =  r#"print("hello world")"#;
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output()[0].as_str(), "hello world");
   
}

#[test]
fn test_for_range() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script = indoc! {"
        for x in range(5):
            print(x)
    "};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 5);
    assert_eq!(env.print_output()[0].as_str(), "0");
    assert_eq!(env.print_output()[1].as_str(), "1");
    assert_eq!(env.print_output()[2].as_str(), "2");
    assert_eq!(env.print_output()[3].as_str(), "3");
    assert_eq!(env.print_output()[4].as_str(), "4");

    let script = indoc! {"
        for x in range(3, 5):
            print(x)
    "};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    env.reset();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 2);
    assert_eq!(env.print_output()[0].as_str(), "3");
    assert_eq!(env.print_output()[1].as_str(), "4");

    let script = indoc! {"
        for x in range(3, 10, 3):
            print(x)
    "};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    env.reset();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 3);
    assert_eq!(env.print_output()[0].as_str(), "3");
    assert_eq!(env.print_output()[1].as_str(), "6");
    assert_eq!(env.print_output()[2].as_str(), "9");

    let script = indoc! {"
        for x in range(15, 0, -3):
            print(x)
        "};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    env.reset();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 5);
    assert_eq!(env.print_output()[0].as_str(), "15");
    assert_eq!(env.print_output()[1].as_str(), "12");
    assert_eq!(env.print_output()[2].as_str(), "9");
    assert_eq!(env.print_output()[3].as_str(), "6");
    assert_eq!(env.print_output()[4].as_str(), "3");

    let script = indoc! {r#"
        for x in "hello":
            print(x)
        "#};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    env.reset();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 5);
    assert_eq!(env.print_output()[0].as_str(), "h");
    assert_eq!(env.print_output()[1].as_str(), "e");
    assert_eq!(env.print_output()[2].as_str(), "l");
    assert_eq!(env.print_output()[3].as_str(), "l");
    assert_eq!(env.print_output()[4].as_str(), "o");

}

#[test]
fn test_while() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script = indoc! {"
        i = 0
        while i < 5:
            print(i)
            i = i + 1
    "};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let _ = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(env.print_output().len(), 5);
    assert_eq!(env.print_output()[0].as_str(), "0");
    assert_eq!(env.print_output()[1].as_str(), "1");
    assert_eq!(env.print_output()[2].as_str(), "2");
    assert_eq!(env.print_output()[3].as_str(), "3");
    assert_eq!(env.print_output()[4].as_str(), "4");

}

#[test]
fn test_def_call() {

    let mut compiler = EmptyCompiler{};
    let mut executor = EmptyExecutor{};

    let funcs = btreemap!{};
    let mut compile_env = Env::new();
    let mut env = Env::new();

    let script = indoc! {r#"
        def test(x: int, y: int) -> int:
            return x + y
        test(10, 5)
        return test(10, 10)
    "#};
    let compiled = compile(script, &mut compiler, &mut compile_env, &funcs, Options::default()).unwrap();
    let val = execute(&mut executor, &(), &mut env, Options::default(), &compiled).unwrap();
    assert_eq!(val, Some(Value::Int(20)));

}
