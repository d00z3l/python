mod expression;
mod statement;

use std::sync::Arc;
use rustpython_parser::{parser};
use crate::{ast::{Block}, ScriptResult, 
    error::{ScriptError, NativeError, ScriptErrorKind}, 
    compiler::CompileContext, NativeType, NativeCompiler, NativeFunctionType};
use self::statement::statements;

pub (crate) fn compile<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, script: &str) -> ScriptResult<Block<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    match parser::parse_program(script, "inline.py") {
        Ok(prog) => {
            let mut block = Block::new();
            statements(ctx, &prog, &mut block, true)?;
            Ok(block)
        }
        Err(e) => {
            ScriptError::err(e.location.row(), e.location.column(), ScriptErrorKind::ParseError(Arc::new(e.error)))
        }
    }
}

#[cfg(test)]
mod tests {
    
    use std::{collections::BTreeMap, fmt};

    use crate::{Value, ast::{Expression, Statement, Position, Comparison, Parameter, BinOpType, 
        BinOp, CallNativeExp, BinExp, ComparisonExp, PrintExp, AssignStat, IfStat, ForRangeStat, 
        ForInStat, WhileStat, ReturnStat, FunctionStat, FunctionParameter, CallExp}, NativeFunction, 
        Options, NativeParameter, compiler::CompiledConstant, Env, NativeParameterOptions, 
        NativeFunctionOptions, EmptyError, EmptyFunction, EmptyCompiler, CallCompileContext, 
        CompiledValue, NativeResult};
    use indoc::indoc;
    use maplit::{btreemap};
    use smol_str::SmolStr;
    use super::*;
    use crate::Empty;

    /*
    fn parse(script: &str) -> Vec<Located<StmtKind>> {
        parser::parse_program(script).unwrap()
    }
    */

    #[derive(Debug, Clone, Copy, PartialEq)]
    enum CallFunction {
        Call
    }
    impl fmt::Display for CallFunction {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "")
        }
    }
    impl NativeFunctionType for CallFunction{}

    pub struct CallCompiler {}
    impl NativeCompiler<Empty, CallFunction, EmptyError> for CallCompiler {
        fn compile(&mut self, _function_type: CallFunction, _ctx: CallCompileContext, _env: &mut Env<CompiledValue<Empty>>) -> NativeResult<Option<CompiledValue<Empty>>, EmptyError> {
            Ok(Some(CompiledValue::new(Value::None, false)))
        }
        fn get(&self, _name: &str) -> Option<(u16, CompiledValue<Empty>)> {
            return None;
        }
    }

    pub struct CallCompilerFloat {}
    impl NativeCompiler<Empty, CallFunction, EmptyError> for CallCompilerFloat {
        fn compile(&mut self, _function_type: CallFunction, _ctx: CallCompileContext, _env: &mut Env<CompiledValue<Empty>>) -> NativeResult<Option<CompiledValue<Empty>>, EmptyError> {
            Ok(Some(CompiledValue::new(0.0.into(), false)))
        }
        fn get(&self, _name: &str) -> Option<(u16, CompiledValue<Empty>)> {
            return None;
        }
    }

    #[test]
    fn test_constant() {

        let mut compiler = EmptyCompiler{};
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> = btreemap!{};
        let mut env = Env::new();

        let script =  r#"1.0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Float(1.0)))
            )),
            vec![]
        ));

        let script = r#"1"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Int(1))),
            )),
            vec![]
        ));

        let script = r#"-1"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Int(-1))),
            )),
            vec![]
        ));

        let script = r#""hello""#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::String("hello".into())))
            )),
            vec![]
        ));

        let script = r#"True"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Bool(true)))
            )),
            vec![]
        ));

        let script = r#"False"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Bool(false)))
            )),
            vec![]
        ));

    }

    #[test]
    fn test_conversion() {

        let mut compiler = EmptyCompiler{};
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};
        let mut env = Env::new();

        let script = r#"float(1)"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Float(1.0)))
            )),
            vec![]
        ));

        let script = r#"float("1")"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Float(1.0)))
            )),
            vec![]
        ));

        let script = r#"int(1.0)"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Int(1)))
            )),
            vec![]
        ));

        let script = r#"int("1")"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::Int(1)))
            )),
            vec![]
        ));

        let script = r#"str(1)"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::String("1".into())))
            )),
            vec![]
        ));

        let script = r#"str(1.0)"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression ( 
                Position::new(1, 0, Expression::Constant(Value::String("1.0".into())))
            )),
            vec![]
        ));

    }

    #[test]
    fn test_assign() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        let script =  r#"a = 1.0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let _ : Block<Empty> = compile(&mut ctx, script).unwrap();
        // Because the value is a constant the assign statement is removed
        assert_eq!(ctx.env.local_get(0).unwrap().value(), &Value::Float(1.0));

        let script =  r#"a = "hello""#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let _ : Block<Empty> = compile(&mut ctx, script).unwrap();
        // Because the value is a constant the assign statement is removed
        assert_eq!(ctx.env.local_get(0).unwrap().value(), &Value::String("hello".into()));

        let script =  r#"a : float = 1"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let _ : Block<Empty> = compile(&mut ctx, script).unwrap();
        // Because the value is a constant the assign statement is removed
        assert_eq!(ctx.env.local_get(0).unwrap().value(), &Value::Float(1.0));

        // Create an example using a function that doesn't return a constant
        let mut compiler = CallCompilerFloat{};
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, CallFunction>> = btreemap!{"call".into() => NativeFunction::new("call", vec![], Value::Float(0.0).into(), NativeFunctionOptions::default(), CallFunction::Call) };
        let script =  indoc! {"
            a = call()
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        // Because the value is a constant the assign statement is removed
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Assign ( AssignStat::new(0, "a".into(), Position::new(1, 4, Expression::CallNative(
                CallNativeExp::new(
                    0,
                    Position::new(1, 4, "call".into()),
                    0, 
                    Vec::new(), 
                    Vec::new() 
                )))))),
            vec![]
        ));
    }

    #[test]
    fn test_return() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        let script =  r#"return"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Return(None) ),
            vec![]
        ));

        let script =  r#"return 1.0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Return(Some(ReturnStat::new("float".into(), Position::new(1, 7, Expression::Constant(Value::Float(1.0))))))),
            vec![]
        ));

        let script =  indoc! {"
            a = 1.0
            return a
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( AssignStat::new(0, "a".into(), Position::new(1, 4, Expression::Constant(Value::Float(1.0)))))),
                Position::new(2, 0, Statement::Return ( Some( ReturnStat::new("float".into(), Position::new(2, 7, Expression::Constant(Value::Float(1.0))))) ))
            ],
            vec![],
            vec![CompiledConstant::new("a".into(), 1, Value::Float(1.0))]
        ));

        let script =  indoc! {r#"
            a = "hello"
            return a
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( AssignStat::new(0, "a".into(), Position::new(1, 4, Expression::Constant(Value::String("hello".into()))))) ),
                Position::new(2, 0, Statement::Return ( Some( ReturnStat::new("string".into(), Position::new(2, 7, Expression::Constant(Value::String("hello".into())))))))
            ],
            vec![],
            vec![CompiledConstant::new("a".into(), 1, Value::String("hello".into()))]
        ));

    }

    #[test]
    fn test_constant_arithmetic() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        let script =  r#"1 + 2 * 3"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Int(7))))),
            vec![]
        ));

        let script =  r#"1 * 2 + 3 * 10"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Int(32))))),
            vec![]
        ));

        let script =  r#"3 / 2"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Int(1))))),
            vec![]
        ));

        let script =  r#"3.0 / 2.0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Float(1.5))))),
            vec![]
        ));

        let script =  r#"3.0 / 0.0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Float(0.0))))),
            vec![]
        ));

        let script =  r#"3 / 0"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Int(0))))),
            vec![]
        ));

        let script =  r#"1 * (2 + 3) * 10"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Int(50))))),
            vec![]
        ));

        let script =  r#"return 1 * (2 + 3) * 10"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Return ( Some(ReturnStat::new("int".into(), Position::new(1, 7, Expression::Constant(Value::Int(50))))))),
            vec![]
        ));

    }

    #[test]
    fn test_function_call() {

        let mut compiler = CallCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, CallFunction>> = btreemap!{"call".into() => NativeFunction::new("call", vec![], Value::None.into(), NativeFunctionOptions::default(), CallFunction::Call) };

        let script =  indoc! {"
            call()
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Expression ( 
                    Position::new(1, 0, 
                        Expression::CallNative (
                            CallNativeExp::new(
                                0,
                                Position::new(1, 0, "call".into()), 
                                0, 
                                vec![],
                                vec![] 
                            )
                        )
                    )
                ))
            ],
            vec![],
            vec![]
        ));

        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, CallFunction>> = btreemap!{"call".into() => 
            NativeFunction::new(
                "call",
                vec![
                    NativeParameter::new("arg1", &vec!["string"], NativeParameterOptions::new(true, false)), 
                    NativeParameter::new("arg2", &vec!["float"], NativeParameterOptions::new(true, false)),
                ], 
                Value::None.into(), 
                NativeFunctionOptions::default(), 
                CallFunction::Call
            )
        };
        let script =  indoc! {r#"
            call("test", 1.0)
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, 
                Options::default()
            );
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Expression ( 
                    Position::new(1, 0, 
                        Expression::CallNative ( 
                            CallNativeExp::new(
                                0,
                                Position::new(1, 0, "call".into()), 
                                0, 
                                vec![
                                    Position::new(1, 5, Expression::Constant(Value::String("test".into()))), 
                                    Position::new(1, 13, Expression::Constant(Value::Float(1.0)))
                                ],
                                vec![("arg1".into(), 0), ("arg2".into(), 1)] 
                            )
                        ))
                    )
                )
            ],
            vec![],
            vec![]
        ));

    }

    #[test]
    fn test_compare() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        let script =  r#"100 == 5"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Bool(false))))),
            vec![]
        ));

        let script =  r#"100 == 100 < 200"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Bool(true))))),
            vec![]
        ));

        let script =  r#"100 == 100 > 200"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Bool(false))))),
            vec![]
        ));

        let script =  r#"100 == 100 == 100 == 100"#;
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statement(
            Position::new(1, 0, Statement::Expression( Position::new(1, 0, Expression::Constant(Value::Bool(true))))),
            vec![]
        ));
    }

    #[test]
    fn test_if() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script =  indoc! {"
            a = 1.0
            if a == 1.0:
                return 1.0
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( 
                    AssignStat::new(
                        0,
                        "a".into(), 
                        Position::new(1, 4, Expression::Constant(Value::Float(1.0))) 
                    )
                )),
                Position::new(3, 4, Statement::Return ( 
                    Some(ReturnStat::new("float".into(), Position::new(3, 11, Expression::Constant(Value::Float(1.0)))))
                ))
            ],
            vec![],
            vec![CompiledConstant { name: "a".into(), line: 1, value: Value::Float(1.0) }]
        ));

        // An if statement that will inline nothing
        let script =  indoc! {"
            a = 1.0
            if a == 2.0:
                return 1.0
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( 
                    AssignStat::new(
                        0,
                        "a".into(), 
                        Position::new(1, 4, Expression::Constant(Value::Float(1.0))) 
                    )
                ))
            ],
            vec![],
            vec![CompiledConstant { name: "a".into(), line: 1, value: Value::Float(1.0) }]
        ));

        // An if statement that will inline the body
        let script =  indoc! {"
            a = 1.0
            if a == 1.0:
                return 1.0
            else:
                return 2.0
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( 
                    AssignStat::new(
                        0,
                        "a".into(), 
                        Position::new(1, 4, Expression::Constant(Value::Float(1.0))) 
                    )
                )),
                Position::new(3, 4, Statement::Return ( 
                    Some(ReturnStat::new("float".into(), Position::new(3, 11, Expression::Constant(Value::Float(1.0)))))
                ))
            ],
            vec![],
            vec![CompiledConstant { name: "a".into(), line: 1, value: Value::Float(1.0) }]
        ));

        // An if statement that will inline the else
        let script =  indoc! {"
            a = 1.0
            if a == 2.0:
                return 1.0
            else:
                return 2.0
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( 
                    AssignStat::new(
                        0,
                        "a".into(), 
                        Position::new(1, 4, Expression::Constant(Value::Float(1.0))) 
                    )
                )),
                Position::new(5, 4, Statement::Return ( 
                    Some(ReturnStat::new("float".into(), Position::new(5, 11, Expression::Constant(Value::Float(2.0)))))
                ))
            ],
            vec![],
            vec![CompiledConstant { name: "a".into(), line: 1, value: Value::Float(1.0) }]
        ));


        // An if statement that will get inlined
        let mut compiler = CallCompilerFloat{};
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, CallFunction>> =btreemap!{"call".into() => NativeFunction::new("call", vec![], Value::Float(0.0).into(), NativeFunctionOptions::default(), CallFunction::Call)};
        let script =  indoc! {"
            a = call()
            if a == 1.0:
                return 1.0
            else:
                return 2.0
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( AssignStat::new(0, "a".into(), Position::new(1, 4, 
                    Expression::CallNative ( 
                        CallNativeExp::new(
                            0,
                            Position::new(1, 4, "call".into()), 
                            0, 
                            vec![],
                            vec![] 
                        )
                    ))))
                ),
                Position::new(2, 3, Statement::If ( 
                    IfStat::new(
                        Position::new(2, 3, Expression::Comparison (
                            ComparisonExp::new( 
                                Position::new(2, 3, Expression::Variable(false, 0, "a".into())), 
                                vec![
                                    (Comparison::Equal, Position::new(2, 8, Expression::Constant(Value::Float(1.0))))
                                ] 
                            )
                        )),
                        Block::with_statements(
                            vec![
                                Position::new(3, 4, Statement::Return ( 
                                    Some(ReturnStat::new("float".into(), Position::new(3, 11, Expression::Constant(Value::Float(1.0)))))
                                ))
                            ],
                            vec![],
                            vec![]
                        ),
                        Block::with_statements(
                            vec![
                                Position::new(5, 4, Statement::Return ( 
                                    Some(ReturnStat::new("float".into(), Position::new(5, 11, Expression::Constant(Value::Float(2.0)))))
                                ))
                            ],
                            vec![],
                            vec![]
                        )
                    )
                ))
            ],
            vec![],
            vec![]
        ));

    }

    #[test]
    fn test_print() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script =  indoc! {r#"
            print("test")
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Expression ( Position::new(1, 0, Expression::Print ( PrintExp::new(Parameter::Constant("test".into()), vec![] )) )))
            ],
            vec![],
            vec![]
        ));

    }

    #[test]
    fn test_for() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script =  indoc! {"
            for x in range(5):
                print(x)
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::ForRange (
                    ForRangeStat::new(
                        0,
                        "x".into(),
                        Parameter::Constant(0),
                        Parameter::Constant(5),
                        Parameter::Constant(1),
                        Block::with_statements(
                            vec![
                                Position::new(2, 4, Statement::Expression ( Position::new(2, 4, Expression::Print (
                                    PrintExp::new( 
                                        Parameter::Expression(Box::new(Position::new(2, 10, Expression::Variable(false, 0, "x".into())))), 
                                        vec![] 
                                    )))
                                ))
                            ],
                            vec![],
                            vec![]
                        ),
                        Block::new(),
                    )
                ))
            ],
            vec![],
            vec![]
        ));

        // An if statement that will inline the body
        let script =  indoc! {r#"
            for x in "hello":
                print(x)
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::ForIn (
                    ForInStat::new(
                        0,
                        "x".into(),
                        Position::new(1, 9, Expression::Constant(Value::String("hello".into()))),
                        Block::with_statements(
                            vec![
                                Position::new(2, 4, Statement::Expression ( Position::new(2, 4, Expression::Print ( 
                                        PrintExp::new(
                                            Parameter::Expression(Box::new(Position::new(2, 10, Expression::Variable(false, 0, "x".into())))), 
                                            vec![] 
                                        )
                                    )) 
                                ))
                            ],
                            vec![],
                            vec![]
                        ),
                        Block::new(),
                    )
                ))
            ],
            vec![],
            vec![]
        ));

    }

    #[test]
    fn test_while() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script = indoc! {"
            i = 0
            while i < 5:
                print(i)
                i = i + 1
        "};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(1, 0, Statement::Assign ( 
                    AssignStat::new(
                        0,
                        "i".into(), 
                        Position::new(1, 4, Expression::Constant(Value::Int(0))) 
                    )
                )),
                Position::new(2, 0, Statement::While (
                    WhileStat::new(
                        Position::new(2, 6, Expression::Comparison ( 
                            ComparisonExp::new(
                                Position::new(2, 6, Expression::Variable(false, 0, "i".into())), 
                                vec![(Comparison::LessThan, Position::new(2, 10, Expression::Constant(Value::Int(5))))]
                            )
                        )),
                        Block::with_statements(
                            vec![
                                Position::new(3, 4, Statement::Expression ( 
                                    Position::new(3, 4, 
                                        Expression::Print (
                                            PrintExp::new( 
                                                Parameter::Expression(Box::new(Position::new(3, 10, Expression::Variable(false, 0, "i".into())))), 
                                                vec![] 
                                            )
                                        )) 
                                )),
                                Position::new(4, 4, Statement::Assign (
                                    AssignStat::new( 
                                        0,
                                        "i".into(), 
                                        Position::new(4, 8, Expression::BinOp ( 
                                            BinExp::new(BinOpType::Int, 
                                                Position::new(4, 8, Expression::Variable(false, 0, "i".into())), 
                                                BinOp::Add, Position::new(4, 12, Expression::Constant(Value::Int(1))))
                                            ) 
                                        ) 
                                    )
                                )),
                            ],
                            vec![],
                            vec![]
                        ),
                        Block::new(),
                    )
                ))
            ],
            vec![],
            vec![
                CompiledConstant::new("i".into(), 1, Value::Int(0) )
            ]
        ));


    }

    #[test]
    fn test_def() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script = indoc! {r#"
            def test(x: int, y: int) -> int:
                return x + y
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![],
            vec![
                Position::new(1, 0, FunctionStat::new(
                    "test".into(), 
                    vec![FunctionParameter::new(0, "x".into(), "int".into()), FunctionParameter::new(1, "y".into(), "int".into())], 
                    Some("int".into()), Block::with_statements(
                        vec![
                            Position::new(
                                2, 
                                4, 
                                Statement::Return(Some(ReturnStat::new("int".into(), 
                                    Position::new(2, 11,
                                        Expression::BinOp(BinExp::new(
                                            BinOpType::Int, 
                                            Position::new(2, 11,
                                                Expression::Variable(false, 0, "x".into())
                                            ),
                                            BinOp::Add,
                                            Position::new(2, 15,
                                                Expression::Variable(false, 1, "y".into())
                                            ),
                                        ))
                                    )
                                )))
                            )
                        ],
                        vec![],
                        vec![]
                    )))
            ],
            vec![]
        ));

    }

    #[test]
    fn test_def_call() {

        let mut compiler = EmptyCompiler{};
        let mut env = Env::new();
        let funcs : BTreeMap<SmolStr, NativeFunction<Empty, EmptyFunction>> =btreemap!{};

        // An if statement that will inline the body
        let script = indoc! {r#"
            def test(x: int, y: int) -> int:
                return x + y
            test(10, 5)
        "#};
        let mut ctx = CompileContext::new(&mut env, &mut compiler, &funcs, Options::default());
        let compiled : Block<Empty> = compile(&mut ctx, script).unwrap();
        assert_eq!(compiled, Block::with_statements(
            vec![
                Position::new(3, 0, Statement::Expression(
                    Position::new(3, 0, Expression::Call (
                        CallExp::new(
                            Position::new(3, 0, "test".into()),
                            0,
                            vec![
                                Position::new(3, 5, Expression::Constant(Value::Int(10))),
                                Position::new(3, 9, Expression::Constant(Value::Int(5))),
                            ]
                        )
                    )
                )))
            ],
            vec![
                Position::new(1, 0, FunctionStat::new(
                    "test".into(), 
                    vec![FunctionParameter::new(0, "x".into(), "int".into()), FunctionParameter::new(1, "y".into(), "int".into())], 
                    Some("int".into()), Block::with_statements(
                        vec![
                            Position::new(2, 4, 
                                Statement::Return(Some(ReturnStat::new("int".into(), 
                                    Position::new(2, 11,
                                        Expression::BinOp(BinExp::new(
                                            BinOpType::Int, 
                                            Position::new(2, 11,
                                                Expression::Variable(false, 0, "x".into())
                                            ),
                                            BinOp::Add,
                                            Position::new(2, 15,
                                                Expression::Variable(false, 1, "y".into())
                                            ),
                                        ))
                                    )
                                )))
                            )
                        ],
                        vec![],
                        vec![]
                    )))
            ],
            vec![]
        ));

    }

}
