use rustpython_parser::{ast::{Located, StmtKind, ExprKind}};
use smol_str::SmolStr;
use crate::{
    ast::{Block, Expression, Statement, Position, BuiltInType, Parameter, AssignStat, IfStat, ForRangeStat, ForInStat, WhileStat, FunctionStat, ReturnStat, FunctionParameter}, 
    ScriptResult, 
    error::{ScriptError, NativeError, ScriptErrorKind, TypeMismatch, RequiredKind, NotFoundKind, UnsupportedKind}, 
    compiler::{CompileContext, CompiledConstant, CompiledValue}, 
    Value, NativeType, NativeCompiler, NativeFunctionType
};
use super::expression::{to_name, expression};

pub (super) fn statements<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, statements: &Vec<Located<StmtKind>>, target: &mut Block<T>, inline_constants: bool) -> ScriptResult<(), E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    for stat in statements.iter() {
        statement(ctx, stat, target, inline_constants)?; 
    }
    Ok(())
}

/// Parses the statement, target is a block incase the if and loop logic can be inlined
pub (super) fn statement<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, statement: &Located<StmtKind>, target: &mut Block<T>, inline_constants: bool) -> ScriptResult<(), E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    
    if ctx.options.debug_mode {
        println!("{}, 1: Statement", statement.location.row());
    }
    
    match &statement.node {
        StmtKind::AnnAssign { target: name, annotation, value, simple: _ } => {
            
            let name = match &name.node {
                ExprKind::Name { id, ctx: _ } => {
                    id
                }
                _ => {
                    return Err(
                        ScriptError::new(
                            statement.location.row(), 
                            statement.location.column(),
                            ScriptErrorKind::Required { kind: RequiredKind::Identifier }
                        )
                    )
                }
            };

            let id = ctx.variable(&name);

            let var_type = match &annotation.node {
                ExprKind::Name { id, ctx: _ } => {
                    id
                }
                _ => {
                    return Err(
                        ScriptError::new(
                            statement.location.row(), 
                            statement.location.column(),
                            ScriptErrorKind::Required { kind: RequiredKind::Constant }
                        )
                    )
                }
            };

            let var_type = if let Some(t) = BuiltInType::from(&var_type) {
                t
            } else {
                return Err(
                    ScriptError::new(
                        annotation.location.row(), 
                        annotation.location.column(),
                        ScriptErrorKind::UnknownType { name: var_type.into() }
                    )
                )
            };

            if let Some(value) = value {
                
                // Execute the expression
                let mut exp = Position::new(value.location.row(), value.location.column(), Expression::None);
                let mut val = expression(ctx, &target.functions, value, &mut exp, inline_constants)?;

                // Convert the type 
                match var_type.to_value(&val.value()) {
                    Ok(converted) => {
                        val.value = converted;
                    }
                    Err(err) => {
                        return Err(err.to_script_error(value.location.row(), value.location.column()));
                    }
                }

                // Check to see if the variable already exists
                if let Some(_) = ctx.env.local_get(id) {
                    val = CompiledValue::new(val.value().clone(), false);
                }

                // Set the variable value
                if ctx.options.debug_mode {
                    println!("Set variable {} to value {}", &name, val.value());
                }
                ctx.env.local_set(id, val.clone());

                if val.is_constant() {
                    // Add the constant, we can check later if the assignment should be removed
                    target.constants.push(CompiledConstant::new(name.into(), statement.location.row(), val.value().clone()));
                }

                // If not we need to add the expression
                target.statements.push(Position::new(exp.row(), exp.column(), Statement::Expression(exp)));

            } else {
                return ScriptError::err(
                    annotation.location.row(),  
                    annotation.location.column(),
                    ScriptErrorKind::InitialisationRequired {name: name.into() }
                );
            }

        }

        StmtKind::Assign { targets, value, type_comment: _ } => {

            for var in targets {

                let name = to_name(var)?;
                let id = ctx.variable(&name);

                let mut exp = Position::new(value.location.row(), value.location.column(), Expression::None);
                let val = expression(ctx, &target.functions, value, &mut exp, inline_constants)?;

                // Set the variable value
                ctx.env.local_set(id, val.clone());

                if val.is_constant() {
                    // Add the constant, we can check later if the assignment should be removed
                    target.constants.push(CompiledConstant::new(name.into(), statement.location.row(), val.value().clone()));
                }

                // If not we need to add the expression
                target.statements.push(Position::new(var.location.row(), var.location.column(), Statement::Assign (AssignStat::new(id, name.into(), exp ))));
            }
            
        }

        StmtKind::Expr { value } => {

            let mut exp = Position::new(value.location.row(), value.location.column(), Expression::None);
            let val = expression(ctx, &target.functions, value, &mut exp, inline_constants)?;
            
            if val.is_constant() {
                // If it is a constant we add the value to the ast
                target.statements.push(Position::new(exp.row(), exp.column(), Statement::Expression(Position::new(exp.row(), exp.column(), Expression::Constant(val.value().clone())) )));
            } else {
                // If not we need to add the expression
                target.statements.push(Position::new(exp.row(), exp.column(), Statement::Expression(exp)));
            }

        }

        StmtKind::Return { value } => {
           
            if let Some(value) = value {
                let mut exp = Position::new(value.location.row(), value.location.column(), Expression::None);
                let val = expression(ctx, &target.functions, value, &mut exp, inline_constants)?;
                if val.is_constant() { 
                    target.statements.push(Position::new(statement.location.row(), statement.location.column(), 
                    Statement::Return(
                        Some(ReturnStat::new(
                            val.value().type_name().into(),
                            Position::new(exp.row(), exp.column(), Expression::Constant(val.value().clone()))
                        )))
                    ));
                } else {
                    target.statements.push(Position::new(statement.location.row(), statement.location.column(), 
                        Statement::Return(
                            Some(ReturnStat::new(
                                val.value().type_name().into(),
                                exp
                            ))
                        )
                    ));
                }
            } else {
                target.statements.push(Position::new(statement.location.row(), statement.location.column(), Statement::Return (None) ));
            }

        }
        StmtKind::If { test, body, orelse } => {
            
            let mut test_exp = Position::new(test.location.row(), test.location.column(), Expression::None);
            let test_val = expression(ctx, &target.functions, test, &mut test_exp, inline_constants)?;
            let mut body_block = Block::new(); 
            statements(ctx, body, &mut body_block, inline_constants)?;
            let mut orelse_block = Block::new();
            statements(ctx, orelse, &mut orelse_block, inline_constants)?;

            if test_val.is_constant() {
                // If the value is a constant we don't need to check the if condition and can instead in-line a block
                if test_val.value().to_bool().unwrap() {
                    target.statements.append(&mut body_block.statements);
                } else {
                    target.statements.append(&mut orelse_block.statements);
                }
            } else {
                target.statements.push(Position::new(test_exp.row(), test_exp.column(), Statement::If (
                    IfStat::new(
                        test_exp,
                        body_block,
                        orelse_block,
                    )
                )));
            }

        }
        StmtKind::For { target: name, iter, body, orelse, type_comment: _ } => {

            // The name of the variable that will be populated on each iteration
            let var_name = to_name(name)?;
            let id = ctx.variable(&var_name);

            let mut body_block = Block::new(); 
            ctx.env.local_set(id, CompiledValue::new(Value::Int(0), false));
            statements(ctx, body, &mut body_block, inline_constants)?;
            let mut orelse_block = Block::new();
            statements(ctx, orelse, &mut orelse_block, inline_constants)?;

            if let ExprKind::Call { func, args, keywords: _ } = &iter.node {
    
                if to_name(func)? == "range" {

                    // We have a range function
                    
                    if args.len() == 0 || args.len() > 3 {
                        return ScriptError::err(
                            func.location.row(), 
                            func.location.column(),
                            ScriptErrorKind::ArgumentsExpected { min: 1, max: Some(3), provided: args.len() }
                        );
                    }
                    
                    // Get the start argument, position 1 or default
                    let mut start = Parameter::Constant(0);
                    if args.len() > 1  {
                        
                        let arg = &args[0];
                        let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None);
                        let exp_val = expression(ctx, &target.functions, arg, &mut exp, true)?;
                        
                        if let Value::Int(val) = exp_val.value() {
                            if exp_val.is_constant() {
                                start = Parameter::Constant(*val);
                            } else {
                                start = Parameter::Expression(Box::new(exp));
                            }
                        } else {
                            return TypeMismatch::err(arg.location.row(), arg.location.column(), "int", exp_val.value().type_name());
                        }

                    }                   

                    // Get the stop argument, which is in either position 1 or 2
                    let stop;
                    let mut arg = &args[0];
                    if args.len() != 1  {
                        arg = &args[1];
                    }

                    let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None);
                    let exp_val = expression(ctx, &target.functions, arg, &mut exp, true)?;
                    if let Value::Int(val) = exp_val.value() {
                        if exp_val.is_constant() {
                            stop = Parameter::Constant(*val);
                        } else {
                            stop = Parameter::Expression(Box::new(exp));
                        } 
                    } else {
                        return TypeMismatch::err(arg.location.row(), arg.location.column(), "int", exp_val.value().type_name());
                    }                     
                    
                    // Get the start argument, position 3 or default
                    let mut step = Parameter::Constant(1);
                    if args.len() == 3 {

                        let arg = &args[2];
                        let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None);
                        let exp_val = expression(ctx, &target.functions, arg, &mut exp, true)?;
                        if let Value::Int(val) = exp_val.value() {
                            if exp_val.is_constant() {
                                step = Parameter::Constant(*val);
                            } else {
                                step = Parameter::Expression(Box::new(exp));
                            }  
                        } else {
                            return TypeMismatch::err(arg.location.row(), arg.location.column(), "int", exp_val.value().type_name());
                        }                        

                    }

                    let id = ctx.variable(&var_name);

                    target.statements.push(Position::new(
                        statement.location.row(), 
                        statement.location.column(), 
                        Statement::ForRange ( ForRangeStat::new(id, var_name.into(), start, stop, step, body_block, orelse_block ))
                    ));

                    return Ok(());

                }

            }

            // Evaluate the iter
            let mut exp = Position::new(iter.location.row(), iter.location.column(), Expression::None);
            let exp_val = expression(ctx, &target.functions, iter, &mut exp, true)?;
            
            // Does it support iteration
            if !exp_val.value().is_iterable() {
                return ScriptError::err(
                    iter.location.row(), 
                    iter.location.column(),
                    ScriptErrorKind::Required { kind: RequiredKind::Iterator }
                )
            }

            let id = ctx.variable(&var_name); 

            if exp_val.is_constant() {
                target.statements.push(Position::new(
                    statement.location.row(), 
                    statement.location.column(), 
                    Statement::ForIn (
                        ForInStat::new(
                            id,
                            var_name.into(), 
                            Position::new(exp.row(), exp.column(), Expression::Constant(exp_val.value().clone())),
                            body_block, 
                            orelse_block 
                        )
                    )
                ));
            } else {
                target.statements.push(Position::new(
                    statement.location.row(), 
                    statement.location.column(), 
                    Statement::ForIn (
                        ForInStat::new(
                            id,
                            var_name.into(), 
                            exp,
                            body_block, 
                            orelse_block 
                        )
                    )
                ));
            }

        },

        StmtKind::While { test, body, orelse } => {

            let mut body_block = Block::new(); 
            statements(ctx, body, &mut body_block, false)?;
            let mut orelse_block = Block::new();
            statements(ctx, orelse, &mut orelse_block, false)?;

            // Evaluate the iter and see if it supports iteration
            let mut exp = Position::new(test.location.row(), test.location.column(), Expression::None);
            let exp_val = expression(ctx, &target.functions, test, &mut exp, false)?;
            if !exp_val.value().is_bool() {
                return TypeMismatch::err(test.location.row(), test.location.column(), "bool", exp_val.value().type_name());
            }

            target.statements.push(Position::new(
                statement.location.row(), 
                statement.location.column(), 
                Statement::While (
                    WhileStat::new(
                        exp,
                        body_block, 
                        orelse_block 
                    )
                )
            ));

        }

        StmtKind::FunctionDef { name, args, body, decorator_list: _, returns, type_comment: _ } => {

            let mut parameters : Vec<FunctionParameter> = Vec::new();

            for arg in args.args.iter() {

                let parameter_name : SmolStr = arg.node.arg.as_str().into();
                let parameter_id = ctx.variable(&parameter_name);
                let parameter_type : SmolStr = if let Some(annotation) = &arg.node.annotation {
                    if let ExprKind::Name { id, ctx: _ } = &annotation.node {
                        id.into()
                    } else {
                        return ScriptError::err(
                            annotation.location.row(), 
                            annotation.location.column(),
                            ScriptErrorKind::Required { kind: RequiredKind::Identifier }
                        );
                    }
                } else {
                    return ScriptError::err(
                        arg.location.row(), 
                        arg.location.column(),
                        ScriptErrorKind::Required { kind: RequiredKind::Type }
                    );
                };

                if !Value::<T>::is_type(&parameter_type) {
                    return ScriptError::err(
                        arg.location.row(), 
                        arg.location.column(),
                        ScriptErrorKind::NotFound { kind: NotFoundKind::Type, name: parameter_type }
                    );
                }

                parameters.push(FunctionParameter::new(parameter_id, parameter_name, parameter_type));

            }

            let mut func_return : Option<SmolStr> = None;

            if let Some(returns) = returns {
                if let ExprKind::Name { id, ctx: _ } = &returns.node {
                    if Value::<T>::is_type(&id) {
                        // Check the it is a valid type
                        func_return = Some(id.into());
                    } else {
                        return ScriptError::err(
                            returns.location.row(), 
                            returns.location.column(),
                            ScriptErrorKind::NotFound { kind: NotFoundKind::Type, name: id.into() }
                        );
                    }
                } else {
                    return ScriptError::err(
                        returns.location.row(), 
                        returns.location.column(),
                        ScriptErrorKind::Required { kind: RequiredKind::Type }
                    );
                }
            }

            // Need to add the function arguments as variables
            for p in &parameters {
                // Can unwrap as we have already checked the type above
                let id = ctx.variable(&p.name); 
                ctx.env.local_set(id, CompiledValue::new(Value::new(&p.type_name).unwrap(), false));
            }

            let mut body_block = Block::new(); 
            statements(ctx, body, &mut body_block, false)?;

            // Check to see if the return types match
            if let Some(type_name) = &func_return {

                for stat in &body_block.statements {
                    if let Statement::Return(return_stat) = &stat.value() {
                        if let Some(return_stat) = &return_stat {
                            if &return_stat.return_type != type_name {
                                return TypeMismatch::err(stat.row(), stat.column(), return_stat.return_type.as_str(), type_name.as_str());                                
                            }
                        } else {
                            return ScriptError::err(
                                stat.row(), 
                                stat.column(),
                                ScriptErrorKind::Required { kind: RequiredKind::Return }
                            );
                        }
                    }
                }

            } else {
                for stat in &body_block.statements {
                    if let Statement::Return(return_stat) = &stat.value() {
                        if return_stat.is_some() {
                            return ScriptError::err(
                                stat.row(), 
                                stat.column(),
                                ScriptErrorKind::ReturnNotDefined
                            );
                        }
                    }
                }
            }

            target.functions.push(Position::new(
                statement.location.row(), 
                statement.location.column(), 
                FunctionStat::new(
                        name.into(),
                        parameters,
                        func_return, 
                        body_block 
                    )
                )
            );
            
        }

        _ => {
            return ScriptError::err(
                statement.location.row(), 
                statement.location.column(),
                ScriptErrorKind::Unsupported { kind: UnsupportedKind::Statement, content: format!("{:?}", statement.node).into() }
            );
        }
    }

    Ok(())

}
