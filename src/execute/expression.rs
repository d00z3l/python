use smartstring::{SmartString, LazyCompact};
use crate::{ast::{Position, Expression, BinOpType, Parameter, FunctionStat}, 
    ScriptResult, Value, error::{ScriptError, NativeError, TypeMismatch, ScriptErrorKind}, 
    NativeType, NativeExecutor, NativeFunctionType, CallContext};
use super::{ExecuteContext, bin_op, comp_op, unary_op, statement::statements};

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn execute<X, C, T, F, E>(ctx: &mut ExecuteContext<X, C, T, F, E>, block_functions: &[Position<FunctionStat<T>>], exp: &Position<Expression<T>>) -> ScriptResult<Value<T>, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{

    if ctx.options.debug_mode {
        println!("{}, {}: Statement '{}'", exp.row(), exp.column(), exp.name());
    }

    match &**exp {

        Expression::None => {
            return ScriptError::err(exp.row(), exp.column(), ScriptErrorKind::ExpressionNone);
        }

        Expression::Constant (value) => {
            return Ok(value.clone());
        }

        Expression::Variable (scope, id, name) => {

            if *scope {
                if let Some(value) = ctx.executor.get(*id) {
                    if ctx.options.debug_mode {
                        println!("{}, {}: Variable '{}' get {}", exp.row(), exp.column(), name, value);
                    }
                    return Ok(value);
                }
            }

            if let Some(value) = ctx.env.local_get(*id) {
                if ctx.options.debug_mode {
                    println!("{}, {}: Variable '{}' get {}", exp.row(), exp.column(), name, value);
                }
                return Ok(value);
            }

            return ScriptError::err(
                exp.row(), 
                exp.column(),
                ScriptErrorKind::VariableNotFound {name: name.clone()}
            );

        }

        Expression::UnaryOp (op) => {

            let val = execute(ctx, block_functions, &op.exp)?;
            let val = unary_op::execute(&op.op, val)?;
            return Ok(val);

        }

        Expression::BinOp (op) => {

            let left_val = execute(ctx, block_functions, &op.left)?;
            let right_val = execute(ctx, block_functions, &op.right)?;

            match &op.op_type {
                BinOpType::Float => {
                    let value = match bin_op::float(&left_val, op.op, &right_val) {
                        Ok(value) => value,
                        Err(err) => return Err(err.to_script_error(op.left.row(), op.left.column())),
                    };
                    return Ok(Value::Float(value));
                }
                BinOpType::Int => {
                    let value = match bin_op::int(&left_val, op.op, &right_val) {
                        Ok(value) => value,
                        Err(err) => return Err(err.to_script_error(op.left.row(), op.left.column())),
                    };
                    return Ok(Value::Int(value));
                }
            }
        }

        Expression::Concatenate (op) => {

            let left_val = execute(ctx, block_functions, &op.left)?.to_str_boxed();
            let right_val = execute(ctx, block_functions, &op.right)?.to_str_boxed();
            let mut value : SmartString<LazyCompact> = SmartString::new();
            value.push_str(&left_val);
            value.push_str(&right_val);
            return Ok(Value::String(value.as_str().into()))

        }

        Expression::Comparison (exp) => {

            let left_val = execute(ctx, block_functions, &exp.left)?;
            for (op, other) in &exp.others {
                let right_val = execute(ctx, block_functions, other)?;
                let val = comp_op::any(other.row(), other.column(), &left_val, *op, &right_val)?;
                return Ok(Value::Bool(val));
            }

        }

        Expression::Call (call) => {

            // Get the function
            let func = &block_functions[call.index];
            
            // Add a new level to the stack
            ctx.env.locals_push().map_err(|level|ScriptError::overflow(call.name.row(), call.name.column(), level))?;
            
            // Add the values to stack
            for (para, arg) in func.parameters.iter().zip(&call.argument_expressions) {
                let value = execute(ctx, block_functions, arg)?;
                ctx.env.local_set(para.id, value);
            }

            // Execute the statment
            let value = match statements(ctx, &func.body) {
                Ok(value) => value,
                Err(e) => {
                    // Remove the stack before returning
                    ctx.env.locals_pop();
                    return Err(e);
                },
            };

            // Remove the stack
            ctx.env.locals_pop();

            if let Some(value) = value {
                return Ok(value);
            }

            return Ok(Value::None);
            
        },

        Expression::CallNative ( call ) => {

            if ctx.options.debug_mode {
                println!("{}, {}: callnative '{}'", exp.row(), exp.column(), call.name.as_str());
            }

            // Get the function
            let func = &ctx.native_functions[call.index];

            // Update the arguments
            ctx.env.argument_clear();
            for exp in call.argument_expressions.iter() {
                let val = execute(ctx, block_functions, exp)?;
                ctx.env.argument_set(val);
            }

            // Add a new level to the stack for downsteam calls
            ctx.env.locals_push().map_err(|level|ScriptError::overflow(call.name.row(), call.name.column(), level))?;

            let val = match ctx.executor.execute(ctx.native_context, func.function_type(), &CallContext::new(call.id, ctx.options), &mut ctx.env) {
                Ok(value) => value,
                Err(err) => {
                    
                    // Clear and pop
                    ctx.env.locals_pop();

                    return Err(err.to_script_error(call.name.row(), call.name.column()));
                }
            };

            // Clear and pop
            ctx.env.locals_pop();

            if ctx.options.debug_mode {
                println!("{}, {}: Return '{}'", exp.row(), exp.column(), &val);
            }

            return Ok(val);

        }

        Expression::Print (e) => {

            let first = match &e.first {
                Parameter::Constant(val) => val.clone(),
                Parameter::Expression(exp) => {
                    let val = execute(ctx, block_functions, &*exp)?;
                    val.to_smol_str()
                },
            };

            if e.others.len() == 0 {
                ctx.env.print_output.push(Position::new(exp.row(), exp.column(), first.clone()))
            } else {
                let mut str : String = first.to_string();
                for arg in &e.others {
                    match arg {
                        Parameter::Constant(val) => str.push_str(&val),
                        Parameter::Expression(exp) => {
                            let val = execute(ctx, block_functions, exp)?;
                            str.push_str(&val.to_str_boxed());
                        },
                    };
                }
                ctx.env.print_output.push(Position::new(exp.row(), exp.column(), str.into()))
            }

        }

        Expression::Str(expression) => {
            
            return Ok(Value::String(execute(ctx, block_functions, expression)?.to_str_boxed()));

        }

        Expression::Float(expression) => {

            let value = execute(ctx, block_functions, expression)?;

            if let Value::String(val) = value {
                
                // Need to manual convert as we don't support implicit conversion
                let float : f64 = match lexical::parse(val.as_str()) {
                    Ok(val) => val,
                    Err(_) => {
                        return TypeMismatch::err(expression.row(), expression.column(), "float", &val);
                    }
                };
    
                return Ok(Value::Float(float));

            } else {

                let float = match value.to_float() {
                    Ok(val) => val,
                    Err(e) => return Err(e.to_script_error(expression.row(), expression.column())),
                };
    
                return Ok(Value::Float(float));
            } 
            
        }

        Expression::Int(expression) => {
            
            let value = execute(ctx, block_functions, expression)?;

            if let Value::String(val) = value {
                
                // Need to manual convert as we don't support implicit conversion
                let int : i64 = match lexical::parse(val.as_str()) {
                    Ok(val) => val,
                    Err(_) => return TypeMismatch::err(expression.row(), expression.column(), "int", &val.to_string())
                };
    
                return Ok(Value::Int(int));

            } else {

                let int = match value.to_int() {
                    Ok(val) => val,
                    Err(e) => return Err(e.to_script_error(expression.row(), expression.column())),
                };
    
                return Ok(Value::Int(int));

            }            

        }
        
        
    }

    Ok(Value::None)
}