use crate::{ast::{Block, Statement, Position, Parameter, FunctionStat, CompiledBlock, OneOrMany}, 
    ScriptResult, Value, NativeType, error::NativeError, NativeExecutor, NativeFunctionType};
use super::{ExecuteContext, expression::{self}};

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn root<X, C, T, F, E>(ctx: &mut ExecuteContext<X, C, T, F, E>, block: &CompiledBlock<T>) -> ScriptResult<Option<Value<T>>, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{
    match &block.statements {
        OneOrMany::One(stat) => {
            if let Some(return_value) = statement(ctx, &block.functions, stat, true)? {
                if return_value != Value::None {
                    return Ok(Some(return_value));
                }
            } 
        }
        OneOrMany::Many(statements) => {
            for (last, stat) in statements.iter() {
                if let Some(return_value) = statement(ctx, &block.functions, stat, *last)? {
                    if return_value != Value::None {
                        return Ok(Some(return_value));
                    }
                }     
            }
        }
    }
    
    Ok(None)
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn statements<X, C, T, F, E>(ctx: &mut ExecuteContext<X, C, T, F, E>, block: &Block<T>) -> ScriptResult<Option<Value<T>>, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{
    for stat in block.statements.iter() {
        if let Some(return_value) = statement(ctx, &block.functions, stat, false)? {
            if return_value != Value::None {
                return Ok(Some(return_value));
            }
        }     
    }
    Ok(None)
}

#[cfg_attr(not(debug_assertions), inline(always))]
fn statement<X, C, T, F, E>(ctx: &mut ExecuteContext<X, C, T, F, E>, block_functions: &[Position<FunctionStat<T>>], statement: &Position<Statement<T>>, return_expression: bool) -> ScriptResult<Option<Value<T>>, E> where X : NativeExecutor<T, C, F, E>, T : NativeType<T>, F : NativeFunctionType, E : NativeError{

    if ctx.options.debug_mode {
        println!("{}, {}: Statement '{}'", statement.row(), statement.column(), statement.name());
    }

    match &**statement {

        Statement::Return (s) => {
            if let Some(s) = s {
                return Ok(Some(expression::execute(ctx, block_functions, &s.exp)?))
            } else {
                return Ok(Some(Value::None));
            }
        }

        Statement::Assign (s) => {
            let value = expression::execute(ctx, block_functions, &s.value)?;
            if ctx.options.debug_mode {
                println!("{}, {}: Assign variable '{}' value {}", statement.row(), statement.column(), &s.name, value);
            }
            ctx.env.local_set(s.id, value);
        }

        Statement::Expression(e) => {
            let value = expression::execute(ctx, block_functions, e)?;
            if return_expression {
                return Ok(Some(value));
            }        
        }

        Statement::If (s) => {
            
            // Evaluate the condition
            let condition = match expression::execute(ctx, block_functions, &s.condition)?.to_bool() {
                Ok(value) => value,
                Err(err) => return Err(err.to_script_error(s.condition.row(), s.condition.column())),
            };

            // Execute the statements
            if condition {
                return statements(ctx, &s.body);
            } else {
                return statements(ctx, &s.orelse);
            }

        }
 
        Statement::ForRange ( s ) => {

            let start = match &s.start {
                Parameter::Constant(val) => *val,
                Parameter::Expression(exp) => {
                    match expression::execute(ctx, block_functions, exp)?.to_int() {
                        Ok(val) => val,
                        Err(e) => return Err(e.to_script_error(statement.row(), statement.column())),
                    }
                }
            };

            let stop = match &s.stop {
                Parameter::Constant(val) => *val,
                Parameter::Expression(exp) => {
                    match expression::execute(ctx, block_functions, exp)?.to_int() {
                        Ok(val) => val,
                        Err(e) => return Err(e.to_script_error(statement.row(), statement.column())),
                    }
                }
            };

            let step = match &s.step {
                Parameter::Constant(val) => *val,
                Parameter::Expression(exp) => {
                    match expression::execute(ctx, block_functions, exp)?.to_int() {
                        Ok(val) => val,
                        Err(e) => return Err(e.to_script_error(statement.row(), statement.column())),
                    }
                }
            };

            if start > stop && step < 0 {
                for i in (stop+1..start+1).rev().step_by(-step as usize) {
                    ctx.env.local_set(s.var_id, Value::Int(i));
                    let val = statements(ctx, &s.body)?;
                    if let Some(val) = val {
                        return Ok(Some(val));
                    }
                }
            } else {
                for i in (start..stop).step_by(step as usize) {
                    ctx.env.local_set(s.var_id, Value::Int(i));
                    let val = statements(ctx, &s.body)?;
                    if let Some(val) = val {
                        return Ok(Some(val));
                    }
                }
            }

            if s.orelse.statements.len() > 0 {
                let val = statements(ctx, &s.orelse)?;
                if let Some(val) = val {
                    return Ok(Some(val));
                }
            }
            
        }

        Statement::ForIn (s) => {

            let value = expression::execute(ctx, block_functions, &s.iter)?;
            for val in value.iter() {
                ctx.env.local_set(s.var_id, val);
                let val = statements(ctx, &s.body)?;
                if let Some(val) = val {
                    return Ok(Some(val));
                }
            }

            if s.orelse.statements.len() > 0 {
                let val = statements(ctx, &s.orelse)?;
                if let Some(val) = val {
                    return Ok(Some(val));
                }
            }
            
        },

        Statement::While (s) => {

            while match expression::execute(ctx, block_functions, &s.test)?.to_bool() {
                Ok(val) => val,
                Err(e) => return Err(e.to_script_error(s.test.row(), s.test.column()))
            } {
                let val = statements(ctx, &s.body)?;
                if let Some(val) = val {
                    return Ok(Some(val));
                }
            }

            if s.orelse.statements.len() > 0 {
                let val = statements(ctx, &s.orelse)?;
                if let Some(val) = val {
                    return Ok(Some(val));
                }
            }
            
        },

    }

    Ok(None)
}