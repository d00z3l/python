use bit_reverse::BitwiseReverse;
use crate::{ast::{UnaryOp, Position}, Value, ScriptResult, error::{NativeError, TypeMismatch}, NativeType};

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn execute<T, E>(op: &Position<UnaryOp>, value: Value<T>) -> ScriptResult<Value<T>, E> where T : NativeType<T>, E : NativeError {

    match &**op {
        UnaryOp::Not => {

            if let Value::Bool(val) = value {
                // Need to use ! to reverse the value
                return Ok(Value::Bool(!val))  
            }

            return TypeMismatch::err(op.row(), op.column(), "bool", value.type_name());
            
        }

        UnaryOp::Add => {

            // For add we don't actually do anything
            return Ok(value);

        }

        UnaryOp::Sub => {

            match value {
                Value::Float(val) => {
                    // Reverse the sign
                    return Ok(Value::Float(-val));   
                }
                Value::Int(val) => {
                    // Reverse the sign
                    return Ok(Value::Int(-val));   
                }
                _ => {
                    return TypeMismatch::err(op.row(), op.column(), "number", value.type_name());
                }
            }

        }

        UnaryOp::Invert => {

            if let Value::Int(num) = value {

                return Ok(Value::Int(num.swap_bits()));

            } else {
                return TypeMismatch::err(op.row(), op.column(), "int", value.type_name());
            }

        }
    }
    

}