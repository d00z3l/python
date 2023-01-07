use crate::{Value, ast::BinOp, NativeType, error::TypeMismatch};

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn float<T>(left: &Value<T>, op: BinOp, right: &Value<T>) -> Result<f64, TypeMismatch> where T : NativeType<T> {

    let left = left.to_float()?;
    let right = right.to_float()?;

    let val = match op {
        BinOp::Add => {
            left + right
        }
        BinOp::Sub => {
            left - right
        }
        BinOp::Mult => {
            left * right
        }
        BinOp::Div => {
            if right == 0.0 {
                0.0
            } else {
                left / right
            }
        }
        BinOp::Mod => {
            left % right
        }
        BinOp::Pow => {
            left.powf(right)
        }
    };

    Ok(val)

}

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn int<T>(left: &Value<T>, op: BinOp, right: &Value<T>) -> Result<i64, TypeMismatch> where T : NativeType<T> {

    let left = left.to_int()?;
    let right = right.to_int()?;

    let val = match op {
        BinOp::Add => {
            left + right
        }
        BinOp::Sub => {
            left - right
        }
        BinOp::Mult => {
            left * right
        }
        BinOp::Div => {
            if right == 0 {
                0
            } else {
                left / right
            }
        }
        BinOp::Mod => {
            left % right
        }
        BinOp::Pow => {
            left.pow(right as u32)
        }
    };

    Ok(val)

}