use crate::{Value, ast::Comparison, error::{ScriptError, NativeError, TypeMismatch, ScriptErrorKind}, ScriptResult, NativeType};

#[cfg_attr(not(debug_assertions), inline(always))]
pub (crate) fn any<T, E>(row: usize, column: usize, left: &Value<T>, op: Comparison, right: &Value<T>) -> ScriptResult<bool, E> where T : NativeType<T>, E : NativeError {
    let bool_value = match &left {
        Value::Bool(_) => bool(left, op, right),
        Value::Float(_) => float(left, op, right),
        Value::Int(_) => int(left, op, right),
        Value::String(_) => string(left, op, right),
        _ => {
            match op {
                Comparison::Equal => {
                    Ok(left == right)
                },
                Comparison::NotEqual => {
                    Ok(left != right)
                },
                _ => {
                    return ScriptError::err(
                        row, 
                        column,
                        ScriptErrorKind::OperatorInvalid { operation: format!("{:?}", op).into(), left: left.type_name().into(), right: right.type_name().into() }
                    );
                }
            }
        }
    };

    match bool_value {
        Ok(val) => Ok(val),
        Err(e) => {
            return Err(e.to_script_error(row, column));
        }
    }
}

#[cfg_attr(not(debug_assertions), inline(always))]
fn float<T>(left: &Value<T>, op: Comparison, right: &Value<T>) -> Result<bool, TypeMismatch> where T : NativeType<T> {

    let left = left.to_float()?;
    let right = right.to_float()?;

    let val = match op {
        Comparison::Equal => left == right,
        Comparison::NotEqual => left != right,
        Comparison::LessThan => left < right,
        Comparison::LessThanEqual => left <= right,
        Comparison::GreaterThan => left > right,
        Comparison::GreaterThanEqual => left >= right,
        _ => {
            // This shouldn't happen
            false
        }
    };

    Ok(val)

}

#[cfg_attr(not(debug_assertions), inline(always))]
fn int<T>(left: &Value<T>, op: Comparison, right: &Value<T>) -> Result<bool, TypeMismatch> where T : NativeType<T> {

    let left = left.to_int()?;
    let right = right.to_int()?;

    let val = match op {
        Comparison::Equal => left == right,
        Comparison::NotEqual => left != right,
        Comparison::LessThan => left < right,
        Comparison::LessThanEqual => left <= right,
        Comparison::GreaterThan => left > right,
        Comparison::GreaterThanEqual => left >= right,
        _ => {
            // This shouldn't happen
            false
        }
    };

    Ok(val)

}

#[cfg_attr(not(debug_assertions), inline(always))]
fn string<T>(left: &Value<T>, op: Comparison, right: &Value<T>) -> Result<bool, TypeMismatch> where T : NativeType<T> {

    let left = left.to_string();
    let right = right.to_string();

    let val = match op {
        Comparison::Equal => left == right,
        Comparison::NotEqual => left != right,
        Comparison::LessThan => left < right,
        Comparison::LessThanEqual => left <= right,
        Comparison::GreaterThan => left > right,
        Comparison::GreaterThanEqual => left >= right,
        Comparison::In => right.contains(&left),
        Comparison::NotIn => !right.contains(&left),
    };

    Ok(val)

}

#[cfg_attr(not(debug_assertions), inline(always))]
fn bool<T>(left: &Value<T>, op: Comparison, right: &Value<T>) -> Result<bool, TypeMismatch> where T : NativeType<T> {

    let left = left.to_bool()?;
    let right = right.to_bool()?;

    let val = match op {
        Comparison::Equal => left == right,
        Comparison::NotEqual => left != right,
        Comparison::LessThan => left < right,
        Comparison::LessThanEqual => left <= right,
        Comparison::GreaterThan => left > right,
        Comparison::GreaterThanEqual => left >= right,
        _ => {
            false
        }
    };

    Ok(val)

}