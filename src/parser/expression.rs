use num_bigint::Sign;
use rustpython_parser::{ast::{Located, ExprKind, Constant, Expr, Keyword, Cmpop, Unaryop}};
use smol_str::SmolStr;
use crate::{ast::
    {Value, Expression, BinOp, BinOpType, Position, Comparison, Parameter, UnaryOp, CallNativeExp, 
        UnaryExp, BinExp, ConcatenateExp, ComparisonExp, PrintExp, FunctionStat, CallExp}, 
    CompiledValue, 
    execute::{self, unary_op}, ScriptResult, error::{ScriptError, NativeError, ScriptErrorKind, 
        TypeMismatch, RequiredKind, NotFoundKind, UnsupportedKind}, compiler::CompileContext, 
        NativeType, NativeCompiler, NativeFunctionType, CallCompileContext, NativeFunctionReturnType
};

pub (super) fn expression<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], exp: &Located<ExprKind>, target: &mut Position<Expression<T>>, inline_constants: bool) -> ScriptResult<CompiledValue<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    
    if ctx.options.debug_mode {
        println!("{}, {}: Expression '{}'", exp.location.row(), exp.location.column(), &exp.node.name());
    }
    
    let location = exp.location;
    match &exp.node {

        ExprKind::Constant { value, kind: _ } => {

            return constant(ctx, exp, target, value);

        }

        ExprKind::Name { id: name, ctx: _ } => {

            if let Some((id, val)) = ctx.compiler.get(name.as_str()) {
                *target = Position::new(location.row(), location.column(), Expression::Variable(true, id, name.into()));
                if ctx.options.debug_mode {
                    println!("{}, {}: Get variable {} with value {}", location.row(), location.column(), name, val);
                }
                return Ok(val);
            }

            let id = ctx.variable(&name);

            // Check to see if a variable exists first
            if let Some(val) = ctx.env.local_get(id) {
                *target = Position::new(location.row(), location.column(), Expression::Variable(false, id, name.into()));
                if ctx.options.debug_mode {
                    println!("{}, {}: Get variable {} with value {}", location.row(), location.column(), name, val);
                }
                return Ok(val);
            }



            return ScriptError::err(
                location.row(), 
                location.column(),
                ScriptErrorKind::VariableNotFound { name: name.into() }
            );
        }

        ExprKind::UnaryOp { op, operand } => {

            let mut exp = Position::new(location.row(), location.column(), Expression::None); 
            let val = expression(ctx, block_functions, operand, &mut exp, inline_constants)?;

            let op = Position::new(location.row(), location.column(), match op {
                Unaryop::Invert => UnaryOp::Invert,
                Unaryop::Not => UnaryOp::Not,
                Unaryop::UAdd => UnaryOp::Add,
                Unaryop::USub => UnaryOp::Sub,
            });

            if inline_constants && val.is_constant() {

                let value = unary_op::execute(&op, val.value().clone())?;
                *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
                return Ok(CompiledValue::new(value, true));

            } else {

                *target = Position::new(location.row(), location.column(), Expression::UnaryOp (
                    UnaryExp::new(op, exp)
                ));

            }

            return Ok(val);
            
        }

        ExprKind::BinOp { left, op, right} => {

            return bin_op(ctx, block_functions, exp, target, &left, op, &right, inline_constants);

        }

        ExprKind::Call { func, args, keywords} =>  {

            return call(ctx, block_functions, target, func, args, keywords, inline_constants);

        }

        ExprKind::Compare {left, ops, comparators} => {

            return compare(ctx, block_functions, target, left, ops, comparators, inline_constants);   

        }

        _ => {
            return ScriptError::err(
                exp.location.row(), 
                exp.location.column(),
                ScriptErrorKind::Unsupported { kind: UnsupportedKind::Expression, content: format!("{:?}", exp.node).into() }
            );
        }
    }
    
}

pub (super) fn to_name<E>(exp: &Located<ExprKind>) -> ScriptResult<&String, E> where E : NativeError {
    let name = match &exp.node {
        ExprKind::Name { id, ctx: _ } => {
            id
        }
        _ => {
            return ScriptError::err(
                exp.location.row(), 
                exp.location.column(),
                ScriptErrorKind::Required { kind: RequiredKind::Identifier }
            );
        }
    };
    Ok(name)
}

fn constant<C, F, T, E>(_ctx: &mut CompileContext<C, F, T, E>, exp: &Located<ExprKind>, target: &mut Position<Expression<T>>, value: &Constant) -> ScriptResult<CompiledValue<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    let location = exp.location;
    match value {
        Constant::None => {
            let value = Value::None;
            *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
            return Ok(CompiledValue::new(value, true));
        },
        Constant::Bool(val) => {
            let value = Value::Bool(*val);
            *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
            return Ok(CompiledValue::new(value, true));
        },
        Constant::Str(val) => {
            let value = Value::String(val.into());
            *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
            return Ok(CompiledValue::new(value, true));
        },
        Constant::Int(val) => {
            let (sign, digits) = val.to_u64_digits();
            let mut val = 0;
            if digits.len() > 0 {
                val = digits[0] as i64;
                if sign == Sign::Minus {
                    val = val * -1;
                }
            }
            let value = Value::Int(val);
            *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
            return Ok(CompiledValue::new(value, true));
        },
        Constant::Float(val) => {
            let value = Value::Float(*val);
            *target = Position::new(location.row(), location.column(), Expression::Constant(value.clone()));
            return Ok(CompiledValue::new(value, true));
        },
        _ => {
            return ScriptError::err(
                location.row(), 
                location.column(),
                ScriptErrorKind::Unsupported { kind: UnsupportedKind::Constant, content: format!("{:?}", exp.node).into() }
            );
        }
    }
}

fn bin_op<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], exp: &Located<ExprKind>, target: &mut Position<Expression<T>>, left: &Box<Expr>, op: &rustpython_parser::ast::Operator, right: &Box<Expr>, inline_constants: bool) -> ScriptResult<CompiledValue<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{
    
    let location = &exp.location.clone();
    
    let mut left_exp = Position::new(location.row(), location.column(), Expression::None); 
    let left_val = expression(ctx, block_functions, left, &mut left_exp, inline_constants)?;
    if ctx.options.debug_mode {
        println!("{}, {}: BinOp left value {}", location.row(), location.column(), left_val);
    }
    
    let mut right_exp = Position::new(location.row(), location.column(), Expression::None); 
    let right_val = expression(ctx, block_functions, right, &mut right_exp, inline_constants)?;
    if ctx.options.debug_mode {
        println!("{}, {}: BinOp right value {}", location.row(), location.column(), right_val);
    }

    if inline_constants && left_val.is_constant() {
        left_exp = Position::new(left_exp.row(), left_exp.column(), Expression::Constant(left_val.value().clone()));
    }
    if inline_constants && right_val.is_constant() {
        right_exp = Position::new(right_exp.row(), right_exp.column(), Expression::Constant(right_val.value().clone()));
    }

    let op = match op {
        rustpython_parser::ast::Operator::Add => {
            BinOp::Add
        },
        rustpython_parser::ast::Operator::Sub => {
            BinOp::Sub
        },
        rustpython_parser::ast::Operator::Mult => {
            BinOp::Mult
        },
        rustpython_parser::ast::Operator::Div => {
            BinOp::Div
        },
        rustpython_parser::ast::Operator::Mod => {
            BinOp::Mod
        },
        rustpython_parser::ast::Operator::Pow => {
            BinOp::Pow
        },
        _ => {
            return ScriptError::err(
                location.row(), 
                location.column(),
                ScriptErrorKind::Unsupported { kind: UnsupportedKind::Operator, content: format!("{:?}", exp.node).into() }
            );
        }
    };

    // Check the two type to see what method we should use
    if (left_val.value().is_float() && (right_val.value().is_float() || right_val.value().is_int())) 
        || (right_val.value().is_float() && (left_val.value().is_float() || left_val.value().is_int())) {
        
        // If either left or right is a float we use float op and return float
        if inline_constants && left_val.is_constant() && right_val.is_constant() {
            
            // We can calculate the value now
            let val = execute::bin_op::float(left_val.value(), op, right_val.value()).map_err(|x|x.to_script_error(location.row(), location.column()))?;
            *target = Position::new(left_exp.row(), left_exp.column(), Expression::Constant(Value::Float(val)));
            return Ok(CompiledValue::new(Value::Float(val), true));

        }

        *target = Position::new(left_exp.row(), left_exp.column(), Expression::BinOp (
                BinExp::new(
                    BinOpType::Float, 
                    left_exp, 
                    op, 
                    right_exp
                )
            )
        );
        return Ok(CompiledValue::new(Value::Float(0.0), false)); 

    } else if left_val.value().is_int() && right_val.value().is_int() {
        // Both are ints so use integer op
        
        if inline_constants && left_val.is_constant() && right_val.is_constant() {
            
            // We can calculate the value now
            let val = execute::bin_op::int(left_val.value(), op, right_val.value()).map_err(|x|x.to_script_error(location.row(), location.column()))?;
            *target = Position::new(left_exp.row(), left_exp.column(), Expression::Constant(Value::Int(val)));
            return Ok(CompiledValue::new(Value::Int(val), true));

        }

        *target = Position::new(left_exp.row(), left_exp.column(), Expression::BinOp (
                BinExp::new(
                    BinOpType::Int, 
                    left_exp, 
                    op, 
                    right_exp
                )
            )
        );

        return Ok(CompiledValue::new(Value::Int(0), false)); 

    } else if op == BinOp::Add && left_val.value().is_string() && right_val.value().is_string() {
        // Add is used for concatenation of strings
        if inline_constants && left_val.is_constant() && right_val.is_constant() {
            
            // We can concatenate the strings now
            let mut val = left_val.to_string();
            val.push_str(&right_val.to_string());
            let val = Value::String(val.into());
            *target = Position::new(left_exp.row(), left_exp.column(), Expression::Constant(val.clone()));
            return Ok(CompiledValue::new(val, true));

        }

        *target = Position::new(left_exp.row(), left_exp.column(), Expression::Concatenate (ConcatenateExp::new(left_exp, right_exp)));
        return Ok(CompiledValue::new(Value::String(Default::default()), false)); 

    }

    return ScriptError::err(
        location.row(), 
        location.column(),
        ScriptErrorKind::OperatorInvalid { operation: format!("{:?}", op).into(), left: left_val.type_name().into(), right: right_val.type_name().into() }
    );

}

fn call<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], target: &mut Position<Expression<T>>, func: &Expr, args: &Vec<Expr>, keywords: &Vec<Keyword>, inline_constants: bool) -> ScriptResult<CompiledValue<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    // First get the name of the function
    let name : Position<SmolStr> = if let ExprKind::Name{ id, ctx: _} = &func.node {
        Position::new(func.location.row(), func.location.column(), id.into())
    } else {
        return ScriptError::err(
            func.location.row(), 
            func.location.column(),
            ScriptErrorKind::Required { kind: RequiredKind::Constant }
        );
    };

    if let Some(val) = builtin(ctx, block_functions, target, name.clone(), args, keywords, inline_constants)? {
        // A built-in function was found
        return Ok(val);
    }

    if let Some(val) = call_native(ctx, block_functions, target, name.clone(), args, keywords, inline_constants)? {
        // A native function was found
        return Ok(val);
    }

    if let Some(val) = call_def(ctx, block_functions, target, name.clone(), args, keywords, inline_constants)? {
        // A native function was found
        return Ok(val);
    }

    return ScriptError::err(
        func.location.row(), 
        func.location.column(),
        ScriptErrorKind::NotFound { kind: NotFoundKind::Function, name: name.value().to_string().into() }
    );

}

fn builtin<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], target: &mut Position<Expression<T>>, name: Position<SmolStr>, args: &Vec<Expr>, _keywords: &Vec<Keyword>, inline_constants: bool) -> ScriptResult<Option<CompiledValue<T>>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    // Check the built in functions
    match &**name {
        "print" => {

            let mut parameters = Vec::new();
            for arg in args {

                let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
                let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;

                if inline_constants && value.is_constant() {
                    parameters.push(Parameter::Constant(value.value.to_smol_str()));
                } else {
                    parameters.push(Parameter::Expression(Box::new(exp)));
                }
            }

            if parameters.len() == 0 {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentsExpected { min:1, max: None, provided: parameters.len() }
                );
            }

            *target = Position::new(name.row(), name.column(), Expression::Print (
                PrintExp::new(
                    parameters[0].clone(), 
                    parameters[1..].to_vec()
                )
            ));

            return Ok(Some(CompiledValue::new(Value::None, false)));

        }
        "float" => {

            if args.len() != 1 {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentsExpected { min:1, max: Some(1), provided: args.len() }
                );
            }

            let arg = &args[0];
            let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
            let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;

            if let Value::String(val) = value.value() {
                
                // Need to manual convert as we don't support implicit conversion
                let float : f64 = match lexical::parse(val.as_str()) {
                    Ok(val) => val,
                    Err(_) => {
                        return TypeMismatch::err(arg.location.row(), arg.location.column(), "float", val.as_str());
                    }
                };

                if inline_constants && value.is_constant() {
                    *target = Position::new(target.row(), target.column(), Expression::Constant(Value::Float(float)));
                } else {
                    *target = Position::new(name.row(), name.column(), Expression::Int(Box::new(exp)));
                }
    
                return Ok(Some(CompiledValue::new(Value::Float(float), inline_constants && value.is_constant())));

            }

            let float = match value.value().to_float() {
                Ok(val) => val,
                Err(e) => return Err(e.to_script_error(arg.location.row(), arg.location.column())),
            };

            if inline_constants && value.is_constant() {
                *target = Position::new(target.row(), target.column(), Expression::Constant(Value::Float(float)));
            } else {
                *target = Position::new(name.row(), name.column(), Expression::Float(Box::new(exp)));
            }

            return Ok(Some(CompiledValue::new(Value::Float(float), inline_constants && value.is_constant())));

        }
        "int" => {

            if args.len() != 1 {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentsExpected { min: 1, max: Some(1), provided: args.len() }
                );
            }

            let arg = &args[0];
            let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
            let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;

            if let Value::String(val) = value.value() {
                
                // Need to manual convert as we don't support implicit conversion
                let int : i64 = match lexical::parse(val.as_str()) {
                    Ok(val) => val,
                    Err(_) => {
                        return TypeMismatch::err(arg.location.row(), arg.location.column(), "int", val.as_str());
                    }
                };

                if inline_constants && value.is_constant() {
                    *target = Position::new(target.row(), target.column(), Expression::Constant(Value::Int(int)));
                } else {
                    *target = Position::new(name.row(), name.column(), Expression::Int(Box::new(exp)));
                }
    
                return Ok(Some(CompiledValue::new(Value::Int(int), inline_constants && value.is_constant())));

            }

            let int = match value.value().to_int() {
                Ok(val) => val,
                Err(e) => return Err(e.to_script_error(arg.location.row(), arg.location.column())),
            };

            if inline_constants && value.is_constant() {
                *target = Position::new(target.row(), target.column(), Expression::Constant(Value::Int(int)));
            } else {
                *target = Position::new(name.row(), name.column(), Expression::Int(Box::new(exp)));
            }

            return Ok(Some(CompiledValue::new(Value::Int(int), inline_constants && value.is_constant())));

        }
        "str" => {

            if args.len() != 1 {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentsExpected { min: 1, max: Some(1), provided: args.len() }
                );
            }

            let arg = &args[0];
            let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
            let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;

            let str = value.value().to_str_boxed();

            if inline_constants && value.is_constant() {
                *target = Position::new(target.row(), target.column(), Expression::Constant(str.clone().into()));
            } else {
                *target = Position::new(name.row(), name.column(), Expression::Str(Box::new(exp)));
            }

            return Ok(Some(CompiledValue::new(str.into(), inline_constants && value.is_constant())));

        }
        _ => {}
    }

    Ok(Option::None)

}

fn call_native<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], target: &mut Position<Expression<T>>, name: Position<SmolStr>, args: &Vec<Expr>, keywords: &Vec<Keyword>, inline_constants: bool) -> ScriptResult<Option<CompiledValue<T>>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    let id = ctx.native_call_id;
    ctx.native_call_id += 1;

    // Check to see if the function exists in the N
    let native_func = if let Some(func) = ctx.native_functions.get(name.value().as_str()) {
        func
    } else {
        return Ok(None);
    };

    // The arguments in the order that are provide in the call function
    let mut arguments = Vec::new();

    let mut has_args = false;
    if let Some(last) = &native_func.parameters.last() {
        has_args = last.options.args;
    }

    // Loop through the fixed position arguments
    for (i, arg) in args.iter().enumerate() {
        if !has_args {
            if i >= native_func.parameters.len() {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentsExpected { min: native_func.parameters.len(), max: Some(native_func.parameters.len()), provided: args.len() }
                );
            }
        }
        if has_args && i >= native_func.parameters.len() {
            arguments.push((native_func.parameters.last().unwrap(), arg));
        } else {
            arguments.push((&native_func.parameters[i], arg));
        }
    }

    // Now loop through the Keyword arguments
    for keyword in keywords.iter() {

        // Get the of the argument
        let name = if let Some(name) = &keyword.node.arg {
            name
        } else {
            return ScriptError::err(
                keyword.location.row(), 
                keyword.location.column(),
                ScriptErrorKind::Required { kind: RequiredKind::Identifier }
            );
        };

        // Does the argument already exist
        if let Some(_) = arguments.iter().position(|x| &x.0.name == &name) {
            return ScriptError::err(
                keyword.location.row(), 
                keyword.location.column(),
                ScriptErrorKind::ArgumentDuplicate { name: name.into() }
            );
        }

        // Find the parameter using the name
        if let Some(para) = native_func.parameters.iter().find(|x| &x.name == &name) {
            arguments.push((para, &keyword.node.value));
        } else {
            return ScriptError::err(
                keyword.location.row(), 
                keyword.location.column(),
                ScriptErrorKind::Unsupported { kind: UnsupportedKind::Argument, content: name.into() }
            );
        }
    }

    if !native_func.skip_validation() {

        // Check to see if there are any required parameters
        for para in native_func.parameters.iter().filter(|x|x.options.required == true) {
            if arguments.iter().filter(|x|&x.0.name == &para.name).count() == 0 {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ArgumentRequired { name: para.name.clone() }
                );
            }
        }

    }

    let mut argument_positions = Vec::new();
    let mut argument_expressions = Vec::new();
    let mut argument_values = Vec::new();

    for (i, (para, arg)) in arguments.iter().enumerate() {
        
        let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
        let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;

        if !native_func.skip_validation() {
            // Check to see if the return type match the parameter
            if !para.is_type(value.type_name()) {
                return ScriptError::err(
                    arg.location.row(), 
                    arg.location.column(),
                    TypeMismatch::new(para.types().as_str(), value.type_name().into()).into()
                );
            }
        }

        if inline_constants && value.is_constant() {
            argument_expressions.push(Position::new(arg.location.row(), arg.location.column(), Expression::Constant(value.value().clone())));
        } else {
            argument_expressions.push(exp);
        }
        argument_values.push(value.clone());
        argument_positions.push((para.name.clone(), i));

    }

    // Update the arguments
    ctx.env.argument_clear();
    for val in argument_values.iter() {
        ctx.env.argument_set(val.clone());
    }

    // Add a new level to the stack for downsteam calls
    ctx.env.locals_push().map_err(|level|ScriptError::overflow(name.row(), name.column(), level))?;

    let val = match ctx.compiler.compile(native_func.function_type(), CallCompileContext::new(id, native_func.name(), ctx.options), &mut ctx.env) {
        Ok(value) => value,
        Err(err) => {
            
            // Clear and pop
            ctx.env.locals_pop();

            return Err(err.to_script_error(name.row(), name.column()));
        }
    };

    // Clear and pop
    ctx.env.locals_pop();

    // Check the return type
    if let Some(val) = &val {
        if let NativeFunctionReturnType::Fixed(return_type) = &native_func.return_type {
            if return_type.type_name() != val.type_name() {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ReturnMismatch(TypeMismatch::new(return_type.type_name(), val.type_name()).into())
                );
            }
        }
    } else {
        if let NativeFunctionReturnType::Fixed(return_type) = &native_func.return_type {
            if return_type.type_name() != "none" {
                return ScriptError::err(
                    name.row(), 
                    name.column(),
                    ScriptErrorKind::ReturnMismatch(TypeMismatch::new(return_type.type_name(), "none").into())
                );
            }
        }
    }

    if let Some(val) = &val {
        if val.is_constant() {
            *target = Position::new(name.row(), name.column(), Expression::Constant(val.value().clone()));
            return Ok(Some(val.clone()));
        }
    }

    // Check to see if the function is used list
    let index = ctx.native_functions_in_use.iter().position(|x| &x.name == name.value());
    let index = if let Some(index) = index {
        index
    } else {
        let index = ctx.native_functions_in_use.len();
        ctx.native_functions_in_use.push(native_func.clone());
        index
    };

    *target = Position::new(name.row(), name.column(), Expression::CallNative(
        CallNativeExp::new(
            id,
            Position::new(name.row(), name.column(), native_func.name().clone()), 
            index, 
            argument_expressions, 
            argument_positions
        )
    ));

    Ok(val)

}

fn call_def<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], target: &mut Position<Expression<T>>, name: Position<SmolStr>, args: &Vec<Expr>, _keywords: &Vec<Keyword>, inline_constants: bool) -> ScriptResult<Option<CompiledValue<T>>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    // Check to see if the function exists in the N
    let mut func = None;
    for (i, f) in block_functions.iter().enumerate() {
        if &f.name == name.value() {
            func = Some((i, f.clone()));
            break;
        }
    }
    let (index, func) = if let Some(func) = func {
        func
    } else {
        return Ok(None);
    };

    if func.parameters.len() != args.len() {
        return ScriptError::err(
            name.row(), 
            name.column(),
            ScriptErrorKind::ArgumentsExpected { min: func.parameters.len(), max: Some(func.parameters.len()), provided: args.len()}
        );
    }

    let mut arg_exps = Vec::with_capacity(args.len());

    for (para, arg) in func.parameters.iter().zip(args) {

        let mut exp = Position::new(arg.location.row(), arg.location.column(), Expression::None); 
        let value = expression(ctx, block_functions, arg, &mut exp, inline_constants)?;
        
        if &value.type_name() != &para.type_name {
            return TypeMismatch::err(exp.row(), exp.column(), para.type_name.as_str(), value.type_name());
        }
        
        if value.is_constant() {
            arg_exps.push(Position::new(exp.row(), exp.column(), Expression::Constant(value.value().clone())));
        } else {
            arg_exps.push(exp);
        }

    }

    *target = Position::new(name.row(), name.column(), Expression::Call(CallExp::new(
        name,
        index,
        arg_exps
    )));
    
    if let Some(return_type) = &func.return_type {
        Ok(Some(CompiledValue::new(Value::new(return_type).unwrap(), false)))
    } else {
        Ok(Some(CompiledValue::new(Value::None, false)))
    }
    

}

fn compare<C, F, T, E>(ctx: &mut CompileContext<C, F, T, E>, block_functions: &[Position<FunctionStat<T>>], target: &mut Position<Expression<T>>, left: &Expr, ops: &[Cmpop], comparators: &[Expr], inline_constants: bool) -> ScriptResult<CompiledValue<T>, E> where C : NativeCompiler<T, F, E>, F : NativeFunctionType, T : NativeType<T>, E : NativeError{

    // https://docs.python.org/3/reference/expressions.html#comparisons
    // Comparisons can be chained arbitrarily, e.g., x < y <= z is equivalent to x < y and y <= z, 
    // except that y is evaluated only once (but in both cases z is not evaluated at all when x < y is found to be false).
    
    let mut left_exp = Position::new(left.location.row(), left.location.column(), Expression::None);
    let left_val = expression(ctx, block_functions, left, &mut left_exp, inline_constants)?;
    let mut last_exp = left_exp.clone();
    let mut last_val = left_val.clone();
    let mut other_exp = Vec::new();
    let mut is_constant = inline_constants && left_val.is_constant();
    let mut result = true;

    // Now loop through and check the other comparitors
    for (op, comp) in ops.iter().zip(comparators) {

        let mut next_exp = Position::new(comp.location.row(), comp.location.column(), Expression::None);
        let next_val = expression(ctx, block_functions, comp, &mut next_exp, inline_constants)?;

        if left_val.type_name() != next_val.type_name() {
            return TypeMismatch::err(next_exp.row(), next_exp.column(), left_val.type_name(), next_val.type_name());
        }

        let comp = match op {
            Cmpop::Eq => Comparison::Equal,
            Cmpop::NotEq => Comparison::NotEqual,
            Cmpop::Lt => Comparison::LessThan,
            Cmpop::LtE => Comparison::LessThanEqual,
            Cmpop::Gt => Comparison::GreaterThan,
            Cmpop::GtE => Comparison::GreaterThanEqual,
            Cmpop::In => Comparison::In,
            Cmpop::NotIn => Comparison::NotIn,
            Cmpop::Is | Cmpop::IsNot => {
                return ScriptError::err(
                    next_exp.row(), 
                    next_exp.column(),
                    ScriptErrorKind::Unsupported { kind: UnsupportedKind::Operator, content: "Is".into() }
                );
            }
        };

        let bool_value = execute::comp_op::any(last_exp.row(), last_exp.column(), last_val.value(), comp, next_val.value())?;

        if bool_value == false {
            // Everything needs to be true for the expression to be true
            result = false;
        }

        if is_constant && !next_val.is_constant() {
            is_constant = false;
        }

        last_val = next_val;
        last_exp = next_exp.clone();
        other_exp.push((comp, next_exp));

    }

    if is_constant {
        *target = Position::new(target.row(), target.column(), Expression::Constant(Value::Bool(result)));
    } else {
        *target = Position::new(target.row(), target.column(), Expression::Comparison(ComparisonExp::new(left_exp, other_exp)));
    }

    return Ok(CompiledValue::new(Value::Bool(result), is_constant));

}