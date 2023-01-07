mod value;
mod str_boxed;

use smol_str::SmolStr;
use crate::{error::{TypeMismatch}, compiler::CompiledConstant, NativeType};
use std::ops::Deref;

pub use {value::*, str_boxed::*};

#[derive(Debug, Clone, PartialEq)]
pub struct Position<T> where T : Clone + PartialEq {
    row: usize,
    column: usize,
    value: T
}

impl<T> Position<T> where T : Clone + PartialEq {

    pub fn new(row: usize, column: usize, value: T) -> Self {
        Self {
            row,
            column,
            value
        }
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn value(&self) -> &T {
        &self.value
    }

}

impl<T> Deref for Position<T> where T : Clone + PartialEq {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInType {
    Int,
    Float,
    String,
}

impl BuiltInType {

    pub fn _name(&self) -> &str {
        match self {
            BuiltInType::Int => "int",
            BuiltInType::Float => "float",
            BuiltInType::String => "string",
        }
    }

    pub fn from(type_name: &str) -> Option<BuiltInType> {
        match type_name {
            "int" => {
                Some(BuiltInType::Int)
            }
            "float" => {
                Some(BuiltInType::Float)
            }
            "str" => {
                Some(BuiltInType::String)
            }
            _ => {
                None
            }
        }
    }

    pub fn to_value<T>(&self, val: &Value<T>) -> Result<Value<T>, TypeMismatch> where T : NativeType<T> {
        match self {
            BuiltInType::Int => Ok(Value::Int(val.to_int()?)),
            BuiltInType::Float => Ok(Value::Float(val.to_float()?)),
            BuiltInType::String => Ok(Value::String(val.to_string().into())),
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum OneOrMany<T> where T : NativeType<T> {
    One(Position<Statement<T>>),
    /// bool indiates whether the statement is the last line
    Many(Vec<(bool, Position<Statement<T>>)>)
}

/// CompiledBlock prevents use of an iterator if there is only one statement
#[derive(Debug, Clone, PartialEq)]
pub struct CompiledBlock<T> where T : NativeType<T> {
    pub (crate) statements: OneOrMany<T>,
    pub (crate) functions: Vec<Position<FunctionStat<T>>>,
    pub (crate) constants: Vec<CompiledConstant<T>>
}

impl<T> From<Block<T>> for CompiledBlock<T> where T : NativeType<T> {
    fn from(mut val: Block<T>) -> Self {
        if val.statements.len() == 0 || val.statements.len() > 1 {
            let mut statements = Vec::with_capacity(val.statements.len());
            for (i, stat) in val.statements.iter().enumerate(){
                if i == val.statements.len() - 1 {
                    statements.push((true, stat.clone()));
                } else {
                    statements.push((false, stat.clone()));
                }
            }
            CompiledBlock{ 
                statements: OneOrMany::Many(statements),
                functions: val.functions,
                constants: val.constants
            }
        } else {
            CompiledBlock{ 
                functions: val.functions,
                constants: val.constants,
                statements: OneOrMany::One(val.statements.remove(0)),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<T> where T : NativeType<T> {
    pub (crate) statements: Vec<Position<Statement<T>>>,
    pub (crate) functions: Vec<Position<FunctionStat<T>>>,
    pub (crate) constants: Vec<CompiledConstant<T>>
}

impl<T> Block<T> where T : NativeType<T> {

    pub (crate) fn new() -> Self {
        Self {
            statements: vec![],
            functions: vec![],
            constants: vec![]
        }
    }

    #[cfg(test)]
    pub (crate) fn with_statement(statement: Position<Statement<T>>, constants: Vec<CompiledConstant<T>>) -> Self {
        Self {
            statements: vec![statement],
            functions: vec![],
            constants
        }
    }

    #[cfg(test)]
    pub (crate) fn with_statements(statements: Vec<Position<Statement<T>>>, functions: Vec<Position<FunctionStat<T>>>, constants: Vec<CompiledConstant<T>>) -> Self {
        Self {
            statements,
            functions,
            constants
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStat<T> where T : NativeType<T> {
    pub (crate) id: u16,
    pub (crate) name: SmolStr,
    pub (crate) value: Position<Expression<T>>,
}

impl<T> AssignStat<T> where T : NativeType<T> {

    pub fn new(id: u16, name: SmolStr, value: Position<Expression<T>>) -> Box<Self> {
        Box::new(Self {
            id,
            name,
            value
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStat<T> where T : NativeType<T> {
    pub (crate) condition: Position<Expression<T>>,
    pub (crate) body: Block<T>,
    pub (crate) orelse: Block<T>,
}

impl<T> IfStat<T> where T : NativeType<T> {

    pub fn new(condition: Position<Expression<T>>, body: Block<T>, orelse: Block<T>) -> Box<Self> {
        Box::new(Self {
            condition,
            body,
            orelse
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ForRangeStat<T> where T : NativeType<T> {
    pub (crate) var_id: u16,
    pub (crate) var_name: SmolStr,
    pub (crate) start: Parameter<i64, T>,
    pub (crate) stop: Parameter<i64, T>,
    pub (crate) step: Parameter<i64, T>,
    pub (crate) body: Block<T>,
    pub (crate) orelse: Block<T>,
}

impl<T> ForRangeStat<T> where T : NativeType<T> {

    pub fn new(var_id: u16, var_name: SmolStr, start: Parameter<i64, T>, stop: Parameter<i64, T>, step: Parameter<i64, T>, body: Block<T>, orelse: Block<T>,) -> Box<Self> {
        Box::new(Self {
            var_id,
            var_name,
            start,
            stop,
            step,
            body,
            orelse
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInStat<T> where T : NativeType<T> {
    pub (crate) var_id: u16,
    pub (crate) var_name: SmolStr,
    pub (crate) iter: Position<Expression<T>>,
    pub (crate) body: Block<T>,
    pub (crate) orelse: Block<T>,
}

impl<T> ForInStat<T> where T : NativeType<T> {

    pub fn new(var_id: u16, var_name: SmolStr, iter: Position<Expression<T>>, body: Block<T>, orelse: Block<T>) -> Box<Self> {
        Box::new(Self {
            var_id,
            var_name,
            iter,
            body,
            orelse,
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStat<T> where T : NativeType<T> {
    pub (crate) test: Position<Expression<T>>,
    pub (crate) body: Block<T>,
    pub (crate) orelse: Block<T>,
}

impl<T> WhileStat<T> where T : NativeType<T> {

    pub fn new(test: Position<Expression<T>>, body: Block<T>, orelse: Block<T>) -> Box<Self> {
        Box::new(Self {
            test,
            body,
            orelse
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub (crate) id: u16,
    pub (crate) name: SmolStr,
    pub (crate) type_name: SmolStr,
}

impl FunctionParameter {

    pub fn new(id: u16, name: SmolStr, type_name: SmolStr) -> Self {
        Self {
            id,
            name,
            type_name
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStat<T> where T : NativeType<T> {
    pub (crate) name: SmolStr,
    pub (crate) parameters: Vec<FunctionParameter>,
    pub (crate) return_type: Option<SmolStr>,
    pub (crate) body: Block<T>,
}

impl<T> FunctionStat<T> where T : NativeType<T> {

    pub fn new(name: SmolStr, parameters: Vec<FunctionParameter>, return_type: Option<SmolStr>, body: Block<T>) -> Self {
        Self {
            name,
            parameters,
            return_type,
            body
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStat<T> where T : NativeType<T> {
    pub (crate) return_type: SmolStr,
    pub (crate) exp: Position<Expression<T>>,
}

impl<T> ReturnStat<T> where T : NativeType<T> {

    pub fn new(return_type: SmolStr, exp: Position<Expression<T>>) -> Box<Self> {
        Box::new(Self {
            return_type,
            exp
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T> where T : NativeType<T> {

    Return (Option<Box<ReturnStat<T>>>),

    Assign (Box<AssignStat<T>>),

    Expression (Position<Expression<T>>),

    If (Box<IfStat<T>>),

    ForRange (Box<ForRangeStat<T>>),

    ForIn (Box<ForInStat<T>>),

    While (Box<WhileStat<T>>),

}

impl<T> Statement<T> where T : NativeType<T> { 

    pub fn name(&self) -> &str {
        match self {
            Statement::Return(_) => "return",
            Statement::Assign(_) => "assign",
            Statement::Expression(_) => "expression",
            Statement::If(_) => "if",
            Statement::ForRange(_) => "forrange",
            Statement::ForIn(_) => "forin",
            Statement::While(_) => "while",
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter<V, T> where T : NativeType<T> {
    Constant(V),
    Expression(Box<Position<Expression<T>>>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallNativeExp<T> where T : NativeType<T> {
    pub (crate) id: usize,
    pub (crate) name: Position<SmolStr>,
    pub (crate) index: usize,
    pub (crate) argument_expressions: Vec<Position<Expression<T>>>,
    pub (crate) argument_positions: Vec<(SmolStr, usize)>
}

impl<T> CallNativeExp<T> where T : NativeType<T> {

    pub fn new(id: usize, name: Position<SmolStr>, index: usize, argument_expressions: Vec<Position<Expression<T>>>, argument_positions: Vec<(SmolStr, usize)>) -> Box<Self> {
        Box::new(Self {
            id,
            name,
            index,
            argument_expressions,
            argument_positions
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExp<T> where T : NativeType<T> {
    pub (crate) name: Position<SmolStr>,
    pub (crate) index: usize,
    pub (crate) argument_expressions: Vec<Position<Expression<T>>>,
}

impl<T> CallExp<T> where T : NativeType<T> {

    pub fn new(name: Position<SmolStr>, index: usize, argument_expressions: Vec<Position<Expression<T>>>) -> Box<Self> {
        Box::new(Self {
            name,
            index,
            argument_expressions
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExp<T> where T : NativeType<T> {
    pub (crate) op: Position<UnaryOp>,
    pub (crate) exp: Position<Expression<T>>,
}

impl<T> UnaryExp<T> where T : NativeType<T> {

    pub fn new(op: Position<UnaryOp>, exp: Position<Expression<T>>) -> Box<Self> {
        Box::new(Self {
            op,
            exp
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct BinExp<T> where T : NativeType<T> {
    pub (crate) op_type: BinOpType,
    pub (crate) left: Position<Expression<T>>,
    pub (crate) op: BinOp,
    pub (crate) right: Position<Expression<T>>,
}

impl<T> BinExp<T> where T : NativeType<T> {

    pub fn new(op_type: BinOpType, left: Position<Expression<T>>, op: BinOp, right: Position<Expression<T>>) -> Box<Self> {
        Box::new(Self {
            op_type,
            left,
            op,
            right
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ConcatenateExp<T> where T : NativeType<T> {
    pub (crate) left: Position<Expression<T>>,
    pub (crate) right: Position<Expression<T>>,
}

impl<T> ConcatenateExp<T> where T : NativeType<T> {

    pub fn new(left: Position<Expression<T>>, right: Position<Expression<T>>) -> Box<Self> {
        Box::new(Self {
            left,
            right
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExp<T> where T : NativeType<T> {
    pub (crate) left: Position<Expression<T>>,
    pub (crate) others: Vec<(Comparison, Position<Expression<T>>)>
}

impl<T> ComparisonExp<T> where T : NativeType<T> {

    pub fn new(left: Position<Expression<T>>, others: Vec<(Comparison, Position<Expression<T>>)>) -> Box<Self> {
        Box::new(Self {
            left,
            others
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintExp<T> where T : NativeType<T> {
    pub (crate) first: Parameter<SmolStr, T>,
    pub (crate) others: Vec<Parameter<SmolStr, T>>
}

impl<T> PrintExp<T> where T : NativeType<T> {

    pub fn new(first: Parameter<SmolStr, T>, others: Vec<Parameter<SmolStr, T>>) -> Box<Self> {
        Box::new(Self {
            first,
            others
        })
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<T> where T : NativeType<T> {

    None,
    
    Constant (Value<T>),

    Variable (bool, u16, SmolStr),

    UnaryOp (Box<UnaryExp<T>>),

    BinOp (Box<BinExp<T>>),

    Concatenate (Box<ConcatenateExp<T>>),

    Comparison (Box<ComparisonExp<T>>),

    Call (Box<CallExp<T>>),

    CallNative (Box<CallNativeExp<T>>),

    Print(Box<PrintExp<T>>),

    Float (Box<Position<Expression<T>>>),

    Str (Box<Position<Expression<T>>>),

    Int (Box<Position<Expression<T>>>)

}

impl<T> Expression<T> where T : NativeType<T> {

    pub fn name(&self) -> &str {
        match self {
            Expression::None => "none",
            Expression::Constant(_) => "constant",
            Expression::Variable(_, _, _) => "variable",
            Expression::UnaryOp(_) => "unaryop",
            Expression::BinOp(_) => "binOp",
            Expression::Concatenate(_) => "concatenate",
            Expression::Comparison(_) => "comparison",
            Expression::Call(_) => "call",
            Expression::CallNative(_) => "callnative",
            Expression::Print(_) => "print",
            Expression::Float(_) => "float",
            Expression::Str(_) => "str",
            Expression::Int(_) => "int",
        }
    }

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpType {
    Float,
    Int
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Invert, // ~
    Add,    // +
    Sub,    // -
    Not     // not
}

/// An operator for a binary operation (an operation with two operands).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow
}

/// An operator for a binary operation (an operation with two operands).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Comparison {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    In,
    NotIn,
}