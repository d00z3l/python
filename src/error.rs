use std::{error::Error, fmt::{self, Display}, sync::Arc};
use rustpython_parser::error::{ParseErrorType};
use smol_str::SmolStr;

pub type NativeResult<T, E> = Result<T, ScriptErrorKind<E>>; 

pub trait NativeError : Error + Display + Clone {} 

#[derive(Debug, Clone)]
pub struct ScriptError<E> where E : NativeError {
    row: usize,
    column: usize,
    error: ScriptErrorKind<E>
}

impl<E> ScriptError<E> where E : NativeError {

    pub fn new(row: usize, column: usize, error: ScriptErrorKind<E>) -> ScriptError<E> {
        ScriptError{
            row,
            column,
            error
        }
    }

    pub fn err<T>(row: usize, column: usize, error: ScriptErrorKind<E>) -> Result<T, ScriptError<E>> {
        Err(
            ScriptError{
                row,
                column,
                error
            }
        )
    }

    pub fn overflow(row: usize, column: usize, level: usize) -> ScriptError<E> {
        ScriptError{
            row,
            column,
            error: ScriptErrorKind::StackOverflow{ level }
        }
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn error(&self) -> &ScriptErrorKind<E> {
        &self.error
    }

}

impl<E> fmt::Display for ScriptError<E> where E : NativeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at row: {} and column: {}", self.error.to_string(), self.row, self.column)
    }
}

impl<E> Error for ScriptError<E> where E : NativeError {}

#[derive(Debug, Clone)]
pub enum RequiredKind {
    Identifier,
    Constant,
    Iterator,
    Type,
    Return,
}

#[derive(Debug, Clone)]
pub enum NotFoundKind {
    Type,
    Function
}

#[derive(Debug, Clone)]
pub enum UnsupportedKind {
    Operator,
    Statement,
    Expression,
    Constant,
    Argument
}

#[derive(Debug, Clone)]
pub enum ScriptErrorKind<E> where E : NativeError {
    ExpressionNone,
    ReturnNotDefined,
    Required { kind: RequiredKind },
    FunctionKindInvalid { expected: SmolStr, provided: SmolStr },
    NotFound { kind: NotFoundKind, name: SmolStr },
    Unsupported { kind: UnsupportedKind, content: SmolStr },
    OperatorInvalid { operation: SmolStr, left: SmolStr, right: SmolStr },
    ParseError(Arc<ParseErrorType>),
    VariableNotFound { name: SmolStr },
    NameMismatch { expected: SmolStr, provided: SmolStr },
    TypeMismatch (TypeMismatch),
    ReturnMismatch (TypeMismatch),
    VariableArgsNotLast { name: SmolStr },
    UnknownType { name: SmolStr },
    StackOverflow { level: usize },
    InitialisationRequired { name: SmolStr },
    ArgumentConstantRequired { name: SmolStr },
    ArgumentRequired { name: SmolStr },
    ArgumentDuplicate { name: SmolStr },
    ArgumentsExpected { min: usize, max: Option<usize>, provided: usize},
    Native(E)
}

impl<E> ScriptErrorKind<E> where E : NativeError { 

    pub fn new_arguments_expected(min: usize, max: Option<usize>, provided: usize) -> Self {
        ScriptErrorKind::ArgumentsExpected { min, max, provided }
    }

    pub fn to_script_error(self, row: usize, column: usize) -> ScriptError<E> {
        ScriptError::new(row, column, self)
    }

}

impl<E> Display for ScriptErrorKind<E> where E : NativeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone)]
pub struct TypeMismatch {
    expected: SmolStr, 
    provided: SmolStr
}

impl TypeMismatch {

    pub fn new<S: AsRef<str>>(expected: S, provided: S) -> Self {
        Self {
            expected: expected.into(),
            provided: provided.into()
        }
    }

    pub fn expected(&self) -> &SmolStr {
        &self.expected
    }

    pub fn provided(&self) -> &SmolStr {
        &self.provided
    }

    pub fn err<T, E, S: AsRef<str>>(row: usize, column: usize, expected: S, provided: S) -> Result<T, ScriptError<E>> where E : NativeError {
        ScriptError::err(
            row,
            column,
            ScriptErrorKind::TypeMismatch(Self {
                expected: expected.into(),
                provided: provided.into()
            }
        ))
    }

    pub fn to_script_error<E>(self, row: usize, column: usize) -> ScriptError<E> where E : NativeError {
        ScriptError::new(row, column, ScriptErrorKind::TypeMismatch(self))
    }

}

impl<E> From<TypeMismatch> for ScriptErrorKind<E> where E : NativeError {
    fn from(error: TypeMismatch) -> Self {
        ScriptErrorKind::TypeMismatch(error)
    }
}

impl fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected: {}, provided: {}", self.expected, self.provided)
    }
}

impl fmt::Debug for TypeMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected: {}, provided: {}", self.expected, self.provided)
    }
}
