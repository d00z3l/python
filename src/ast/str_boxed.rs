use std::borrow::Borrow;
use smol_str::SmolStr;
use crate::{Value, NativeType};


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrBoxed (Box<SmolStr>);

impl StrBoxed {

    pub fn new<T>(text: T) -> Self where T: AsRef<str>,
    {
        Self(Box::new(text.into()))
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn to_string(&self) -> String {
        self.as_str().to_string()
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn to_smolstr(&self) -> SmolStr {
        (*self.0).clone()
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn as_smolstr(&self) -> &SmolStr {
        &(*self.0)
    }

}

impl From<&str> for StrBoxed {
    fn from(text: &str) -> Self {
        Self(Box::new(text.into()))
    }
}

impl From<&String> for StrBoxed {
    fn from(text: &String) -> Self {
        Self(Box::new(text.into()))
    }
}

impl From<String> for StrBoxed {
    fn from(text: String) -> Self {
        Self(Box::new(text.into()))
    }
}

impl From<SmolStr> for StrBoxed {
    fn from(text: SmolStr) -> Self {
        Self(Box::new(text))
    }
}

impl From<&SmolStr> for StrBoxed {
    fn from(text: &SmolStr) -> Self {
        Self(Box::new(text.clone()))
    }
}

impl From<StrBoxed> for SmolStr {
    fn from(text: StrBoxed) -> Self {
        *text.0
    }
}

impl From<&StrBoxed> for SmolStr {
    fn from(text: &StrBoxed) -> Self {
        text.0.as_ref().clone()
    }
}

impl<T> From<&StrBoxed> for Value<T> where T : NativeType<T>{
    fn from(text: &StrBoxed) -> Self {
        Value::String(text.clone())
    }
}

impl<T> From<StrBoxed> for Value<T> where T : NativeType<T>{
    fn from(text: StrBoxed) -> Self {
        Value::String(text)
    }
}

impl Borrow<str> for StrBoxed {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl std::ops::Deref for StrBoxed {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl Default for StrBoxed {
    fn default() -> Self {
        Self(Default::default())
    }
}