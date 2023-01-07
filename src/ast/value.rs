use std::{fmt::Display, sync::Arc};
use parking_lot::Mutex;
use smol_str::SmolStr;
use crate::{error::{TypeMismatch}, NativeType};
use super::StrBoxed;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<T> where T : NativeType<T> {
    None,
    Bool(bool),
    Float(f64),
    Int(i64),
    String(StrBoxed),
    Pair(Box<Pair<T>>),
    Dict(Box<Dict<T>>),
    List(Box<List<T>>),
    Native(T),
}

impl<T> Value<T> where T : NativeType<T> {

    pub fn is_bool(&self) -> bool {
        if let Value::Bool(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_float(&self) -> bool {
        if let Value::Float(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_int(&self) -> bool {
        if let Value::Int(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_string(&self) -> bool {
        if let Value::String(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_dict(&self) -> bool {
        if let Value::Dict(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_list(&self) -> bool {
        if let Value::List(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_native(&self) -> bool {
        if let Value::Native(_) = self {
            return true;
        }
        return false;
    }

    pub fn to_int(&self) -> Result<i64, TypeMismatch> {
        match self {
            Value::None => {
                Ok(0) 
            }
            Value::Bool(val) => {
                if *val {
                    return Ok(1);
                }
                Ok(0)
            },
            Value::Float(val) => {
                Ok(*val as i64)
            },
            Value::Int(val) => {
                Ok(*val)
            }
            _ => {
                return Err(
                    TypeMismatch::new("int", self.type_name())
                );
            }
        }
    }

    pub fn to_bool(&self) -> Result<bool, TypeMismatch> {
        match self {
            Value::None => {
                Ok(false) 
            }
            Value::Bool(val) => {
                if *val {
                    return Ok(true);
                }
                Ok(false)
            },
            Value::Float(val) => {
                Ok(*val != 0.0)
            },
            Value::Int(val) => {
                Ok(*val != 0)
            }
            _ => {
                return Err(
                    TypeMismatch::new("int", self.type_name())
                );
            }
        }
    }

    pub fn to_float(&self) -> Result<f64, TypeMismatch> {
        match self {
            Value::None => {
                Ok(0.0) 
            }
            Value::Bool(val) => {
                if *val {
                    return Ok(1.0);
                }
                Ok(0.0)
            },
            Value::Float(val) => {
                Ok(*val)
            },
            Value::Int(val) => {
                Ok(*val as f64)
            }
            _ => {
                return Err(
                    TypeMismatch::new("float", self.type_name())
                );
            }
        }
    }

    pub fn to_native(&self) -> Result<&T, TypeMismatch> {
        match self {
            Value::Native(val) => {
                Ok(val) 
            }
            _ => {
                return Err(
                    TypeMismatch::new("native", self.type_name())
                );
            }
        }
    }

    pub fn to_str_boxed(&self) -> StrBoxed {
        match self {
            Value::None => {
                "".into()
            }
            Value::Bool(val) => {
                if *val {
                    return "True".into();
                }
                "False".into()
            },
            Value::Float(val) => {
                lexical::to_string(*val).into()
            },
            Value::Int(val) => {
                lexical::to_string(*val).into()
            }
            Value::String(val) => {
                val.clone()
            }
            _ => {
                self.to_string().into()
            }
            
        }
    }

    pub fn to_smol_str(&self) -> SmolStr {
        self.to_str_boxed().into()
    }

    pub fn iter(&self) -> ValueIterator<T> {
        ValueIterator{
            list: self,
            index: 0
        }
    }

}

impl<T> From<f64> for Value<T> where T : NativeType<T> {
    fn from(val: f64) -> Self {
        Value::Float(val)
    }
}

impl<T> From<&f64> for Value<T> where T : NativeType<T> {
    fn from(val: &f64) -> Self {
        Value::Float(*val)
    }
}

impl<T> From<i64> for Value<T> where T : NativeType<T> {
    fn from(val: i64) -> Self {
        Value::Int(val)
    }
}

impl<T> From<u16> for Value<T> where T : NativeType<T> {
    fn from(val: u16) -> Self {
        Value::Int(val.into())
    }
}

impl<T> From<i32> for Value<T> where T : NativeType<T> {
    fn from(val: i32) -> Self {
        Value::Int(val.into())
    }
}

impl<T> From<u32> for Value<T> where T : NativeType<T> {
    fn from(val: u32) -> Self {
        Value::Int(val.into())
    }
}

impl<T> From<&i64> for Value<T> where T : NativeType<T> {
    fn from(val: &i64) -> Self {
        Value::Int(*val)
    }
}

impl<T> From<SmolStr> for Value<T> where T : NativeType<T> {
    fn from(val: SmolStr) -> Self {
        Value::String(val.into())
    }
}

impl<T> From<&str> for Value<T> where T : NativeType<T> {
    fn from(val: &str) -> Self {
        Value::String(val.into())
    }
}

#[derive(Debug)]
pub struct ValueIterator<'a, T> where T : NativeType<T> {
    list: &'a Value<T>,
    index: usize
}

impl<'a, T> Iterator for ValueIterator<'a, T> where T : NativeType<T> {
    type Item = Value<T>;
    fn next(&mut self) -> Option<Self::Item> {
        match &self.list {
            Value::String(str) => {
                if let Some(char) = str.chars().nth(self.index) {
                    self.index += 1;
                    return Some(Value::String(char.to_string().into()));
                }
            }
            Value::Dict(dict) => {
                if let Some(val) = dict.get_at(self.index) {
                    self.index += 1;
                    return Some(Value::Pair(Box::new(val.clone())));
                }
            }
            Value::List(list) => {
                if let Some(val) = list.get(self.index) {
                    self.index += 1;
                    return Some(val.clone());
                }
            }
            Value::Native(val) => {
                if let Some(val) = val.get_at(self.index) {
                    self.index += 1;
                    return Some(Value::Native(val.clone()));
                }
            }
            _ => {}
        }
        None
    }
}


impl<T> Default for Value<T> where T : NativeType<T> {
    fn default() -> Self {
        Value::None
    }
}

impl<T> Display for Value<T> where T : NativeType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => {
                f.write_str("")
            },
            Value::Bool(val) => {
                if *val {
                    f.write_str("true")
                } else {
                    f.write_str("false")
                }
            },
            Value::String(val) => {
                f.write_str(&val)
            },
            Value::Float(val) => {
                f.write_str(&lexical::to_string(*val))
            },
            Value::Int(val) => {
                f.write_str(&lexical::to_string(*val))
            },
            Value::Pair(val) => {
                f.write_fmt(format_args!("{}:{:?}", val.name(), val.value()))
            },
            Value::Dict(val) => {
                f.write_fmt(format_args!("{:?}", val))
            },
            Value::List(val) => {
                f.write_fmt(format_args!("{:?}", val))
            },
            Value::Native(val) => {
                f.write_str(&val.to_string())
            },
        }
    }
}

impl<T> NativeType<Value<T>> for Value<T> where T : NativeType<T> {

    fn type_name(&self) -> &str {
        match self {
            Value::None => {
                "none"
            },
            Value::Bool(_) => {
                "bool"
            },
            Value::String(_) => {
                "string"
            },
            Value::Float(_) => {
                "float"
            },
            Value::Int(_) => {
                "int"
            },
            Value::Pair(_) => {
                "pair"
            },
            Value::Dict(_) => {
                "dict"
            },
            Value::List(_) => {
                "list"
            },
            Value::Native(val) => {
                val.type_name()
            },
        }
    }

    fn get_at(&self, index: usize) -> Option<Value<T>> {
        match &self {
            Value::String(str) => {
                if let Some(char) = str.chars().nth(index) {
                    return Some(Value::String(char.to_string().into()));
                }
            }
            Value::Dict(dict) => {
                if let Some(val) = dict.get_at(index) {
                    return Some(Value::Pair(Box::new(val.clone())));
                }
            }
            Value::List(list) => {
                if let Some(val) = list.get(index) {
                    return Some(val.clone());
                }
            }
            Value::Native(val) => {
                if let Some(val) = val.get_at(index) {
                    return Some(Value::Native(val.clone()));
                }
            }
            _ => {}
        }
        None
    }

    fn is_iterable(&self) -> bool {
        match &self {
            Value::String(_) => {
                return true;
            }
            Value::Dict(_) => {
                return true;
            }
            Value::List(_) => {
                return true;
            }
            Value::Native(val) => {
                return val.is_iterable();
            }
            _ => {}
        }
        false
    }

    fn new(type_name: &str) -> Option<Value<T>> {
        match type_name {
            "bool" => {
                return Some(Value::Bool(false));
            }
            "int" => {
                return Some(Value::Int(0));
            }
            "float" => {
                return Some(Value::Float(0.0));
            }
            "string" => {
                return Some(Value::String("".into()));
            }
            _ => {}
        }

        if let Some(val) = T::new(type_name) {
            // Create a new native type
            return Some(Value::Native(val));
        }

        // The type wasn't found so return None
        None
    }

    fn is_type(type_name: &str) -> bool {
        if let Some(_) = Self::new(type_name) {
            return true;
        }
        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pair<T> where T : NativeType<T> {
    pub (crate) name: SmolStr,
    pub (crate) value: Value<T>
}

impl<T> Pair<T> where T : NativeType<T> {
    
    pub fn new(name: SmolStr, value: Value<T>) -> Self {
        Self {
            name,
            value
        }
    }
    
    pub fn name(&self) -> &SmolStr {
        &self.name
    }

    pub fn value(&self) -> &Value<T> {
        &self.value
    }

}

impl<T> Default for Pair<T> where T : NativeType<T> {
    fn default() -> Self {
        Self { name: SmolStr::default(), value: Value::None }
    }
}

#[derive(Debug, Clone)]
pub struct Dict<T> where T : NativeType<T> {
    values: Arc<Mutex<Vec<Pair<T>>>>
}

impl<T> Dict<T> where T : NativeType<T> {

    pub fn get(&self, name: &str) -> Option<Value<T>> {
        let values = self.values.lock();
        for item in values.iter() {
            if item.name == name {
                return Some(item.value.clone());
            }
        }
        None
    }

    pub fn get_at(&self, index: usize) -> Option<Pair<T>> {
        let values = self.values.lock();
        if let Some(val) = values.get(index) {
            return Some(val.clone());
        }
        None
    }

    pub fn set(&self, name: SmolStr, value: Value<T>) {
        
        let mut values = self.values.lock();
        for item in values.iter_mut() {
            if item.name == name {
               item.value = value;
               return;
            }
        }

        values.push(Pair::new(name, value));
    }

}

impl<T> PartialEq for Dict<T> where T : NativeType<T> {
    fn eq(&self, other: &Self) -> bool {
        *self.values.lock() == *other.values.lock()
    }
}

#[derive(Debug, Clone)]
pub struct List<T> where T : NativeType<T> {
    values: Arc<Mutex<Vec<Value<T>>>>
}

impl<T> List<T> where T : NativeType<T> {

    pub fn get(&self, index: usize) -> Option<Value<T>> {
        let values = self.values.lock();
        if let Some(val) = values.get(index) {
            return Some(val.clone());
        }
        None
    }

    pub fn push(&self, value: Value<T>) {
        let mut values = self.values.lock();
        values.push(value);
    }

    pub fn insert(&self, index: usize, value: Value<T>) {
        let mut values = self.values.lock();
        values.insert(index, value);
    }

    pub fn remove(&self, index: usize) {
        let mut values = self.values.lock();
        values.remove(index);
    }

}

impl<T> PartialEq for List<T> where T : NativeType<T> {
    fn eq(&self, other: &Self) -> bool {
        *self.values.lock() == *other.values.lock()
    }
}

#[cfg(test)]
mod tests {
    
    use std::mem;
    use crate::Empty;
    use super::*;

    #[test]
    fn test_size() {
        let size = mem::size_of::<Value<Empty>>();
        assert_eq!(size, 16);
    }

}