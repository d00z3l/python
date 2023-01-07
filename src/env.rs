use smol_str::SmolStr;
use crate::{ast::Position, NativeType};



#[derive(Debug, Clone)]
pub struct Env<T> where T : NativeType<T> {
    locals: Vec<VariableStack<T>>,
    stack_level: usize,
    native_level: usize,
    pub (crate) native_arguments: Vec<Vec<T>>,
    pub (crate) print_output: Vec<Position<SmolStr>>
}

impl<T> Env<T> where T : NativeType<T> {

    pub fn new() -> Self {
        Env {
            locals: vec![VariableStack::with_capacity(10), VariableStack::with_capacity(5)],
            stack_level: 0,
            native_level: 0,
            native_arguments: vec![Vec::with_capacity(3), Vec::with_capacity(3)],
            print_output: vec![]
        }
    }

    pub fn reset(&mut self) {
        self.print_output.clear();
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn local_get(&self, id: u16) -> Option<T> {
            
        // If the level is greater than zero check the function level variables.
        if let Some(val) = self.locals.get(self.stack_level) {
            match val.get(id) {
                Some(val) => {
                    return Some(val.clone())
                }
                None => {
                    // Nothing found so continue on
                }
            }
        }

        None
        
    }

    pub fn local_set(&mut self, id: u16, value: T) {
        self.locals[self.stack_level].set(id, value);
    }

    pub fn locals_push(&mut self) -> Result<(), usize> {
        self.stack_check()?;
        self.stack_level += 1;
        if self.locals.len() <= self.stack_level {
            self.locals.push(VariableStack::with_capacity(5));
        }
        Ok(())
    }

    pub fn locals_pop(&mut self) {
        if self.stack_level < self.locals.len() {
            // We don't remove the stack but clear it instead
            // This means we don't have to reallocate memory
            self.locals[self.stack_level].clear();
        }
        if self.stack_level > 0 {
            self.stack_level -= 1;
        }
    }

    pub fn argument_push(&mut self) {
        self.native_level += 1;
        if self.native_arguments.len() < self.native_level {
            self.native_arguments.push(Vec::with_capacity(3));
        }
    }

    pub fn argument_pop(&mut self) {
        // We don't remove the arguments but clear it instead
        // This means we don't have to reallocate memory
        self.native_arguments[self.native_level].clear();
        if self.native_level > 0 {
            self.native_level -= 1;
        }
    }

    pub fn argument_clear(&mut self) {
           
        // If the level is greater than zero check the function level variables.
        if let Some(stack) = self.native_arguments.get_mut(self.native_level) {
            stack.clear();
        }
        
    }

    pub fn argument_set(&mut self, value: T) -> Option<&T> {
           
        // If the level is greater than zero check the function level variables.
        if let Some(stack) = self.native_arguments.get_mut(self.native_level) {
            stack.push(value);
        }

        None
        
    }

    pub fn argument(&self, index: usize) -> Option<&T> {
            
        // If the level is greater than zero check the function level variables.
        if let Some(stack) = self.native_arguments.get(self.native_level) {
            match stack.get(index) {
                Some(val) => {
                    return Some(val)
                }
                None => {
                    // Nothing found so continue on
                }
            }
        }

        None
        
    }

    pub fn argument_len(&self) -> usize {
        if let Some(stack) = self.native_arguments.get(self.native_level) {
            return stack.len()
        }
        0
    }

    pub fn print_output(&self) -> &[Position<SmolStr>] {
        &self.print_output
    }

    /// Checks to see if the stack has been exhausted and then throws an error
    fn stack_check(&mut self) -> Result<(), usize> {
        
        if self.stack_level >= 50 {
            return Err(self.stack_level);
        }
        
        Ok(())
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<T> where T : NativeType<T> {
    pub (crate) id: u16,
    pub (crate) value: T
}

impl<T> Variable<T> where T : NativeType<T> {
    pub fn new(id: u16, value: T) -> Self {
        Self {
            id,
            value
        }
    }
}

impl<T> Default for Variable<T> where T : NativeType<T> {
    fn default() -> Self {
        Self { id: Default::default(), value: Default::default() }
    }
}

/// Used to store values on the stack for each block
#[derive(Debug, Clone, PartialEq)]
pub struct VariableStack<T> where T : NativeType<T> {
    values: Vec<Variable<T>>
}

impl<T> VariableStack<T> where T : NativeType<T> {

    pub (crate) fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity)
        }
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn get(&self, id: u16) -> Option<&T> {
        if let Some(item) = self.values.iter().find(|x|x.id == id) {
            return Some(&item.value);
        }
        None
    }

    pub fn set(&mut self, id: u16, value: T) {

        for item in self.values.iter_mut() {
            if item.id == id {
                item.value = value;
                return;
            }
        }

        self.values.push(Variable::new(id, value));

    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    /*
    pub fn iter(&self) -> VariableStackIter<T> {
        VariableStackIter{
            index: 0,
            container: self
        }
    }
    */

}

pub struct VariableStackIter<'a, T> where T : NativeType<T> {
    index: usize,
    container: &'a VariableStack<T>
}

impl<'a, T> Iterator for VariableStackIter<'a, T> where T : NativeType<T> {
    type Item = &'a Variable<T>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(val) = self.container.values.get(self.index) {
            self.index += 1;
            return Some(val);
        }
        return None;
    }
}

#[cfg(test)]
mod tests {

    use std::mem;
    use crate::{ast::{Value, Pair}, Empty};
    use super::*;
    
    #[test]
    fn test_sizes() {
        let size = mem::size_of::<Value<Empty>>();
        assert_eq!(size, 16);
        let size = mem::size_of::<Pair<Empty>>();
        assert_eq!(size, 40);
        let size = mem::size_of::<VariableStack<Empty>>();
        assert_eq!(size, 24);
        let size = mem::size_of::<Env<Empty>>();
        assert_eq!(size, 88);       
    }
    
}