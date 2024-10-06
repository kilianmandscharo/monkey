use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: None,
        }
    }
    pub fn new_enclosed(outer: Environment) -> Self {
        Self {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: Some(Box::new(outer)),
        }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        let map_borrow = self.store.borrow();
        let val = map_borrow.get(name);
        if let Some(val) = val {
            Some(val.clone())
        } else if let Some(outer) = &self.outer {
            outer.get(name)
        } else {
            None
        }
    }
    pub fn set(&self, name: &str, obj: Object) -> Object {
        let return_val = obj.clone();
        self.store.borrow_mut().insert(name.to_string(), obj);
        return_val
    }
}
