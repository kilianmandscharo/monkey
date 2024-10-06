use crate::object::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        let val = self.store.get(name);
        val.cloned()
    }
    pub fn set(&mut self, name: &str, obj: Object) -> Object {
        let return_val = obj.clone();
        self.store.insert(name.to_string(), obj);
        return_val
    }
}
