use crate::cplusplus::error::Error;
use crate::type_::Type;
use std::sync::Arc;

#[derive(Debug, Clone)]
struct DeclaredVariable {
    count: usize,
    typ: Arc<Type>,
}

#[derive(Debug, Clone)]
pub(crate) struct LexicalScope {
    current_scope_vars: im::HashMap<String, DeclaredVariable>,
    parent: Option<Box<LexicalScope>>,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalVariable {
    pub name: String,
    pub typ: Arc<Type>,
}

impl LocalVariable {
    fn new(name: String, v: &DeclaredVariable) -> LocalVariable {
        let name_doc = if v.count == 0 {
            name
        } else {
            format!("{}${}", name, v.count)
        };
        LocalVariable {
            name: name_doc,
            typ: v.typ.clone(),
        }
    }
}

impl LexicalScope {
    pub fn new_root() -> Self {
        LexicalScope {
            current_scope_vars: im::HashMap::new(),
            parent: None,
        }
    }
    pub fn into_child(self) -> Self {
        LexicalScope {
            current_scope_vars: im::HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }
    pub fn into_parent(self) -> Option<Self> {
        self.parent.map(|p| *p)
    }

    pub fn local_var(&mut self, name: String) -> Result<LocalVariable, Error> {
        match self.current_scope_vars.get(&name) {
            None => match self.parent.as_mut() {
                None => Err(Error::InternalError {
                    message: format!("Missing variable in scope: {}", name),
                }),
                Some(parent) => parent.local_var(name),
            },
            Some(v) => Ok(LocalVariable::new(name, v)),
        }
    }

    pub fn declare_local_var<'a>(
        &mut self,
        name: String,
        typ: &'a Arc<Type>,
    ) -> LocalVariable {
        let next = self.current_scope_vars.get(&name).map_or(0, |v| v.count + 1);
        let _ = self.current_scope_vars.insert(
            name.to_string(),
            DeclaredVariable {
                count: next,
                typ: typ.clone(),
            },
        );
        self.local_var(name)
            .expect("This cannot failure because we just inserted this into the map")
    }
}
