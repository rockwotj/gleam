use crate::cplusplus::error::Error;
use crate::pretty::*;
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
    accessed_parent_vars: im::HashMap<String, Arc<Type>>,
    parent: Option<Box<LexicalScope>>,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalVariable<'a> {
    pub name: Document<'a>,
    pub typ: Arc<Type>,
}

impl<'a> LocalVariable<'a> {
    fn new<'b>(name: &'a str, v: &'b DeclaredVariable) -> LocalVariable<'a> {
        let name_doc = if v.count == 0 {
            name.to_doc()
        } else {
            Document::String(format!("{}${}", name, v.count))
        };
        LocalVariable {
            name: name_doc,
            typ: v.typ.clone(),
        }
    }
}

// TODO: This is likely not needed anymore :)
impl LexicalScope {
    pub fn new_root() -> Self {
        LexicalScope {
            current_scope_vars: im::HashMap::new(),
            accessed_parent_vars: im::HashMap::new(),
            parent: None,
        }
    }
    pub fn into_child<'child>(self) -> Self {
        LexicalScope {
            current_scope_vars: im::HashMap::new(),
            accessed_parent_vars: im::HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }
    pub fn into_parent(self) -> Option<Self> {
        self.parent.map(|p| *p)
    }

    pub fn local_var<'a>(&mut self, name: &'a str) -> Result<LocalVariable<'a>, Error> {
        match self.current_scope_vars.get(name) {
            None => match self.parent.as_mut() {
                None => Err(Error::InternalError {
                    message: format!("Missing variable in scope: {}", name),
                }),
                Some(parent) => {
                    let result = parent.local_var(name);
                    if let Ok(LocalVariable { typ, .. }) = &result {
                        let _ = self
                            .accessed_parent_vars
                            .insert(name.to_string(), typ.clone());
                    }
                    result
                }
            },
            Some(v) => Ok(LocalVariable::new(name, v)),
        }
    }

    pub fn declare_local_var<'a>(
        &mut self,
        name: &'a str,
        typ: &'a Arc<Type>,
    ) -> LocalVariable<'a> {
        let next = self.current_scope_vars.get(name).map_or(0, |v| v.count + 1);
        let _ = self.current_scope_vars.insert(
            name.to_string(),
            DeclaredVariable {
                count: next,
                typ: typ.clone(),
            },
        );
        // Cannot fail because we just insert into the map.
        self.local_var(name).unwrap()
    }
}
