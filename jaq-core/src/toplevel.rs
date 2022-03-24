use crate::functions::FUNCTIONS;
use crate::preprocess::{OpenFilter, PreFilter, UndefinedError};
use alloc::string::{String, ToString};
use alloc::{collections::BTreeMap, vec::Vec};

pub struct Definition {
    pub name: String,
    pub args: Vec<String>,
    pub term: PreFilter,
}

pub struct Definitions(Vec<Definition>);

impl Definitions {
    pub fn new(defs: Vec<Definition>) -> Self {
        Self(defs)
    }
}

pub struct Main {
    pub defs: Definitions,
    pub term: PreFilter,
}

impl Main {
    pub fn open(self, module: Definitions) -> Result<OpenFilter, UndefinedError> {
        let filter = self.term;
        let mut fns: BTreeMap<(String, usize), _> = FUNCTIONS
            .iter()
            .map(|(name, args, f)| ((name.to_string(), *args), f.clone().into()))
            .collect();
        for def in module.0.into_iter().chain(self.defs.0.into_iter()) {
            let open = def.term.open(&def.args, &|fun| fns.get(fun).cloned());
            fns.insert((def.name, def.args.len()), open?);
        }
        filter.open(&[], &|fun| fns.get(fun).cloned())
    }
}
