use alloc::{boxed::Box, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub struct Root<T>(Option<Rc<Tree<T>>>);

impl<T> Default for Root<T> {
    fn default() -> Self {
        Self(None)
    }
}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Root<T> {
    pub fn new() -> Self {
        Self(None)
    }

    pub fn size(&self) -> usize {
        self.0.as_ref().map_or(0, |tree| tree.size())
    }

    pub fn insert(self, x: T) -> Self {
        match self.0 {
            None => Self(Some(Tree::Leaf(x).into())),
            Some(tree) => Self(Some(Tree::insert(tree, x))),
        }
    }

    pub fn lookup(&self, n: usize) -> Option<&T> {
        self.0.as_ref().and_then(|tree| tree.lookup(n))
    }

    /// Repeatedly add elements to the list.
    pub fn extend(self, iter: impl IntoIterator<Item = T>) -> Self {
        iter.into_iter().fold(self, Self::insert)
    }
}

impl<T: Clone> Root<T> {
    pub fn pop(self) -> Option<(T, Self)> {
        self.0.and_then(|tree| match Tree::remove(tree) {
            (x, None) => Some((x, Self::default())),
            (x, Some(tree_)) => Some((x, Self(Some(tree_)))),
        })
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
enum Tree<T> {
    Node(usize, Rc<Self>, Rc<Self>),
    Leaf(T),
}

impl<T> Tree<T> {
    fn size(&self) -> usize {
        match self {
            Self::Leaf(_) => 1,
            Self::Node(size, ..) => *size,
        }
    }

    fn insert(tree: Rc<Self>, x: T) -> Rc<Self> {
        let (size, l, r) = match &*tree {
            Self::Leaf(leaf) => (1, tree, Self::Leaf(x).into()),
            // tree is full
            Self::Node(size, ..) if size.is_power_of_two() => (*size, tree, Self::Leaf(x).into()),
            Self::Node(size, l, r) => (*size, l.clone(), Tree::insert(r.clone(), x)),
        };
        Self::Node(size + 1, l, r).into()
    }

    fn lookup(&self, n: usize) -> Option<&T> {
        match self {
            Self::Leaf(leaf) => Some(leaf).filter(|_| n == 0),
            Self::Node(size, l, r) => n
                .checked_sub(size.next_power_of_two() / 2)
                .map_or_else(|| l.lookup(n), |n_| r.lookup(n_)),
        }
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &T> + '_> {
        match self {
            Self::Leaf(leaf) => Box::new(core::iter::once(leaf)),
            Self::Node(_, l, r) => Box::new(l.iter().chain(r.iter())),
        }
    }
}

impl<T: Clone> Tree<T> {
    fn remove(tree: Rc<Self>) -> (T, Option<Rc<Self>>) {
        match &*tree {
            Self::Leaf(leaf) => (leaf.clone(), None),
            Self::Node(size, l, r) => match Tree::remove(r.clone()) {
                (x, None) => (x, Some(l.clone())),
                (x, Some(r_)) => (x, Some(r_)),
            }
        }
    }
}
