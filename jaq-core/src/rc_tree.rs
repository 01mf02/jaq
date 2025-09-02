use alloc::{boxed::Box, rc::Rc};

/// Root of a vector tree.
///
/// Unlike a `Vec`, a vector tree can be cloned in O(1).
/// Furthermore, unlike a linked list, elements can be looked up in O(log n).
/// However, adding elements also takes O(log n).
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
    /// Construct an empty tree.
    pub fn new() -> Self {
        Self(None)
    }

    /// Number of elements stored in the tree.
    ///
    /// Complexity: O(1)
    pub fn len(&self) -> usize {
        self.0.as_ref().map_or(0, |tree| tree.len())
    }

    /// Add an element to the tree.
    ///
    /// Complexity: O(log n)
    pub fn push(self, x: T) -> Self {
        match self.0 {
            None => Self(Some(Tree::Leaf(x).into())),
            Some(tree) => Self(Some(Tree::push(tree, x))),
        }
    }

    /// Get the `n`-th element added to the tree.
    ///
    /// Complexity: O(log n)
    pub fn get(&self, n: usize) -> Option<&T> {
        self.0.as_ref().and_then(|tree| tree.get(n))
    }

    /// Keep only the first `n` elements in the tree.
    ///
    /// Complexity: O(log n)
    pub fn truncate(self, n: usize) -> Self {
        match n {
            0 => Self(None),
            n => Self(self.0.map(|tree| Tree::truncate(tree, n))),
        }
    }

    /// Repeatedly add elements to the tree.
    pub fn extend(self, iter: impl IntoIterator<Item = T>) -> Self {
        iter.into_iter().fold(self, Self::push)
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.0.iter().flat_map(|tree| tree.iter())
    }
}

impl<T: Clone> Root<T> {
    /// Remove most recently added element from the tree.
    ///
    /// Complexity: O(log n)
    pub fn pop(self) -> Option<(T, Self)> {
        self.0.and_then(|tree| {
            let (x, tree_) = Tree::pop(tree);
            Some((x, Self(tree_)))
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Tree<T> {
    Node(usize, Rc<Self>, Rc<Self>),
    Leaf(T),
}

impl<T> Tree<T> {
    fn len(&self) -> usize {
        match self {
            Self::Leaf(_) => 1,
            Self::Node(len, ..) => *len,
        }
    }

    fn push(tree: Rc<Self>, x: T) -> Rc<Self> {
        let (len, l, r) = match &*tree {
            Self::Leaf(_) => (1, tree, Self::Leaf(x).into()),
            // tree is full
            Self::Node(len, ..) if len.is_power_of_two() => (*len, tree, Self::Leaf(x).into()),
            Self::Node(len, l, r) => (*len, l.clone(), Tree::push(r.clone(), x)),
        };
        Self::Node(len + 1, l, r).into()
    }

    fn get(&self, n: usize) -> Option<&T> {
        match self {
            Self::Leaf(leaf) => Some(leaf).filter(|_| n == 0),
            Self::Node(len, l, r) => n
                .checked_sub(len.next_power_of_two() / 2)
                .map_or_else(|| l.get(n), |n_| r.get(n_)),
        }
    }

    fn truncate(tree: Rc<Self>, n: usize) -> Rc<Self> {
        debug_assert!(n > 0);
        let cut = |l: &Rc<Self>, r: &Rc<Self>, n_| match n_ {
            0 => l.clone(),
            n_ => Rc::new(Self::Node(
                l.len() + core::cmp::min(r.len(), n_),
                l.clone(),
                Tree::truncate(r.clone(), n_),
            )),
        };
        match &*tree {
            Self::Leaf(_) => tree,
            Self::Node(len, l, r) => n
                .checked_sub(len.next_power_of_two() / 2)
                .map_or_else(|| Tree::truncate(l.clone(), n), |n_| cut(l, r, n_)),
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
    fn pop(tree: Rc<Self>) -> (T, Option<Rc<Self>>) {
        match &*tree {
            Self::Leaf(leaf) => (leaf.clone(), None),
            Self::Node(len, l, r) => match Tree::pop(r.clone()) {
                (x, None) => (x, Some(l.clone())),
                (x, Some(r_)) => (x, Some(Self::Node(len - 1, l.clone(), r_).into())),
            },
        }
    }
}

#[test]
fn test() {
    use alloc::{vec, vec::Vec};
    let eq = |l: &Root<_>, a| assert_eq!(l.iter().cloned().collect::<Vec<_>>(), a);

    let l = Root::new().push(0).push(1).push(2);
    eq(&l, vec![0, 1, 2]);

    eq(&l.clone().truncate(4), vec![0, 1, 2]);
    eq(&l.clone().truncate(3), vec![0, 1, 2]);
    eq(&l.clone().truncate(2), vec![0, 1]);
    eq(&l.clone().truncate(1), vec![0]);
    eq(&l.clone().truncate(0), vec![]);

    assert_eq!(l.get(0), Some(&0));
    assert_eq!(l.get(1), Some(&1));
    assert_eq!(l.get(2), Some(&2));
    assert_eq!(l.get(3), None);
}
