use std::num::NonZeroU32;
use std::ops::{Deref, DerefMut};

use kg_utils::collections::SparseSet;

pub const EMPTY_GOTO: u32 = ::std::u32::MAX;
pub const EMPTY_MATCH: u32 = ::std::u32::MAX;


#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Match(NonZeroU32);

impl Match {
    pub fn new(m: u32) -> Match {
        debug_assert_ne!(m, 0);
        Match(unsafe { NonZeroU32::new_unchecked(m) })
    }

    pub fn matching(&self) -> u32 {
        self.0.get()
    }
}

impl std::fmt::Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{}]", self.0.get())
    }
}


#[derive(Clone)]
pub struct ByteMask([bool; 256]);

#[allow(unused)]
impl ByteMask {
    pub fn empty() -> ByteMask {
        ByteMask([false; 256])
    }

    pub fn full() -> ByteMask {
        ByteMask([true; 256])
    }

    pub fn matches(&self, input: u8) -> bool {
        self.0[input as usize]
    }

    pub fn iter(&self) -> ByteMaskIter {
        ByteMaskIter {
            mask: self,
            b: 0,
        }
    }

    pub fn include(&mut self, input: u8) {
        self.0[input as usize] = true;
    }

    pub fn exclude(&mut self, input: u8) {
        self.0[input as usize] = false;
    }

    pub fn negate(&mut self) {
        for i in 0..256 {
            self.0[i] = !self.0[i];
        }
    }
}

impl PartialEq for ByteMask {
    fn eq(&self, other: &Self) -> bool {
        use libc::{c_void, memcmp};
        use std::mem::size_of;

        unsafe { memcmp(self.0.as_ptr() as *const c_void, other.0.as_ptr() as *const c_void, size_of::<ByteMask>()) == 0 }
    }
}

impl Eq for ByteMask { }

impl std::fmt::Debug for ByteMask {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl std::fmt::Display for ByteMask {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::fmt::Write;

        let mut s = String::with_capacity(256);
        for i in 0u8 ..= 255u8 {
            if self.matches(i) {
                write!(s, "{}", i as char)?;
            }
        }
        write!(f, "{:?}", s)
    }
}

pub struct ByteMaskIter<'a> {
    mask: &'a ByteMask,
    b: usize,
}

impl<'a> Iterator for ByteMaskIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        while self.b < 256 {
            let b = self.b;
            self.b += 1;
            if self.mask.0[b] {
                return Some(b as u8);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(256))
    }
}


#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Edge(u8, u32);

impl Edge {
    pub fn new(value: u8, state: u32) -> Edge {
        Edge(value, state)
    }

    pub fn value(&self) -> u8 {
        self.0
    }

    pub fn state(&self) -> u32 {
        self.1
    }
}

impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> {}", self.value() as char, self.state())
    }
}


#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Accept(u32, u32);

impl Accept {
    pub fn new(state: u32, matched: u32) -> Accept {
        Accept(state, matched)
    }

    pub fn empty() -> Accept {
        Accept(EMPTY_GOTO, EMPTY_MATCH)
    }

    pub fn is_empty(&self) -> bool {
        self.0 == EMPTY_GOTO
    }

    pub fn state(&self) -> u32 {
        self.0
    }

    pub fn matching(&self) -> u32 {
        self.1
    }
}

impl std::fmt::Display for Accept {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            write!(f, "accept [-]")
        } else {
            write!(f, "accept [{}]({})", self.matching(), self.state())
        }
    }
}


#[derive(Debug, Clone)]
pub struct StateEx {
    pub merged_states: SparseSet<u32>,
    pub edges: Vec<Edge>,
    pub accepts: Vec<Accept>,
}

impl StateEx {
    pub fn new(state_count: usize) -> StateEx {
        StateEx {
            merged_states: SparseSet::with_capacity(state_count),
            edges: Vec::new(),
            accepts: Vec::new(),
        }
    }

    pub fn add_accept(&mut self, accept: Accept) {
        if !self.accepts.contains(&accept) {
            self.accepts.push(accept);
        }
    }
}

impl PartialEq for StateEx {
    fn eq(&self, other: &Self) -> bool {
        self.merged_states == other.merged_states
    }
}

impl Eq for StateEx {}

impl std::fmt::Display for StateEx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.merged_states)?;
        for a in self.accepts.iter() {
            write!(f, " {}", a)?;
        }
        write!(f, "\n")?;
        for e in self.edges.iter() {
            write!(f, "        {}\n", e)?;
        }
        Ok(())
    }
}
