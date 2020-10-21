use std::fmt;
use std::cmp;
use std::cmp::Ordering;
use std::option::Option::*;
use std::char;

mod parse;

pub use self::parse::Error;

#[derive(Debug, Clone, Copy)]
struct Char {
    value: char,
}

impl Char {
    fn new(c: char) -> Char {
        Char {
            value: c
        }
    }

    fn prev(c: char) -> char {
        unsafe {
            char::from_u32_unchecked(c as u32 - 1)
        }
    }

    fn next(c: char) -> char {
        unsafe {
            char::from_u32_unchecked(c as u32 + 1)
        }
    }
}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.value < ' ' {
            match self.value {
                '\0' => write!(f, "\\0"),
                '\t' => write!(f, "\\t"),
                '\r' => write!(f, "\\r"),
                '\n' => write!(f, "\\n"),
                _ => write!(f, "\\x{0:02x}", self.value as u32),
            }
        } else if self.value <= '\u{007F}' {
            if f.alternate() {
                // in literals
                match self.value {
                    '\\' | '.' | '+' | '*' | '?' | '|' | '[' | ']' | '(' | ')' | '{' | '}' => write!(f, "\\{}", self.value),
                    _ => write!(f, "{}", self.value)
                }
            } else {
                // in ranges
                match self.value {
                    '\\' | '-' | '[' | ']' | '^' => write!(f, "\\{}", self.value),
                    _ => write!(f, "{}", self.value)
                }
            }
        } else if self.value.is_alphabetic() {
            write!(f, "{}", self.value)
        } else {
            write!(f, "\\u{0:X}", self.value as u32)
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CharRange {
    from: char,
    to: char,
}

impl CharRange {
    pub fn from_char(c: char) -> CharRange {
        CharRange { from: c, to: c }
    }

    pub fn from_range(from: char, to: char) -> CharRange {
        CharRange {
            from: cmp::min(from, to),
            to: cmp::max(from, to),
        }
    }

    pub fn contains(&self, r: CharRange) -> bool {
        self.from <= r.from && self.to >= r.to
    }

    pub fn overlaps(&self, r: CharRange) -> bool {
        self.from <= r.to && self.to >= r.from
    }

    pub fn adjacent(&self, r: CharRange) -> bool {
        self.from as u32 == r.to as u32 + 1 || self.to as u32 + 1 == r.from as u32
    }

    pub fn overlaps_or_adjacent(&self, r: CharRange) -> bool {
        self.from as u32 <= r.to as u32 + 1 && self.to as u32 + 1 >= r.from as u32
    }

    pub fn cardinality(&self) -> u32 {
        self.to as u32 - self.from as u32 + 1
    }

    pub fn chars(&self) -> CharRangeIter {
        CharRangeIter {
            range: self,
            curr: self.from as u32,
        }
    }

    pub fn is_ascii_range(&self) -> bool {
        self.to <= '\x7F'
    }

    pub fn is_byte_range(&self) -> bool {
        self.to as u32 <= 255
    }

    pub fn from(&self) -> char {
        self.from
    }

    pub fn to(&self) -> char {
        self.to
    }
}

impl fmt::Display for CharRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.to as u32 - self.from as u32 {
            0 => write!(f, "{}", Char::new(self.from)),
            1 => write!(f, "{}{}", Char::new(self.from), Char::new(self.to)),
            _ => write!(f, "{}-{}", Char::new(self.from), Char::new(self.to)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CharRangeIter<'a> {
    range: &'a CharRange,
    curr: u32,
}

impl<'a> Iterator for CharRangeIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c: Option<Self::Item>;
        if self.curr <= self.range.to as u32 {
            unsafe {
                c = Some(char::from_u32_unchecked(self.curr));
            }
            self.curr += 1;
        } else {
            c = None;
        }
        return c;
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharSet {
    ranges: Vec<CharRange>,
    cardinality: u32,
}

impl CharSet {
    fn recalc_cardinality(&mut self) {
        let mut c = 0;
        for r in self.ranges.iter() {
            c += r.cardinality();
        }
        self.cardinality = c;
    }

    pub fn new() -> CharSet {
        CharSet {
            ranges: Vec::new(),
            cardinality: 0,
        }
    }

    pub fn add(&mut self, r: CharRange) {
        if self.ranges.is_empty() {
            self.ranges.push(r);
            self.cardinality = r.cardinality();
        } else {
            match self.ranges.binary_search_by(|&range| {
                if range.overlaps_or_adjacent(r) {
                    Ordering::Equal
                } else if range > r {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }) {
                Ok(index) => {
                    let mut range = self.ranges[index];
                    if range != r {
                        if r.from < range.from && index > 0 {
                            let mut r0 = self.ranges[index - 1];
                            if r0.adjacent(r) {
                                r0.to = cmp::max(range.to, r.to);
                                self.ranges[index - 1] = r0;
                                self.ranges.remove(index);
                                self.recalc_cardinality();
                                return;
                            }
                        }
                        if r.to > range.to && index < self.ranges.len() - 1 {
                            let mut r1 = self.ranges[index + 1];
                            if r1.adjacent(r) {
                                r1.from = cmp::min(range.from, r.from);
                                self.ranges[index] = r1;
                                self.ranges.remove(index + 1);
                                self.recalc_cardinality();
                                return;
                            }
                        }
                        range.from = cmp::min(range.from, r.from);
                        range.to = cmp::max(range.to, r.to);
                        self.ranges[index] = range;
                        self.recalc_cardinality();
                    }
                }
                Err(index) => {
                    self.ranges.insert(index, r);
                    self.cardinality += r.cardinality();
                }
            }
        }
    }

    pub fn add_char(&mut self, c: char) {
        self.add(CharRange::from_char(c));
    }

    pub fn add_range(&mut self, from: char, to: char) {
        self.add(CharRange::from_range(from, to));
    }

    pub fn remove(&mut self, r: CharRange) {
        if self.ranges.is_empty() {
            if r.from > '\0' {
                self.ranges.push(CharRange::from_range('\0', Char::prev(r.from)));
            }
            if r.to < '\u{10FFFF}' {
                self.ranges.push(CharRange::from_range(Char::next(r.to), '\u{10FFFF}'));
            }
            self.recalc_cardinality();
        } else {
            let mut ranges = Vec::with_capacity(self.ranges.len());
            for range in self.ranges.iter() {
                if range.contains(r) {
                    if range.from < r.from {
                        ranges.push(CharRange::from_range(range.from, Char::prev(r.from)));
                    }
                    if range.to > r.to {
                        ranges.push(CharRange::from_range(Char::next(r.to), range.to));
                    }
                } else if range.overlaps(r) {
                    if range.from < r.from {
                        ranges.push(CharRange::from_range(range.from, Char::prev(r.from)));
                    } else {
                        ranges.push(CharRange::from_range(Char::next(r.to), range.to));
                    }
                } else {
                    ranges.push(*range);
                }
            }
            self.ranges = ranges;
            self.recalc_cardinality();
        }
    }

    pub fn remove_char(&mut self, c: char) {
        self.remove(CharRange::from_char(c));
    }

    pub fn remove_range(&mut self, from: char, to: char) {
        self.remove(CharRange::from_range(from, to));
    }

    pub fn contains(&self, r: CharRange) -> bool {
        if self.ranges.is_empty() {
            false
        } else {
            self.ranges
                .binary_search_by(|&range| {
                    if range.contains(r) {
                        Ordering::Equal
                    } else if range < r {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                })
                .is_ok()
        }
    }

    pub fn contains_char(&self, c: char) -> bool {
        self.contains(CharRange::from_char(c))
    }

    pub fn contains_range(&self, from: char, to: char) -> bool {
        self.contains(CharRange::from_range(from, to))
    }

    pub fn overlaps(&self, r: CharRange) -> bool {
        if self.ranges.is_empty() {
            false
        } else {
            self.ranges
                .binary_search_by(|&range| {
                    if range.overlaps(r) {
                        Ordering::Equal
                    } else if range < r {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                })
                .is_ok()
        }
    }

    pub fn overlaps_range(&self, from: char, to: char) -> bool {
        self.overlaps(CharRange::from_range(from, to))
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    pub fn cardinality(&self) -> u32 {
        self.cardinality
    }

    pub fn iter(&self) -> ::std::slice::Iter<CharRange> {
        self.ranges.iter()
    }

    pub fn ranges(&self) -> usize {
        self.ranges.len()
    }

    pub fn is_ascii_range(&self) -> bool {
        self.ranges.is_empty() || self.ranges.last().unwrap().is_ascii_range()
    }

    pub fn is_byte_range(&self) -> bool {
        self.ranges.is_empty() || self.ranges.last().unwrap().is_byte_range()
    }
}

impl fmt::Display for CharSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        if self.cardinality < 0x10FFFF / 2 {
            for r in self.ranges.iter() {
                write!(f, "{}", r)?;
            }
        } else {
            write!(f, "^")?;
            for w in self.ranges.windows(2) {
                write!(f, "{}", CharRange::from_range(Char::next(w[0].to), Char::prev(w[1].from)))?;
            }
        }
        write!(f, "]")
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    Empty,
    Any,
    Set {
        set: CharSet,
    },
    Literal {
        chars: Vec<char>,
        icase: bool,
    },
    Repeat {
        e: Box<Regex>,
        min: u32,
        max: u32,
        greedy: bool,
    },
    Concat {
        es: Vec<Regex>,
    },
    Alternate {
        es: Vec<Regex>,
    },
}

impl Regex {
    pub fn parse(s: &str) -> Result<Regex, Error> {
        parse::Parser::parse(s)
    }

    pub fn simplify(self, nest_limit: usize) -> Result<Regex, Error> {
        use self::Regex::*;

        fn simp(expr: Regex, recurse: usize, limit: usize) -> Result<Regex, Error> {
            if recurse > limit {
                return Err(Error::Unspecified(line!()));
            }
            let simplify = |e| simp(e, recurse + 1, limit);
            Ok(match expr {
                Repeat { e, min, max, greedy } => {
                    if min == 1 && max == 1 {
                        simplify(*e)?
                    } else {
                        Repeat {
                            e: Box::new(simplify(*e)?),
                            min,
                            max,
                            greedy,
                        }
                    }
                }
                Concat { es } => {
                    let mut nes = Vec::with_capacity(es.len());
                    let mut again = false; // Concat containing one or more Concat needs one more recursion
                    for e in es {
                        match (nes.pop(), simplify(e)?) {
                            (None, e) => nes.push(e),
                            (Some(Literal { chars: mut chars1, icase: icase1 }),
                             Literal { chars: chars2, icase: icase2 }) => {
                                if icase1 == icase2 {
                                    chars1.extend(chars2);
                                    nes.push(Literal {
                                        chars: chars1,
                                        icase: icase1,
                                    });
                                } else {
                                    nes.push(Literal {
                                        chars: chars1,
                                        icase: icase1,
                                    });
                                    nes.push(Literal {
                                        chars: chars2,
                                        icase: icase2,
                                    });
                                }
                            }
                            (Some(Concat { es: es1 }), Concat { es: es2 }) => {
                                nes.extend(es1);
                                nes.extend(es2);
                                again = true;
                            }
                            (Some(Concat { es: es1 }), e2) => {
                                nes.extend(es1);
                                nes.push(e2);
                                again = true;
                            }
                            (Some(e1), Concat { es: es2 }) => {
                                nes.push(e1);
                                nes.extend(es2);
                                again = true;
                            }
                            (Some(e1), e2) => {
                                nes.push(e1);
                                nes.push(e2);
                            }
                        }
                    }
                    let e: Regex;
                    if nes.len() == 1 {
                        e = nes.pop().unwrap()
                    } else {
                        nes.shrink_to_fit();
                        e = Concat { es: nes }
                    }
                    if again {
                        simplify(e)?
                    } else {
                        e
                    }
                }
                Alternate { es } => {
                    let mut nes = Vec::with_capacity(es.len());
                    for e in es {
                        let e = simplify(e)?;
                        if let Alternate { es: es2 } = e {
                            for e in es2 {
                                nes.push(simplify(e)?);
                            }
                        } else {
                            nes.push(e);
                        }
                    }
                    nes.shrink_to_fit();
                    Alternate { es: nes }
                }
                Set { mut set } => {
                    set.ranges.shrink_to_fit();
                    Set { set: set }
                }
                e => e,
            })
        }
        simp(self, 0, nest_limit)
    }

    #[inline]
    fn precedence(&self) -> u32 {
        use self::Regex::*;

        match *self {
            Empty | Any | Set { .. } => 0,
            Literal { ref chars, .. } => {
                if chars.len() < 2 {
                    0
                } else {
                    1
                }
            }
            Repeat { .. } => 1,
            Concat { .. } => 2,
            Alternate { .. } => 3,
        }
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Regex::*;

        let p = self.precedence();

        match *self {
            Empty => Ok(()),
            Any => write!(f, "."),
            Set { ref set } => write!(f, "{}", set),
            Literal { ref chars, icase } => {
                if icase {
                    write!(f, "((?i)")?;
                }
                for &c in chars {
                    write!(f, "{:#}", Char::new(c))?;
                }
                if icase {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Repeat { ref e, min, max, greedy } => {
                let n = e.precedence();
                if p > n {
                    write!(f, "{}", e)?;
                } else {
                    write!(f, "({})", e)?;
                }
                if min == 0 && max == 1 {
                    write!(f, "?")?;
                } else if min == 0 && max == 0 {
                    write!(f, "*")?;
                } else if min == 1 && max == 0 {
                    write!(f, "+")?;
                } else if min == max {
                    write!(f, "{{{}}}", min)?;
                } else {
                    write!(f, "{{{},{}}}", min, max)?;
                }
                if !greedy {
                    write!(f, "?")?;
                }
                Ok(())
            }
            Concat { ref es } => {
                for expr in es {
                    if p > expr.precedence() {
                        write!(f, "{}", expr)?;
                    } else {
                        write!(f, "({})", expr)?;
                    }
                }
                Ok(())
            }
            Alternate { ref es } => {
                for (i, expr) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, "|")?;
                    }
                    if p > expr.precedence() {
                        write!(f, "{}", expr)?;
                    } else {
                        write!(f, "({})", expr)?;
                    }
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests;
