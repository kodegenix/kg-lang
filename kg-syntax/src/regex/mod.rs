use super::*;

use std::cmp::Ordering;

mod parse;

pub use self::parse::Error as ParseError;
pub use self::parse::ParseErrorDetail;
pub use self::parse::Parser as RegexParser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    Empty,
    Any,
    Set(CharSet),
    //Class(CharClass),
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
    Concat(Vec<Regex>),
    Alternate(Vec<Regex>),
}

impl Regex {
    pub fn parse(s: &str) -> Result<Regex, ParseError> {
        parse::Parser::new().parse_str(s)
    }

    fn simplify(self, nest_limit: usize) -> Result<Regex, ParseError> {
        fn simp(expr: Regex, recurse: usize, limit: usize) -> Result<Regex, ParseError> {
            if recurse > limit {
                return Err(ParseErrorDetail::Unspecified(line!()).into());
            }
            let simplify = |e| simp(e, recurse + 1, limit);
            Ok(match expr {
                Regex::Repeat { e, min, max, greedy } => {
                    if min == 1 && max == 1 {
                        simplify(*e)?
                    } else {
                        Regex::Repeat {
                            e: Box::new(simplify(*e)?),
                            min,
                            max,
                            greedy,
                        }
                    }
                }
                Regex::Concat(es) => {
                    let mut nes = Vec::with_capacity(es.len());
                    let mut again = false; // Concat containing one or more Concat needs one more recursion
                    for e in es {
                        match (nes.pop(), simplify(e)?) {
                            (None, e) => nes.push(e),
                            (Some(Regex::Literal { chars: mut chars1, icase: icase1 }),
                                Regex::Literal { chars: chars2, icase: icase2 }) => {
                                if icase1 == icase2 {
                                    chars1.extend(chars2);
                                    nes.push(Regex::Literal {
                                        chars: chars1,
                                        icase: icase1,
                                    });
                                } else {
                                    nes.push(Regex::Literal {
                                        chars: chars1,
                                        icase: icase1,
                                    });
                                    nes.push(Regex::Literal {
                                        chars: chars2,
                                        icase: icase2,
                                    });
                                }
                            }
                            (Some(Regex::Concat(es1)), Regex::Concat(es2)) => {
                                nes.extend(es1);
                                nes.extend(es2);
                                again = true;
                            }
                            (Some(Regex::Concat(es1)), e2) => {
                                nes.extend(es1);
                                nes.push(e2);
                                again = true;
                            }
                            (Some(e1), Regex::Concat(es2)) => {
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
                        e = Regex::Concat(nes)
                    }
                    if again {
                        simplify(e)?
                    } else {
                        e
                    }
                }
                Regex::Alternate(es) => {
                    let mut nes = Vec::with_capacity(es.len());
                    for e in es {
                        let e = simplify(e)?;
                        if let Regex::Alternate(es2) = e {
                            for e in es2 {
                                nes.push(simplify(e)?);
                            }
                        } else {
                            nes.push(e);
                        }
                    }
                    nes.shrink_to_fit();
                    Regex::Alternate(nes)
                }
                Regex::Set(mut set) => {
                    set.ranges.shrink_to_fit();
                    Regex::Set(set)
                }
                e => e,
            })
        }
        simp(self, 0, nest_limit)
    }

    #[inline]
    fn precedence(&self) -> u32 {
        match *self {
            Regex::Empty | Regex::Any | Regex::Set(..) => 0,
            Regex::Literal { ref chars, .. } => {
                if chars.len() < 2 {
                    0
                } else {
                    1
                }
            }
            Regex::Repeat { .. } => 1,
            Regex::Concat(..) => 2,
            Regex::Alternate(..) => 3,
        }
    }
}

impl std::fmt::Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let p = self.precedence();

        match *self {
            Regex::Empty => Ok(()),
            Regex::Any => write!(f, "."),
            Regex::Set(ref set) => write!(f, "{}", set),
            Regex::Literal { ref chars, icase } => {
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
            Regex::Repeat { ref e, min, max, greedy } => {
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
            Regex::Concat(ref es) => {
                for expr in es {
                    if p > expr.precedence() {
                        write!(f, "{}", expr)?;
                    } else {
                        write!(f, "({})", expr)?;
                    }
                }
                Ok(())
            }
            Regex::Alternate(ref es) => {
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
            std::char::from_u32_unchecked(c as u32 - 1)
        }
    }

    fn next(c: char) -> char {
        unsafe {
            std::char::from_u32_unchecked(c as u32 + 1)
        }
    }
}

impl std::fmt::Display for Char {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    start: char,
    end: char,
}

impl CharRange {
    pub fn from_char(c: char) -> CharRange {
        CharRange { start: c, end: c }
    }

    pub fn from_range(start: char, end: char) -> CharRange {
        CharRange {
            start: std::cmp::min(start, end),
            end: std::cmp::max(start, end),
        }
    }

    pub fn contains(&self, r: CharRange) -> bool {
        self.start <= r.start && self.end >= r.end
    }

    pub fn overlaps(&self, r: CharRange) -> bool {
        self.start <= r.end && self.end >= r.start
    }

    pub fn adjacent(&self, r: CharRange) -> bool {
        self.start as u32 == r.end as u32 + 1 || self.end as u32 + 1 == r.start as u32
    }

    pub fn overlaps_or_adjacent(&self, r: CharRange) -> bool {
        self.start as u32 <= r.end as u32 + 1 && self.end as u32 + 1 >= r.start as u32
    }

    pub fn cardinality(&self) -> u32 {
        self.end as u32 - self.start as u32 + 1
    }

    pub fn chars(&self) -> CharRangeIter {
        CharRangeIter {
            range: self,
            curr: self.start as u32,
        }
    }

    pub fn is_ascii_range(&self) -> bool {
        self.end <= '\x7F'
    }

    pub fn is_byte_range(&self) -> bool {
        self.end as u32 <= 255
    }

    pub fn start(&self) -> char {
        self.start
    }

    pub fn end(&self) -> char {
        self.end
    }
}

impl std::fmt::Display for CharRange {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.end as u32 - self.start as u32 {
            0 => write!(f, "{}", Char::new(self.start)),
            1 => write!(f, "{}{}", Char::new(self.start), Char::new(self.end)),
            _ => write!(f, "{}-{}", Char::new(self.start), Char::new(self.end)),
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
        if self.curr <= self.range.end as u32 {
            unsafe {
                c = Some(std::char::from_u32_unchecked(self.curr));
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
                        if r.start < range.start && index > 0 {
                            let mut r0 = self.ranges[index - 1];
                            if r0.adjacent(r) {
                                r0.end = std::cmp::max(range.end, r.end);
                                self.ranges[index - 1] = r0;
                                self.ranges.remove(index);
                                self.recalc_cardinality();
                                return;
                            }
                        }
                        if r.end > range.end && index < self.ranges.len() - 1 {
                            let mut r1 = self.ranges[index + 1];
                            if r1.adjacent(r) {
                                r1.start = std::cmp::min(range.start, r.start);
                                self.ranges[index] = r1;
                                self.ranges.remove(index + 1);
                                self.recalc_cardinality();
                                return;
                            }
                        }
                        range.start = std::cmp::min(range.start, r.start);
                        range.end = std::cmp::max(range.end, r.end);
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
            if r.start > '\0' {
                self.ranges.push(CharRange::from_range('\0', Char::prev(r.start)));
            }
            if r.end < '\u{10FFFF}' {
                self.ranges.push(CharRange::from_range(Char::next(r.end), '\u{10FFFF}'));
            }
            self.recalc_cardinality();
        } else {
            let mut ranges = Vec::with_capacity(self.ranges.len());
            for range in self.ranges.iter() {
                if range.contains(r) {
                    if range.start < r.start {
                        ranges.push(CharRange::from_range(range.start, Char::prev(r.start)));
                    }
                    if range.end > r.end {
                        ranges.push(CharRange::from_range(Char::next(r.end), range.end));
                    }
                } else if range.overlaps(r) {
                    if range.start < r.start {
                        ranges.push(CharRange::from_range(range.start, Char::prev(r.start)));
                    } else {
                        ranges.push(CharRange::from_range(Char::next(r.end), range.end));
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

    pub fn contains_range(&self, start: char, end: char) -> bool {
        self.contains(CharRange::from_range(start, end))
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

    pub fn overlaps_range(&self, start: char, end: char) -> bool {
        self.overlaps(CharRange::from_range(start, end))
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

impl std::fmt::Display for CharSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        if self.cardinality < 0x10FFFF / 2 {
            for r in self.ranges.iter() {
                write!(f, "{}", r)?;
            }
        } else {
            write!(f, "^")?;
            for w in self.ranges.windows(2) {
                write!(f, "{}", CharRange::from_range(Char::next(w[0].end), Char::prev(w[1].start)))?;
            }
        }
        write!(f, "]")
    }
}


#[cfg(test)]
mod tests;
