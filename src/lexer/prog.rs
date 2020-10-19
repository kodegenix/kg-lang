use super::*;

use std::collections::VecDeque;

/// Regex engine implementation in form of virtual machine. This is essentially equivalent to
/// epsilon-NFA (NFA with null moves), but it is convenient to implement as executable machine
/// with small memory footprint
#[derive(Debug, Clone)]
pub struct Program {
    code: Vec<Opcode>,
    masks: Vec<ByteMask>,
}

impl Program {
    pub fn from_regex(regex: &Regex, m: usize) -> Program {
        Compiler::new(m).compile(regex)
    }

    pub fn merge<'a, I, E>(progs: I) -> Program
        where I: IntoIterator<IntoIter = E>, E: ExactSizeIterator<Item = &'a Program>
    {
        let mut progs_iter = progs.into_iter();
        let progs_count = progs_iter.len();

        if progs_count == 1 {
            return progs_iter.next().unwrap().clone();
        }

        let mut prog = Program::new();

        for _ in 0..progs_count as u32 {
            prog.code.push(Opcode::Split(0, 0));
        }

        let mut op_offset = prog.code.len();
        let mut mask_offset = 0;
        let mut mask_map = HashMap::new();

        for (i, p) in progs_iter.enumerate() {
            for (i, m) in p.masks.iter().enumerate() {
                let mi = prog.add_mask(m.clone());
                mask_map.insert((mask_offset + i) as u32, mi);
            }
            for mut o in p.code.iter().cloned() {
                o.shift(op_offset as u32, mask_offset as u32);
                prog.code.push(o);
            }

            prog.code[i] = Opcode::Split(op_offset as u32, (i + 1) as u32);

            op_offset += p.code.len();
            mask_offset += p.masks.len();
        }

        // remapping masks, since we removed duplicated masks in merged program
        for p in prog.code.iter_mut() {
            if let &mut Opcode::Mask(_, ref mut m) = p {
                *m = *mask_map.get(m).unwrap();
            }
        }

        prog
    }

    fn new() -> Program {
        Program {
            code: Vec::new(),
            masks: Vec::new(),
        }
    }

    #[inline]
    pub fn code(&self) -> &[Opcode] {
        &self.code
    }

    #[inline]
    fn opcode_count(&self) -> u32 {
        self.code.len() as u32
    }

    pub fn opcode(&self, pc: u32) -> &Opcode {
        &self.code[pc as usize]
    }

    fn opcode_mut(&mut self, pc: u32) -> &mut Opcode {
        &mut self.code[pc as usize]
    }

    #[inline]
    fn add_opcode(&mut self, op: Opcode) -> u32 {
        let n = self.code.len() as u32;
        self.code.push(op);
        n
    }

    #[inline]
    pub fn masks(&self) -> &[ByteMask] {
        &self.masks
    }

    #[inline]
    pub fn mask(&self, mask: u32) -> &ByteMask {
        &self.masks[mask as usize]
    }

    fn add_mask(&mut self, mask: ByteMask) -> u32 {
        for (i, m) in self.masks.iter().enumerate() {
            if *m == mask {
                return i as u32;
            }
        }
        let idx = self.masks.len() as u32;
        self.masks.push(mask);
        idx
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Opcode::*;

        for (i, op) in self.code.iter().enumerate() {
            let i = i as u32;
            write!(f, "({:4}): ", i)?;
            match *op {
                Byte(goto, b) => write!(f, "byte  ({:4})  {:?}", goto, b as char)?,
                Range(goto, b1, b2) => write!(f, "range ({:4})  {:?}-{:?}", goto, b1 as char, b2 as char)?,
                Mask(goto, m) => write!(f, "mask  ({:4})  {}:{}", goto, m, self.mask(m))?,
                Split(goto1, goto2) => write!(f, "split ({:4}, {:4})", goto1, goto2)?,
                Match(m) => write!(f, "match [{}]", m)?,
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Match(u32),
    Byte(u32, u8),
    Range(u32, u8, u8),
    Mask(u32, u32),
    Split(u32, u32),
}

impl Opcode {
    fn shift(&mut self, goto_offset: u32, mask_offset: u32) {
        match *self {
            Opcode::Match(..) => { },
            Opcode::Byte(ref mut g, ..) => *g += goto_offset,
            Opcode::Range(ref mut g, ..) => *g += goto_offset,
            Opcode::Mask(ref mut g, ref mut m) => {
                *g += goto_offset;
                *m += mask_offset;
            }
            Opcode::Split(ref mut g1, ref mut g2) => {
                *g1 += goto_offset;
                *g2 += goto_offset;
            }
        }
    }
}


#[derive(Debug)]
struct Compiler {
    matching: usize,
    prog: Program,
}

impl Compiler {
    pub fn new(matching: usize) -> Compiler {
        Compiler {
            matching,
            prog: Program::new(),
        }
    }

    fn byte(&mut self, b: u8) -> Proc {
        let n = self.prog.opcode_count();
        self.prog.add_opcode(Opcode::Byte(n + 1, b));
        Proc(n, n + 1)
    }

    fn range(&mut self, a: u8, b: u8) -> Proc {
        let n = self.prog.opcode_count();
        self.prog.add_opcode(Opcode::Range(n + 1, a, b));
        Proc(n, n + 1)
    }

    fn mask(&mut self, m: ByteMask) -> Proc {
        let m = self.prog.add_mask(m);
        let n = self.prog.opcode_count();
        self.prog.add_opcode(Opcode::Mask(n + 1, m));
        Proc(n, n + 1)
    }

    fn star(&mut self, e: &Regex, greedy: bool) -> Proc {
        let s = self.prog.add_opcode(Opcode::Split(0, 0));
        let p = self.regex(e);
        let f = self.prog.add_opcode(Opcode::Split(0, 0));
        if greedy {
            *self.prog.opcode_mut(s) = Opcode::Split(p.0, p.1 + 1);
            *self.prog.opcode_mut(f) = Opcode::Split(p.0, f + 1);
        } else {
            *self.prog.opcode_mut(s) = Opcode::Split(p.1 + 1, p.0);
            *self.prog.opcode_mut(f) = Opcode::Split(f + 1, p.0);
        }
        Proc(s, f + 1)
    }

    fn plus(&mut self, e: &Regex, greedy: bool) -> Proc {
        let p = self.regex(e);
        let f = self.prog.add_opcode(Opcode::Split(0, 0));
        if greedy {
            *self.prog.opcode_mut(f) = Opcode::Split(p.0, f + 1);
        } else {
            *self.prog.opcode_mut(f) = Opcode::Split(f + 1, p.0);
        }
        Proc(p.0, f + 1)
    }

    fn option(&mut self, e: &Regex, greedy: bool) -> Proc {
        let s = self.prog.add_opcode(Opcode::Split(0, 0));
        let p = self.regex(e);
        if greedy {
            *self.prog.opcode_mut(s) = Opcode::Split(p.0, p.1);
        } else {
            *self.prog.opcode_mut(s) = Opcode::Split(p.1, p.0);
        }
        Proc(s, p.1)
    }

    fn regex(&mut self, r: &Regex) -> Proc {
        match *r {
            Regex::Literal { ref chars, icase } => {
                let mut p = Proc::default();
                if icase {
                    let mut up = String::with_capacity(chars.len());
                    let mut lo = String::with_capacity(chars.len());
                    for c in chars {
                        up.clear();
                        lo.clear();
                        for c in c.to_uppercase() {
                            up.push(c);
                        }
                        for c in c.to_lowercase() {
                            lo.push(c);
                        }
                        let mut up = up.bytes().fuse();
                        let mut lo = lo.bytes().fuse();

                        loop {
                            match (up.next(), lo.next()) {
                                (Some(b1), Some(b2)) if b1 == b2 => {
                                    p.merge(self.byte(b1));
                                }
                                (Some(b1), Some(b2)) => {
                                    let mut m = ByteMask::empty();
                                    m.include(b1);
                                    m.include(b2);
                                    p.merge(self.mask(m));
                                }
                                (Some(b1), None) => {
                                    p.merge(self.byte(b1));
                                }
                                (None, Some(b2)) => {
                                    p.merge(self.byte(b2));
                                }
                                (None, None) => break
                            }
                        }
                    }
                } else {
                    let mut buf = [0; 4];
                    for c in chars {
                        for b in c.encode_utf8(&mut buf).bytes() {
                            p = p.merge(self.byte(b));
                        }
                    }
                }
                p
            }
            Regex::Set { ref set } => {
                if set.is_ascii_range() {
                    if set.ranges() == 1 {
                        let range = set.iter().next().unwrap();
                        self.range(range.from() as u8, range.to() as u8)
                    } else {
                        let mut m = ByteMask::empty();
                        for r in set.iter() {
                            for c in r.chars() {
                                m.include(c as u8);
                            }
                        }
                        self.mask(m)
                    }
                } else {
                    //FIXME (jc) handle Unicode UTF8
                    let mut m = ByteMask::empty();
                    for r in set.iter() {
                        for c in r.chars() {
                            if c <= '\x7F' {
                                m.include(c as u8);
                            }
                        }
                    }
                    self.mask(m)
                }
            }
            Regex::Repeat { ref e, min, max, greedy } => {
                match (min, max) {
                    (0, 0) => { // eg. `a*`
                        self.star(e, greedy)
                    }
                    (1, 0) => { // eg. `a+`
                        self.plus(e, greedy)
                    }
                    (0, 1) => { // eg. `a?`
                        self.option(e, greedy)
                    }
                    (lo, 0) => { // eg. `a{2,}`
                        let mut s = Proc::default();
                        for _ in 0..lo {
                            s.merge(self.regex(e));
                        }
                        let p = self.star(e, greedy);
                        Proc(s.0, p.1)
                    }
                    (lo, up) => { // eg. `a{2}`, `a{2,2}`, `a{2,4}`
                        debug_assert!(lo <= up);
                        let mut s = Proc::default();
                        for _ in 0..lo {
                            s.merge(self.regex(e));
                        }
                        for _ in 0..up - lo {
                            s.merge(self.option(e, greedy));
                        }
                        s
                    }
                }
            }
            Regex::Concat { ref es } => {
                let mut n = Proc::default();
                for e in es {
                    n.merge(self.regex(e));
                }
                n
            }
            Regex::Alternate { ref es } => {
                debug_assert!(es.len() > 1);
                let s = self.prog.opcode_count();
                for _ in 0..es.len() - 1 {
                    self.prog.add_opcode(Opcode::Split(0, 0));
                }
                let mut procs = VecDeque::with_capacity(es.len());
                for e in es {
                    let p = self.regex(e);
                    procs.push_back(p);
                }
                let mut n = Proc::default();
                for (i, p) in procs.iter().skip(1).cloned().enumerate() {
                    let i = i as u32;
                    *self.prog.opcode_mut(s + i) = Opcode::Split(s + i + 1, p.0);
                    n.merge(p);
                }
                // the last arm will have correct goto(s), so we remove it from `procs`
                procs.pop_back();
                // in other arms the goto(s) must be corrected to jump after the alternative
                for p in procs {
                    let pc = p.1 - 1;
                    let old_goto = pc + 1;
                    let new_goto = n.1;
                    debug_assert_ne!(old_goto, new_goto);
                    match self.prog.opcode_mut(pc) {
                        Opcode::Match(..) => unreachable!(),
                        Opcode::Mask(ref mut g, ..) |
                        Opcode::Byte(ref mut g, ..) |
                        Opcode::Range(ref mut g, ..) => {
                            debug_assert_eq!(*g, old_goto);
                            *g = new_goto;
                        },
                        Opcode::Split(ref mut g1, ref mut g2) => {
                            debug_assert!(*g1 == old_goto || *g2 == old_goto);
                            if *g1 == old_goto {
                                *g1 = new_goto;
                            } else {
                                *g2 = new_goto;
                            }
                        },
                    }
                }
                Proc(s, n.1)
            }
            Regex::Any => {
                //FIXME (jc) handle Unicode UTF8
                self.range(b'\0', b'\x7F')
            }
            Regex::Empty => {
                unreachable!();
            }
        }
    }

    pub fn compile(mut self, regex: &Regex) -> Program {
        self.regex(regex);
        self.prog.add_opcode(Opcode::Match(self.matching as u32));
        self.prog
    }
}


#[derive(Debug, Clone, Copy)]
struct Proc(u32, u32);

impl Proc {
    fn is_empty(&self) -> bool {
        self.0 == EMPTY_GOTO
    }

    fn merge(&mut self, p: Proc) -> Proc {
        if self.is_empty() {
            *self = p;
        } else if !p.is_empty() {
            debug_assert!(self.1 == p.0);
            self.1 = p.1;
        }
        *self
    }
}

impl Default for Proc {
    fn default() -> Proc {
        Proc(EMPTY_GOTO, EMPTY_GOTO)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Processing,
    Failed,
    Matched(u32),
}

#[derive(Debug)]
pub struct Machine {
    program: Program,
    pc1: SparseSet<u32>,
    pc2: SparseSet<u32>,
}

impl Machine {
    pub fn new(program: Program) -> Machine {
        let len = program.code.len();
        Machine {
            program,
            pc1: SparseSet::with_capacity(len),
            pc2: SparseSet::with_capacity(len),
        }
    }

    fn swap(&mut self) {
        std::mem::swap(&mut self.pc1, &mut self.pc2);
    }

    fn restart(&mut self) {
        self.pc2.clear();
        self.push_pc(0);
        self.swap();
    }

    fn push_pc(&mut self, pc: u32) {
        if !self.pc2.contains(&pc) {
            match *self.program.opcode(pc) {
                Opcode::Split(goto1, goto2) => {
                    self.push_pc(goto1);
                    self.push_pc(goto2);
                }
                _ => {
                    self.pc2.insert(pc);
                }
            }
        }
    }

    fn step(&mut self, input: u8) -> Status {
        let mut matched = 0;

        self.pc2.clear();

        // must be iterated by indexing, because `self` must be mutably borrowed within loop
        // and iterator over `self.pc1` would already borrow `self` immutably
        for i in 0..self.pc1.len() {
            let pc = self.pc1[i];
            let opcode = *self.program.opcode(pc);
            match opcode {
                Opcode::Match(m) => {
                    matched = m;
                    break;
                }
                Opcode::Byte(goto, b) => {
                    if input == b {
                        self.push_pc(goto);
                    }
                }
                Opcode::Range(goto, b1, b2) => {
                    if input >= b1 && input <= b2 {
                        self.push_pc(goto);
                    }
                }
                Opcode::Mask(goto, m) => {
                    if self.program.mask(m).matches(input) {
                        self.push_pc(goto);
                    }
                }
                _ => unreachable!()
            }
        }

        self.swap();

        if self.pc1.is_empty() {
            if matched > 0 {
                Status::Matched(matched)
            } else {
                Status::Failed
            }
        } else {
            Status::Processing
        }
    }
}


#[derive(Debug)]
pub struct ProgMatcher {
    machines: Vec<Machine>,
    stack1: Vec<usize>,
    stack2: Vec<usize>,
}

impl ProgMatcher {
    pub fn from_regexes<'a, I: Iterator<Item = &'a Regex>>(regexes: I) -> ProgMatcher {
        let mut machines = Vec::with_capacity(regexes.size_hint().0);

        for (i, r) in regexes.enumerate() {
            let p = Program::from_regex(r, i + 1);
            machines.push(Machine::new(p));
        }

        let len = machines.len();
        ProgMatcher {
            machines,
            stack1: Vec::with_capacity(len),
            stack2: Vec::with_capacity(len),
        }
    }

    pub fn from_progs<I: Iterator<Item = Program>>(progs: I) -> ProgMatcher {
        let mut machines = Vec::with_capacity(progs.size_hint().0);

        for p in progs {
            machines.push(Machine::new(p));
        }

        let len = machines.len();
        ProgMatcher {
            machines,
            stack1: Vec::with_capacity(len),
            stack2: Vec::with_capacity(len),
        }
    }

    fn swap(&mut self) {
        std::mem::swap(&mut self.stack1, &mut self.stack2);
    }

    fn reset(&mut self) {
        for m in self.machines.iter_mut() {
            m.restart();
        }
        self.stack1.clear();
        self.stack1.extend(0..self.machines.len());
    }

    pub fn exec(&mut self, reader: &mut dyn ByteReader) -> IoResult<Option<Match>> {
        if reader.eof() {
            Ok(None)
        } else {
            self.reset();

            let mut matched = 0;

            while let Some(c) = reader.peek_byte(0)? {
                self.stack2.clear();

                for &m in self.stack1.iter() {
                    match self.machines[m].step(c) {
                        Status::Processing => {
                            self.stack2.push(m)
                        },
                        Status::Matched(m) => {
                            matched = m;
                        },
                        Status::Failed => {},
                    }
                }

                if self.stack2.is_empty() {
                    break;
                }

                self.swap();
                reader.next_byte()?;
            }

            if reader.eof() {
                for m in self.stack1.iter().cloned() {
                    if let Status::Matched(m) = self.machines[m].step(0xFFu8) {
                        matched = m;
                    }
                }
            }

            if matched > 0 {
                Ok(Some(Match::new(matched)))
            } else {
                Ok(None)
            }
        }
    }
}


#[derive(Debug)]
pub struct ProgLexer {
    matcher: ProgMatcher,
    grammar: GrammarRef,
    unmatched: usize,
    mode: usize,
}

impl ProgLexer {
    pub fn new(grammar: &GrammarRef, unmatched: usize, mode: usize) -> ProgLexer {
        let g = grammar.borrow();

        let mut lexemes: Vec<&Lexeme> = g.terminals().iter()
            .filter(|lexeme| lexeme.has_mode(mode) && match *lexeme.regex() {
                Regex::Empty | Regex::Any => false,
                _ => true,
            })
            .collect();

        lexemes.sort_by(|a, b| {
            let pa = a.precedence(mode);
            let pb = b.precedence(mode);
            match pa.cmp(&pb) {
                Ordering::Equal => a.index().cmp(&b.index()),
                o => o
            }
        });

        let matcher = ProgMatcher::from_progs(lexemes.iter().map(|lexeme| {
            Program::from_regex(lexeme.regex(), lexeme.index())
        }));

        ProgLexer {
            matcher,
            grammar: grammar.clone(),
            unmatched,
            mode,
        }
    }
}

impl Lexer for ProgLexer {
    fn reset(&mut self) {
        self.matcher.reset();
    }

    fn lex(&mut self, reader: &mut dyn ByteReader) -> Result<Token, LexerError> {
        let s = reader.position();

        if reader.eof() {
            Ok(Token::with_id(0, "$".into(), "".into(), s, s))
        } else {
            match self.matcher.exec(reader)? {
                Some(m) => {
                    let g = self.grammar.borrow();
                    let t = g.terminal(m.matching() as usize);
                    let e = reader.position();
                    let mut token = Token::with_id(t.index(), t.id().into(), reader.slice(s.offset, e.offset)?.into(), s, e);
                    token.set_mode(self.mode);
                    Ok(token)
                }
                None => {
                    if self.unmatched > 0 {
                        reader.next_byte()?;
                        let g = self.grammar.borrow();
                        let t = g.terminal(self.unmatched);
                        let e = reader.position();
                        let mut token = Token::with_id(t.index(), t.id().into(), reader.slice(s.offset, e.offset)?.into(), s, e);
                        token.set_mode(self.mode);
                        Ok(token)
                    } else {
                        Err(LexerError::UnexpectedInput(s))
                    }
                }
            }
        }
    }

    fn unmatched(&self) -> usize {
        self.unmatched
    }

    fn mode(&self) -> usize {
        self.mode
    }
}


//FIXME (jc)
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let re1 = Regex::parse("/\\*.*?\\*/").unwrap();
        let re2 = Regex::parse("class").unwrap();
        let re3 = Regex::parse("[0-9a-f]+").unwrap();
        let re4 = Regex::parse("[a-zA-Z_][0-9a-zA-Z_]*").unwrap();

        println!("{:?}", re1);
        println!("{:?}", re2);
        println!("{:?}", re3);
        println!("{:?}", re4);

        let p1 = Program::from_regex(&re1, 1);
        let p2 = Program::from_regex(&re2, 2);
        let p3 = Program::from_regex(&re3, 3);
        let p4 = Program::from_regex(&re4, 4);

       // let p = Program::merge(&[p1, p2, p3, p4]);
        //eprintln!("\n{}", p);

        let mut m = ProgMatcher::from_progs(vec![p1, p2, p3, p4].into_iter());

        let mut r = MemByteReader::new(b"/* */123a/****/*/ class classa  ");
        println!("{}", r.input().unwrap());

        while !r.eof() {
            let p1 = r.position();
            match m.exec(&mut r).unwrap() {
                Some(m) => {
                    let s = r.slice_pos(p1, r.position()).unwrap();
                    eprintln!("match {}: {:?}", m.matching(), s);
                },
                None => {
                    r.next_byte().unwrap();
                    eprintln!("?");
                }
            }
        }
    }

}