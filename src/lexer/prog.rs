use super::*;

use kg_utils::collections::SparseSet;
use kg_diag::parse::ParseResult;


const EMPTY_GOTO: u32 = ::std::u32::MAX;

#[derive(Debug, Clone, Copy)]
enum Opcode {
    Match(usize),
    Byte(u32, u8),
    Range(u32, u8, u8),
    Mask(u32, u32),
    Split(u32, u32),
}

impl Opcode {
    fn goto_iter(&self) -> GotoIter {
        GotoIter::new(self)
    }

    fn goto_iter_mut(&mut self) -> GotoIterMut {
        GotoIterMut::new(self)
    }

    fn goto_count(&self) -> usize {
        use self::Opcode::*;

        match *self {
            Match(..) => 0,
            Byte(..) | Range(..) | Mask(..) => 1,
            Split(..) => 2,
        }
    }

    fn get_goto(&self, idx: usize) -> Option<&u32> {
        use self::Opcode::*;

        if idx == 0 {
            match *self {
                Match(..) => None,
                Byte(ref g, ..) |
                Range(ref g, ..) |
                Mask(ref g, ..) |
                Split(ref g, ..) => Some(g),
            }
        } else if idx == 1 {
            match *self {
                Split(_, ref g) => Some(g),
                _ => None,
            }
        } else {
            None
        }
    }

    fn get_goto_mut(&mut self, idx: usize) -> Option<&mut u32> {
        use self::Opcode::*;

        if idx == 0 {
            match *self {
                Match(..) => None,
                Byte(ref mut g, ..) |
                Range(ref mut g, ..) |
                Mask(ref mut g, ..) |
                Split(ref mut g, ..) => Some(g),
            }
        } else if idx == 1 {
            match *self {
                Split(_, ref mut g) => Some(g),
                _ => None,
            }
        } else {
            None
        }
    }
}


struct GotoIter<'a> {
    opcode: &'a Opcode,
    count: usize,
    len: usize,
}

impl<'a> GotoIter<'a> {
    fn new(opcode: &'a Opcode) -> GotoIter<'a> {
        GotoIter {
            count: 0,
            len: opcode.goto_count(),
            opcode: opcode,
        }
    }
}

impl<'a> Iterator for GotoIter<'a> {
    type Item = &'a u32;

    fn next(&mut self) -> Option<&'a u32> {
        let idx = self.count;
        self.count += 1;
        self.opcode.get_goto(idx)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a> ExactSizeIterator for GotoIter<'a> {
    fn len(&self) -> usize {
        self.len - self.count
    }
}

struct GotoIterMut<'a> {
    opcode: &'a mut Opcode,
    count: usize,
    len: usize,
}

impl<'a> GotoIterMut<'a> {
    fn new(opcode: &'a mut Opcode) -> GotoIterMut<'a> {
        GotoIterMut {
            count: 0,
            len: opcode.goto_count(),
            opcode: opcode,
        }
    }
}

impl<'a> Iterator for GotoIterMut<'a> {
    type Item = &'a mut u32;

    fn next(&mut self) -> Option<&'a mut u32> {
        let idx = self.count;
        self.count += 1;
        unsafe {
            std::mem::transmute::<Option<&mut u32>, Option<&'a mut u32>>(self.opcode.get_goto_mut(idx))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a> ExactSizeIterator for GotoIterMut<'a> {
    fn len(&self) -> usize {
        self.len - self.count
    }
}



#[derive(Debug)]
pub struct Program {
    code: Vec<Opcode>,
    masks: Vec<Mask>,
}

impl Program {
    pub fn from_regex(regex: &Regex, m: usize) -> Program {
        Compiler::new().compile_regex(regex, m)
    }

    fn new() -> Program {
        Program {
            code: Vec::new(),
            masks: Vec::new(),
        }
    }

    fn opcode(&self, pc: u32) -> Opcode {
        self.code[pc as usize]
    }

    fn mask(&self, mask: u32) -> &Mask {
        &self.masks[mask as usize]
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Opcode::*;

        for (i, op) in self.code.iter().enumerate() {
            let i = i as u32;
            write!(f, "{:04} ", i)?;
            match *op {
                Byte(goto, b) => write!(f, "byte  ({})    {:?}", goto, b as char)?,
                Range(goto, b1, b2) => write!(f, "range ({})    {:?}-{:?}", goto, b1 as char, b2 as char)?,
                Mask(goto, m) => write!(f, "mask  ({})    {}:{}", goto, m, self.mask(m))?,
                Split(goto1, goto2) => write!(f, "split ({}, {})", goto1, goto2)?,
                Match(m) => write!(f, "match        {}", m)?,
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}


struct Mask([bool; 256]);

impl Mask {
    fn new() -> Mask {
        Self::empty()
    }

    fn empty() -> Mask {
        Mask([false; 256])
    }

    fn full() -> Mask {
        Mask([true; 256])
    }

    fn matches(&self, input: u8) -> bool {
        self.0[input as usize]
    }

    fn include(&mut self, input: u8) {
        self.0[input as usize] = true;
    }

    fn exclude(&mut self, input: u8) {
        self.0[input as usize] = false;
    }

    fn negate(&mut self) {
        for i in 0..256 {
            self.0[i] = !self.0[i];
        }
    }
}

impl PartialEq for Mask {
    fn eq(&self, other: &Self) -> bool {
        use libc::{c_void, memcmp};

        unsafe { memcmp(self.0.as_ptr() as *const c_void, other.0.as_ptr() as *const c_void, 256) == 0 }
    }
}

impl Eq for Mask { }

impl std::fmt::Debug for Mask {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl std::fmt::Display for Mask {
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


#[derive(Debug)]
struct Compiler {
    opcodes: Vec<OpcodeEx>,
    masks: Vec<Mask>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            opcodes: Vec::new(),
            masks: Vec::new(),
        }
    }

    fn add_opcode(&mut self, opcode: Opcode) -> &mut OpcodeEx {
        self.opcodes.push(OpcodeEx::new(opcode));
        self.opcodes.last_mut().unwrap()
    }

    fn add_mask(&mut self, mask: Mask) -> u32 {
        for (i, m) in self.masks.iter().enumerate() {
            if *m == mask {
                return i as u32;
            }
        }
        let idx = self.masks.len() as u32;
        self.masks.push(mask);
        idx
    }

    fn opcode(&self, addr: u32) -> &OpcodeEx {
        &self.opcodes[addr as usize]
    }

    fn opcode_mut(&mut self, addr: u32) -> &mut OpcodeEx {
        &mut self.opcodes[addr as usize]
    }

    fn unwind(&self, e: Proc) -> (Vec<u32>, HashMap<u32, u32>) {
        fn append(compiler: &Compiler, op: u32, opcodes: &mut Vec<u32>, remaps: &mut HashMap<u32, u32>, gotos: &mut HashMap<u32, u32>) {
            if !remaps.contains_key(&op) {
                let opcode = compiler.opcode(op);
                if let Some(g) = opcode.goto {
                    gotos.insert(op, g);
                    append(compiler, g, opcodes, remaps, gotos);
                } else {
                    remaps.insert(op, opcodes.len() as u32);
                    opcodes.push(op);
                    if let Some(se) = opcode.split_end {
                        remaps.insert(se, 0);
                        let mut iter = opcode.goto_iter().peekable();
                        while let Some(e) = iter.next() {
                            if iter.peek().is_none() {
                                remaps.remove(&se);
                            }
                            append(compiler, *e, opcodes, remaps, gotos);
                        }
                    } else {
                        for e in opcode.goto_iter() {
                            append(compiler, *e, opcodes, remaps, gotos);
                        }
                    }
                }
            }
        }

        if e.is_empty() {
            (Vec::new(), HashMap::new())
        } else {
            let mut opcodes = Vec::with_capacity(self.opcodes.len());
            let mut remaps = HashMap::with_capacity(self.opcodes.len());
            let mut gotos = HashMap::new();
            append(self, e.0, &mut opcodes, &mut remaps, &mut gotos);
            loop {
                let mut again = false;
                for (&k, &v) in gotos.iter() {
                    if let Some(&v) = remaps.get(&v) {
                        remaps.insert(k, v);
                    } else {
                        again = true;
                    }
                }
                if !again {
                    break;
                }
            }
            (opcodes, remaps)
        }
    }

    fn join(&mut self, a: u32, b: u32) {
        let se;
        {
            let op1 = self.opcode_mut(a);
            op1.goto = Some(b);
            se = op1.split_end;
        }
        {
            let op2 = self.opcode_mut(b);
            if op2.split_end.is_none() {
                op2.split_end = se;
            }
        }
    }

    fn copy(&mut self, e: Proc) -> Proc {
        let (opcodes, remaps) = self.unwind(e);
        let start = self.opcodes.len() as u32;

        self.opcodes.reserve(opcodes.len());
        for op in opcodes.iter() {
            let mut opcode = self.opcode(*op).clone();
            for g in opcode.goto_iter_mut() {
                *g = *remaps.get(g).unwrap();
            }
            if let Some(g) = opcode.split_end {
                opcode.split_end = Some(*remaps.get(&g).unwrap());
            }
            self.opcodes.push(opcode);
        }
        Proc(start, self.opcodes.len() as u32 - 1)
    }

    fn matching(&mut self, p: Proc, m: usize) -> Proc {
        let op = self.opcode_mut(p.1);
        if let Opcode::Match(ref mut pm) = op.opcode {
            *pm = m;
        } else {
            unreachable!();
        }
        p
    }

    fn byte(&mut self, b: u8) -> Proc {
        let s = self.opcodes.len() as u32;
        let f = s + 1;
        self.add_opcode(Opcode::Byte(f, b));
        self.add_opcode(Opcode::Match(1));
        Proc(s, f)
    }

    fn range(&mut self, b1: u8, b2: u8) -> Proc {
        let s = self.opcodes.len() as u32;
        let f = s + 1;
        self.add_opcode(Opcode::Range(f, b1, b2));
        self.add_opcode(Opcode::Match(1));
        Proc(s, f)
    }

    fn mask(&mut self, mask: u32) -> Proc {
        let s = self.opcodes.len() as u32;
        let f = s + 1;
        self.add_opcode(Opcode::Mask(f, mask));
        self.add_opcode(Opcode::Match(1));
        Proc(s, f)
    }

    fn concatenate(&mut self, p1: Proc, p2: Proc) -> Proc {
        if p1.is_empty() {
            p2
        } else if p2.is_empty() {
            p1
        } else {
            self.join(p1.1, p2.0);
            Proc(p1.0, p2.1)
        }
    }

    fn alternate(&mut self, p1: Proc, p2: Proc) -> Proc {
        if p1.is_empty() {
            p2
        } else if p2.is_empty() {
            p1
        } else {
            let s = self.opcodes.len() as u32;
            let f = s + 1;
            self.add_opcode(Opcode::Split(p1.0, p2.0)).split_end = Some(f);
            self.add_opcode(Opcode::Match(1));
            self.join(p1.1, f);
            self.join(p2.1, f);
            Proc(s, f)
        }
    }

    fn star(&mut self, p: Proc, greedy: bool) -> Proc {
        let s = self.opcodes.len() as u32;
        let e = s + 1;
        let f = e + 1;
        self.add_opcode(if greedy {
            Opcode::Split(p.0, f)
        } else {
            Opcode::Split(f, p.0)
        }).split_end = Some(f);
        self.add_opcode(if greedy {
            Opcode::Split(p.0, f)
        } else {
            Opcode::Split(f, p.0)
        });
        self.add_opcode(Opcode::Match(1));
        self.join(p.1, e);
        Proc(s, f)
    }

    fn plus(&mut self, p: Proc, greedy: bool) -> Proc {
        let s = p.0;
        let e = self.opcodes.len() as u32;
        let f = e + 1;
        self.add_opcode(if greedy {
            Opcode::Split(s, f)
        } else {
            Opcode::Split(f, s)
        });
        self.add_opcode(Opcode::Match(1));
        self.join(p.1, e);
        Proc(s, f)
    }

    fn option(&mut self, p: Proc, greedy: bool) -> Proc {
        let s = self.opcodes.len() as u32;
        let f = s + 1;
        self.add_opcode(if greedy {
            Opcode::Split(p.0, f)
        } else {
            Opcode::Split(f, p.0)
        }).split_end = Some(f);
        self.add_opcode(Opcode::Match(1));
        self.join(p.1, f);
        Proc(s, f)
    }

    fn build(self, p: Proc) -> Program {
        let (opcodes, remaps) = self.unwind(p);
        let mut prog = Program::new();
        prog.code.reserve_exact(opcodes.len());
        unsafe {
            prog.code.set_len(opcodes.len());
        }
        for (i, op) in opcodes.iter().enumerate() {
            let mut opcode = self.opcode(*op).opcode;
            for g in opcode.goto_iter_mut() {
                *g = *remaps.get(g).unwrap();
            }
            prog.code[i] = opcode;
        }
        prog.masks = self.masks;
        prog
    }

    fn regex(&mut self, r: &Regex) -> Proc {
        match *r {
            Regex::Literal { ref chars, icase } => {
                let mut n = Proc::default();
                let mut up = String::with_capacity(10);
                let mut lo = String::with_capacity(10);

                if icase {
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
                                    let s1 = self.byte(b1);
                                    n = self.concatenate(n, s1);
                                }
                                (Some(b1), Some(b2)) => {
                                    let mut m = Mask::empty();
                                    m.include(b1);
                                    m.include(b2);
                                    let m = self.add_mask(m);
                                    let m = self.mask(m);
                                    n = self.concatenate(n, m);
                                }
                                (Some(b1), None) => {
                                    let s1 = self.byte(b1);
                                    n = self.concatenate(n, s1);
                                }
                                (None, Some(b2)) => {
                                    let s2 = self.byte(b2);
                                    n = self.concatenate(n, s2);
                                }
                                (None, None) => break
                            }
                        }
                    }
                } else {
                    let mut buf = [0; 4];
                    for c in chars {
                        for b in c.encode_utf8(&mut buf).bytes() {
                            let n2 = self.byte(b);
                            n = self.concatenate(n, n2);
                        }
                    }
                }
                n
            }
            Regex::Set { ref set } => {
                if set.is_ascii_range() {
                    if set.ranges() == 1 {
                        let range = set.iter().next().unwrap();
                        let r = self.range(range.from() as u8, range.to() as u8);
                        r
                    } else {
                        let mut m = Mask::empty();
                        for r in set.iter() {
                            for c in r.chars() {
                                m.include(c as u8);
                            }
                        }
                        let m = self.add_mask(m);
                        let m = self.mask(m);
                        m
                    }
                } else {
                    //FIXME (jc) handle Unicode UTF8
                    let mut m = Mask::empty();
                    for r in set.iter() {
                        for c in r.chars() {
                            if c <= '\x7F' {
                                m.include(c as u8);
                            }
                        }
                    }
                    let m = self.add_mask(m);
                    let m = self.mask(m);
                    m
                }
            }
            Regex::Repeat { ref e, min, max, greedy } => {
                let n = self.regex(e);
                match (min, max) {
                    (0, 0) => {
                        self.star(n, greedy)
                    }
                    (1, 0) => {
                        self.plus(n, greedy)
                    }
                    (0, 1) => {
                        self.option(n, greedy)
                    }
                    (_, _) => {
                        //FIXME (jc) handle counted repeats
                        unimplemented!();
                    }
                }
            }
            Regex::Concat { ref es } => {
                let mut n = Proc::default();
                for e in es {
                    let s = self.regex(e);
                    n = self.concatenate(n, s);
                }
                n
            }
            Regex::Alternate { ref es } => {
                let mut n = Proc::default();
                for e in es {
                    let s = self.regex(e);
                    n = self.alternate(n, s);
                }
                n
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

    fn compile_regex(mut self, regex: &Regex, m: usize) -> Program {
        let n = self.regex(regex);
        let n = self.matching(n, m);
        self.build(n)
    }
}


#[derive(Debug, Clone)]
struct OpcodeEx {
    opcode: Opcode,
    goto: Option<u32>,
    split_end: Option<u32>,
}

impl OpcodeEx {
    fn new(opcode: Opcode) -> OpcodeEx {
        OpcodeEx {
            opcode: opcode,
            goto: None,
            split_end: None,
        }
    }

    fn join(&mut self, index: u32) {
        self.goto = Some(index);
    }
}

impl Deref for OpcodeEx {
    type Target = Opcode;

    fn deref(&self) -> &Self::Target {
        &self.opcode
    }
}

impl DerefMut for OpcodeEx {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.opcode
    }
}


#[derive(Debug, Clone, Copy)]
struct Proc(u32, u32);

impl Proc {
    fn is_empty(&self) -> bool {
        self.0 == EMPTY_GOTO
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
    Matched(usize),
}

#[derive(Debug)]
pub struct Machine {
    program: Rc<Program>,
    pc1: SparseSet<u32>,
    pc2: SparseSet<u32>,
}

impl Machine {
    pub fn new(program: &Rc<Program>) -> Machine {
        Machine {
            program: program.clone(),
            pc1: SparseSet::new(program.code.len()),
            pc2: SparseSet::new(program.code.len()),
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
        use self::Opcode::*;

        if !self.pc2.contains(&pc) {
            match self.program.opcode(pc) {
                Split(goto1, goto2) => {
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
        let mut i = 0;

        self.pc2.clear();

        while i < self.pc1.len() {
            let pc = self.pc1[i];
            let opcode = self.program.opcode(pc);
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
            i += 1;
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

    fn exec(&mut self, reader: &mut dyn ByteReader) -> ParseResult<Option<usize>> {
        self.restart();

        let pos = reader.position();

        while let Some(b) = reader.peek_byte(0)? {
            match self.step(b) {
                Status::Processing => {
                    reader.next_byte()?;
                }
                Status::Failed => {
                    reader.seek(pos)?;
                    return Ok(None);
                }
                Status::Matched(m) => {
                    return Ok(Some(m));
                }
            }
        }

        if let Status::Matched(m) = self.step(0xFFu8) {
            Ok(Some(m))
        } else {
            reader.seek(pos)?;
            Ok(None)
        }
    }
}


#[derive(Debug)]
pub struct Matcher {
    grammar: GrammarRef,
    machines: Vec<Machine>,
    unmatched: usize,
    mode: usize,
    stack1: Vec<usize>,
    stack2: Vec<usize>,
}

impl Matcher {
    pub fn new(grammar: &GrammarRef, unmatched: usize, mode: usize) -> Matcher {
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

        let mut machines = Vec::with_capacity(lexemes.len());

        for &lexeme in lexemes.iter() {
            let p = Rc::new(Program::from_regex(lexeme.regex(), lexeme.index()));
            machines.push(Machine::new(&p));
        }

        Matcher {
            grammar: grammar.clone(),
            machines,
            unmatched,
            mode,
            stack1: Vec::with_capacity(lexemes.len()),
            stack2: Vec::with_capacity(lexemes.len()),
        }
    }

    fn swap(&mut self) {
        std::mem::swap(&mut self.stack1, &mut self.stack2);
    }
}

impl Lexer for Matcher {
    fn reset(&mut self) {
        for m in self.machines.iter_mut() {
            m.restart();
        }
        self.stack1.clear();
        self.stack1.extend(0..self.machines.len());
    }

    fn lex(&mut self, reader: &mut dyn ByteReader) -> Result<Token, LexerError> {
        let s = reader.position();

        if reader.eof() {
            Ok(Token::with_id(0, "$".into(), "".into(), s, s))
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
                for &m in self.stack1.iter() {
                    if let Status::Matched(m) = self.machines[m].step(0xFFu8) {
                        matched = m;
                    }
                }
            }

            if matched > 0 {
                let g = self.grammar.borrow();
                let t = g.terminal(matched);
                let e = reader.position();
                let mut token = Token::with_id(t.index(), t.id().into(), reader.slice(s.offset, e.offset)?.into(), s, e);
                token.set_mode(self.mode);
                Ok(token)
            } else if self.unmatched > 0 {
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

    fn unmatched(&self) -> usize {
        self.unmatched
    }

    fn mode(&self) -> usize {
        self.mode
    }
}
