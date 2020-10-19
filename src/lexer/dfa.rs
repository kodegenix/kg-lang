use super::*;
use std::collections::VecDeque;

/// DFA (Deterministic Finite Automaton) implementation
#[derive(Debug, Default, Clone)]
pub struct Dfa {
    states: Vec<State>,
}

impl Dfa {
    pub fn from_nfa(nfa: &Nfa) -> Dfa {
        Builder::new().build(nfa)
    }

    pub fn exec(&self, reader: &mut dyn ByteReader) -> IoResult<Option<usize>> {
        let pos = reader.position();

        let mut s = 0u32;
        let mut m = 0u32;

        while let Some(b) = reader.peek_byte(0)? {
            let ns = self.states[s as usize].edges[b as usize];
            if ns == EMPTY_GOTO {
                break;
            } else {
                s = ns;
                reader.next_byte()?;
            }
        }

        if let Some(a) = self.states[s as usize].accept {
            m = a.matching();
            Ok(Some(m as usize))
        } else {
            reader.seek(pos)?;
            Ok(None)
        }
    }
}

impl std::fmt::Display for Dfa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.states.iter().enumerate() {
            write!(f, "({:4}): {}", i, s)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct State {
    edges: [u32; 256],
    accept: Option<Accept>,
}

impl State {
    fn from(state: StateEx) -> Self {
        debug_assert!(state.accepts.len() <= 1);
        debug_assert!(state.edges.len() > 0 || state.accepts.len() == 1);
        let mut edges = [EMPTY_GOTO; 256];
        for e in state.edges {
            let value = e.value() as usize;
            let state = e.state();
            debug_assert_eq!(edges[value], EMPTY_GOTO);
            edges[value] = state;
        }
        State {
            edges,
            accept: state.accepts.get(0).cloned(),
        }
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(a) = self.accept {
            write!(f, "{}\n", a)?;
        } else {
            write!(f, "\n")?;
        }
        let mut empty = true;
        for (i, s) in self.edges.iter().cloned().enumerate() {
            if s != EMPTY_GOTO {
                write!(f, "        {:?} -> {}\n", i as u8 as char, s)?;
                empty = false;
            }
        }
        if empty {
            write!(f, "        -\n")?;
        }
        Ok(())
    }
}


#[derive(Debug)]
struct Builder {
    states: Vec<StateEx>,
}

impl Builder {
    fn new() -> Self {
        Builder {
            states: Vec::new(),
        }
    }

    fn build(mut self, nfa: &Nfa) -> Dfa {
        let mut state = StateEx::new(nfa.states().len());
        state.merged_states.insert(0);
        self.states.push(state);

        let mut stack_queue = VecDeque::new();
        stack_queue.push_back(0);

        let mut merged_states = SparseSet::with_capacity(nfa.states().len());

        while let Some(s) = stack_queue.pop_front() {
            for input in 0..255u8 { //TODO (jc) only iterate on actual alphabet of the NFA
                merged_states.clear();
                for state in self.states[s].merged_states.iter().cloned() {
                    for e in nfa.states()[state as usize].edges() {
                        if e.value() == input {
                            merged_states.insert(e.state());
                        }
                    }
                }
                if !merged_states.is_empty() {
                    let mut found = false;
                    for (i, state) in self.states.iter().enumerate() {
                        if state.merged_states == merged_states {
                            found = true;
                            self.states[s].edges.push(Edge::new(input, i as u32));
                            break;
                        }
                    }
                    if !found {
                        let i = self.states.len();
                        self.states[s].edges.push(Edge::new(input, i as u32));
                        stack_queue.push_back(i);
                        let mut nstate = StateEx {
                            merged_states: merged_states.clone(),
                            edges: Vec::new(),
                            accepts: Vec::new(),
                        };
                        for s in merged_states.iter().cloned() {
                            if let Some(a) = nfa.states()[s as usize].accept() {
                                nstate.add_accept(a);
                            }
                        }
                        self.states.push(nstate);
                    }
                }
            }
        }

        let mut dfa_states = Vec::with_capacity(self.states.len());
        for state in self.states {
            dfa_states.push(State::from(state));
        }

        Dfa {
            states: dfa_states,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::prog::Machine;

    //FIXME (jc)
    #[test]
    fn test1() {
        let re1 = Regex::parse("aba+aab").unwrap();
        let re2 = Regex::parse("amma").unwrap();
        let re3 = Regex::parse("a[0a-f]+m").unwrap();

        println!("{:?}", re1);
        println!("{:?}", re2);
        println!("{:?}", re3);

        let p1 = Program::from_regex(&re1, 1);
        let p2 = Program::from_regex(&re2, 2);
        let p3 = Program::from_regex(&re3, 3);

        let mut p = Program::merge(&[p1]);
        println!("--- prog\n{}", p);

        let nfa = Nfa::from_program(&p);
        println!("--- nfa\n{}", nfa);
        let dfa = Dfa::from_nfa(&nfa);
        println!("--- dfa\n{}", dfa);

        let s = "abaaaaab abc abb amma aaaddddmm";
        let mut r = MemByteReader::new(s.as_bytes());
        while !r.eof() {
            let p1 = r.position();
            match dfa.exec(&mut r).unwrap() {
                Some(m) => {
                    let p2 = r.position();
                    println!("match: {} {:?}", m, r.slice_pos(p1, p2).unwrap());
                }
                None => {
                    println!("err: {:?}", r.next_byte().unwrap().unwrap() as char);
                }
            }
        }
    }

    //FIXME (jc)
    #[test]
    fn test2() {
        let s = "/*aaa*/aaaabbbb/*bbb*/zz";
        let re1 = Regex::parse("/\\*[/*ab]*\\*/").unwrap();

        println!("{:?}", re1);

        let p1 = Program::from_regex(&re1, 1);

        let mut p = Program::merge(&[p1]);
        println!("--- prog\n{}", p);
        let nfa = Nfa::from_program(&p);
        println!("--- nfa\n{}", nfa);
        let dfa = Dfa::from_nfa(&nfa);
        println!("--- dfa\n{}", dfa);

        let mut mach = Machine::new(p);
        let mut r = MemByteReader::new(s.as_bytes());
        println!("\nprog matching:");
        while !r.eof() {
            let p1 = r.position();
            match mach.exec(&mut r).unwrap() {
                Some(m) => {
                    let p2 = r.position();
                    println!("match: {} {:?}", m, r.slice_pos(p1, p2).unwrap());
                }
                None => {
                    println!("err: {:?}", r.next_byte().unwrap().unwrap() as char);
                }
            }
        }

        println!("\ndfa matching:");
        let mut r = MemByteReader::new(s.as_bytes());
        while !r.eof() {
            let p1 = r.position();
            match dfa.exec(&mut r).unwrap() {
                Some(m) => {
                    let p2 = r.position();
                    println!("match: {} {:?}", m, r.slice_pos(p1, p2).unwrap());
                }
                None => {
                    println!("err: {:?}", r.next_byte().unwrap().unwrap() as char);
                }
            }
        }
    }
}
