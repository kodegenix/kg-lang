use super::*;

/// NFA (Non-deterministic Finite Automaton) without null moves implementation
#[derive(Debug, Clone)]
pub struct Nfa {
    states: Vec<State>,
}

impl Nfa {
    pub fn from_program(prog: &Program) -> Nfa {
        Builder::new().build(prog)
    }

    pub fn states(&self) -> &[State] {
        &self.states
    }
}

impl std::fmt::Display for Nfa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.states.iter().enumerate() {
            write!(f, "({:4}): {}", i, s)?;
        }
        Ok(())
    }
}


#[derive(Debug, Clone)]
pub struct State {
    edges: Vec<Edge>,
    accept: Option<Accept>,
}

impl State {
    fn from(state: StateEx) -> State {
        debug_assert!(state.accepts.len() <= 1);
        debug_assert!(state.edges.len() > 0 || state.accepts.len() == 1);
        State {
            edges: state.edges,
            accept: state.accepts.get(0).cloned(),
        }
    }

    pub fn edges(&self) -> &[Edge] {
        &self.edges
    }

    pub fn accept(&self) -> Option<Accept> {
        self.accept
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(a) = self.accept {
            write!(f, "{}\n", a)?;
        } else {
            write!(f, "\n")?;
        }
        if self.edges.is_empty() {
            write!(f, "        -\n")?;
        } else {
            for e in self.edges.iter() {
                write!(f, "        {}\n", e)?;
            }
        }
        Ok(())
    }
}


struct Builder {
    states: Vec<StateEx>,
}

impl Builder {
    fn new() -> Builder {
        Builder {
            states: Vec::new(),
        }
    }

    fn merge_epsilon_states(&mut self, prog: &Program) {
        fn resolve_edges(pc: u32, edges: &mut SparseSet<u32>, prog: &Program, level: usize) {
            debug_assert!(level < prog.code().len());
            if let Opcode::Split(g1, g2) = *prog.opcode(pc) {
                resolve_edges(g1, edges, prog, level + 1);
                resolve_edges(g2, edges, prog, level + 1);
            } else {
                edges.insert(pc);
            }
        }

        fn add_code(state: &mut StateEx, pc: u32, edges: &HashMap<u32, SparseSet<u32>>, prog: &Program, nested: bool) {
            debug_assert!(!state.merged_states.contains(&pc));
            match *prog.opcode(pc) {
                Opcode::Match(m) => {
                    state.accepts.push(Accept::new(pc, m));
                    state.merged_states.insert(pc);
                }
                Opcode::Byte(g, b) => {
                    state.edges.push(Edge::new(b, g));
                    state.merged_states.insert(pc);
                }
                Opcode::Range(g, a, b) => {
                    for i in a..=b {
                        state.edges.push(Edge::new(i, g));
                    }
                    state.merged_states.insert(pc);
                }
                Opcode::Mask(g, m) => {
                    let m = prog.mask(m);
                    for i in m.iter() {
                        state.edges.push(Edge::new(i, g));
                    }
                    state.merged_states.insert(pc);
                }
                Opcode::Split(..) => {
                    debug_assert!(!nested);
                    for s in edges.get(&pc).unwrap().iter().cloned() {
                        add_code(state, s, edges, prog, true);
                    }
                }
            }
        }

        // resolve epsilon edges
        let mut edge_map: HashMap<u32, SparseSet<u32>> = HashMap::with_capacity(prog.code().len());
        for pc in 0..prog.code().len() as u32 {
            if let Opcode::Split(..) = prog.opcode(pc) {
                let mut e = SparseSet::with_capacity(prog.code().len());
                resolve_edges(pc, &mut e, prog, 0);
                edge_map.insert(pc, e);
            }
        }

        // create merged NFA states (without empty moves)
        let mut state_map = vec![EMPTY_GOTO; prog.code().len()];
        let mut states: Vec<StateEx> = Vec::with_capacity(prog.code().len());

        let mut pc = 0;
        let mut done = false;
        while !done {
            let mut state = StateEx::new(prog.code().len());
            add_code(&mut state, pc, &edge_map, prog, false);
            // TODO (jc) to improve performance, use hashset on states
            let mut found = false;
            for (i, s) in states.iter().enumerate() {
                if *s == state {
                    state_map[pc as usize] = i as u32;
                    found = true;
                    break;
                }
            }
            if !found {
                state_map[pc as usize] = states.len() as u32;
                states.push(state);
            }

            done = true;
            'search_loop:
            for s in states.iter() {
                for e in s.edges.iter().cloned() {
                    if state_map[e.state() as usize] == EMPTY_GOTO {
                        pc = e.state();
                        done = false;
                        break 'search_loop;
                    }
                }
            }
        }

        // map code transitions to NFA edges
        for s in states.iter_mut() {
            for e in s.edges.iter_mut() {
                let t = state_map[e.state() as usize];
                debug_assert_ne!(t, EMPTY_GOTO);
                *e = Edge::new(e.value(), t);
            }
        }

        self.states = states;
    }

    fn build(mut self, prog: &Program) -> Nfa {
        self.merge_epsilon_states(prog);

        let mut states = Vec::with_capacity(self.states.len());
        for state in self.states {
            debug_assert!(state.accepts.len() <= 1);
            states.push(State::from(state));
        }
        Nfa {
            states
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    //FIXME (jc)
    #[test]
    fn test1() {
        let re = Regex::parse("aa*bb+|bb").unwrap();
        let p = Program::from_regex(&re, 1);

        println!("{}", p);

        println!("{}", Nfa::from_program(&p));
    }
}
