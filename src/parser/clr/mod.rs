use super::*;

mod build;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ActionEdge {
    terminal: usize,
    action: Action,
}

impl ActionEdge {
    fn new(terminal: usize, action: Action) -> ActionEdge {
        ActionEdge {
            terminal: terminal,
            action: action,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct GotoEdge {
    production: usize,
    state: usize,
}

impl GotoEdge {
    fn new(production: usize, state: usize) -> GotoEdge {
        GotoEdge {
            production: production,
            state: state,
        }
    }
}


#[derive(Debug)]
struct State {
    index: usize,
    actions: OrdSet<ActionEdge>,
    gotos: OrdSet<GotoEdge>,
}

impl State {
    fn new(index: usize) -> State {
        State {
            index: index,
            actions: OrdSet::new(),
            gotos: OrdSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct ClrParser {
    grammar: Rc<RefCell<Grammar>>,
    states: Vec<State>,
    stack: Vec<usize>,
    channel: usize,
}

impl ClrParser {
    pub fn build(grammar: &Rc<RefCell<Grammar>>, channel: usize) -> Result<ClrParser, Error> {
        build::build(ClrParser::new(grammar, channel)).map_err(|_| Error::Unspecified(line!()))
    }

    fn new(grammar: &Rc<RefCell<Grammar>>, channel: usize) -> ClrParser {
        ClrParser {
            grammar: grammar.clone(),
            states: Vec::new(),
            stack: Vec::new(),
            channel: channel,
        }
    }

    fn current_state(&self) -> &State {
        &self.states[*self.stack.last().unwrap()]
    }

    fn find_action_edge(&self, terminal: usize) -> Option<ActionEdge> {
        self.current_state().actions.iter().cloned().find(|a| a.terminal == terminal)
    }

    fn find_goto_edge(&self, production: usize) -> Option<GotoEdge> {
        self.current_state().gotos.iter().cloned().find(|g| g.production == production)
    }
}

impl Parser for ClrParser {
    fn channel(&self) -> usize {
        self.channel
    }

    fn reset(&mut self) {
        self.stack.clear();
        self.stack.push(0);
    }

    //FIXME (jc) multi.rs handling
    fn parse(&mut self, token: &Token) -> Result<Step, ParserError> {
        if let Some(a) = self.find_action_edge(token.lexeme()) {
            match a.action {
                Action::Shift(s) => {
                    self.stack.push(s);
                    return Ok(Step::Shift);
                }
                Action::Reduce(r) => {
                    let g = self.grammar.borrow();
                    let rule = g.rule(r);
                    let n = rule.symbols().len();

                    let t = self.stack.len() - n;
                    self.stack.truncate(t);

                    if let Some(g) = self.find_goto_edge(rule.production()) {
                        self.stack.push(g.state);
                    } else {
                        unreachable!();
                    }
                    return Ok(Step::Reduce(r))
                }
                Action::Accept => {
                    self.stack.pop();
                    return Ok(Step::Accept);
                }
            }
        } else {
            println!("{:?}", token);
            unreachable!();
        }
    }
}
