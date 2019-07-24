use super::*;

pub mod parse;


#[derive(Debug, PartialEq, Eq)]
pub enum Cmd {
    Skip,
    More,
    Type(usize),
    Mode(usize),
    ModePush(usize),
    ModePop,
    Channel(usize),
    ChannelPush(usize),
    ChannelPop,
    Custom(String),
}


#[derive(Debug)]
pub struct Command {
    cmd: Cmd,
    modes: OrdSet<usize>,
}

impl Command {
    fn new(cmd: Cmd) -> Command {
        Command {
            cmd: cmd,
            modes: OrdSet::new(),
        }
    }

    pub fn has_mode(&self, mode: usize) -> bool {
        self.modes.contains(&mode)
    }

    pub fn cmd(&self) -> &Cmd {
        &self.cmd
    }

    fn add_mode(&mut self, mode: usize) {
        self.modes.insert(mode);
    }
}


pub struct Commands(Vec<Command>);

impl Commands {
    fn new() -> Commands {
        Commands(Vec::new())
    }

    pub fn iter_mode<'a>(&'a self, mode: usize) -> impl DoubleEndedIterator<Item = &'a Cmd> + 'a {
        self.0.iter().filter(move |c| c.has_mode(mode)).map(|c| c.cmd())
    }
}

impl Deref for Commands {
    type Target = Vec<Command>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Commands {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Debug for Commands {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}


#[derive(Debug)]
pub struct Lexeme {
    index: usize,
    id: String,
    modes: OrdSet<usize>,
    precedences: Vec<usize>,
    regex: Regex,
    commands: Commands,
    grammar: Weak<RefCell<Grammar>>,
}

impl Lexeme {
    fn new<S>(index: usize, id: S, regex: Regex, grammar: &Rc<RefCell<Grammar>>) -> Lexeme
        where S: Into<String>
    {
        Lexeme {
            index: index,
            id: id.into(),
            modes: OrdSet::new(),
            precedences: Vec::new(),
            regex: regex,
            commands: Commands::new(),
            grammar: Rc::downgrade(grammar),
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn modes(&self) -> &OrdSet<usize> {
        &self.modes
    }

    pub fn regex(&self) -> &Regex {
        &self.regex
    }

    pub fn commands(&self) -> &Commands {
        &self.commands
    }

    pub fn has_mode(&self, mode: usize) -> bool {
        self.modes.contains(&mode)
    }

    fn add_mode(&mut self, mode: usize) {
        if !self.has_mode(mode) {
            self.modes.insert(mode);
        }
    }

    pub fn precedence(&self, mode: usize) -> usize {
        if let Some(&p) = self.precedences.get(mode) {
            p
        } else {
            self.index
        }
    }
}

impl PartialEq for Lexeme {
    fn eq(&self, other: &Lexeme) -> bool {
        self.index == other.index
    }
}

impl Eq for Lexeme {}

impl PartialOrd for Lexeme {
    fn partial_cmp(&self, other: &Lexeme) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Lexeme {
    fn cmp(&self, other: &Lexeme) -> Ordering {
        self.index.cmp(&other.index)
    }
}


#[derive(Debug)]
pub struct Production {
    index: usize,
    id: String,
    rules: RefCell<Vec<usize>>,
    grammar: Weak<RefCell<Grammar>>,
}

impl Production {
    fn new<S>(index: usize, id: S, grammar: &Rc<RefCell<Grammar>>) -> Production
        where S: Into<String>
    {
        Production {
            index: index,
            id: id.into(),
            rules: RefCell::new(Vec::new()),
            grammar: Rc::downgrade(grammar),
        }
    }

    pub fn grammar(&self) -> Rc<RefCell<Grammar>> {
        self.grammar.upgrade().unwrap()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn rules(&self) -> Ref<Vec<usize>> {
        if self.rules.borrow().is_empty() {
            let g = self.grammar();
            self.rules.borrow_mut()
                .extend(g.borrow().rules().iter()
                    .filter(|r| r.production == self.index)
                    .map(|r| r.index));
        }
        self.rules.borrow()
    }
}

impl PartialEq for Production {
    fn eq(&self, other: &Production) -> bool {
        self.index == other.index
    }
}

impl Eq for Production {}

impl PartialOrd for Production {
    fn partial_cmp(&self, other: &Production) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Production {
    fn cmp(&self, other: &Production) -> Ordering {
        self.index.cmp(&other.index)
    }
}


#[derive(Debug)]
pub struct Rule {
    index: usize,
    id: String,
    production: usize,
    channels: Vec<usize>,
    symbols: Vec<Symbol>,
    action: String,
    grammar: Weak<RefCell<Grammar>>,
}

impl Rule {
    fn new(index: usize, production: usize, grammar: &Rc<RefCell<Grammar>>) -> Rule {
        Rule {
            index: index,
            id: format!("r{}", index),
            production: production,
            channels: Vec::new(),
            symbols: Vec::new(),
            action: String::new(),
            grammar: Rc::downgrade(&grammar),
        }
    }

    pub fn grammar(&self) -> Rc<RefCell<Grammar>> {
        self.grammar.upgrade().unwrap()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn production(&self) -> usize {
        self.production
    }

    pub fn channels(&self) -> &Vec<usize> {
        &self.channels
    }

    pub fn symbols(&self) -> &Vec<Symbol> {
        &self.symbols
    }

    pub fn action(&self) -> &str {
        &self.action
    }

    fn has_channel(&self, channel: usize) -> bool {
        //FIXME (jc) use binary search
        self.channels.contains(&channel)
    }

    fn add_channel(&mut self, channel: usize) {
        //FIXME (jc) use binary search
        if !self.has_channel(channel) {
            self.channels.push(channel);
            self.channels.sort();
        }
    }
}

impl PartialEq for Rule {
    fn eq(&self, other: &Rule) -> bool {
        self.index == other.index
    }
}

impl Eq for Rule {}

impl PartialOrd for Rule {
    fn partial_cmp(&self, other: &Rule) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Rule {
    fn cmp(&self, other: &Rule) -> Ordering {
        self.index.cmp(&other.index)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Production(usize),
    Terminal(usize),
}


#[derive(Debug)]
pub struct Mode {
    index: usize,
    id: String,
    unmatched: usize,
    terminals: RefCell<Vec<usize>>,
    grammar: Weak<RefCell<Grammar>>,
}

impl Mode {
    fn new<S>(index: usize, id: S, grammar: &Rc<RefCell<Grammar>>) -> Mode
        where S: Into<String>
    {
        Mode {
            index: index,
            id: id.into(),
            unmatched: 0,
            terminals: RefCell::new(Vec::new()),
            grammar: Rc::downgrade(grammar),
        }
    }

    pub fn grammar(&self) -> Rc<RefCell<Grammar>> {
        self.grammar.upgrade().unwrap()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn unmatched(&self) -> usize {
        self.unmatched
    }

    pub fn terminals(&self) -> Ref<Vec<usize>> {
        if self.terminals.borrow().is_empty() {
            let g = self.grammar();
            self.terminals.borrow_mut()
                .extend(g.borrow().terminals().iter()
                    .filter(|t| t.modes.iter().any(|&m| m == self.index))
                    .map(|t| t.index));
        }
        self.terminals.borrow()
    }
}

impl PartialEq for Mode {
    fn eq(&self, other: &Mode) -> bool {
        self.index == other.index
    }
}

impl Eq for Mode {}

impl PartialOrd for Mode {
    fn partial_cmp(&self, other: &Mode) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Mode {
    fn cmp(&self, other: &Mode) -> Ordering {
        self.index.cmp(&other.index)
    }
}


#[derive(Debug)]
pub struct Channel {
    index: usize,
    id: String,
    rules: RefCell<Vec<usize>>,
    grammar: Weak<RefCell<Grammar>>,
}

impl Channel {
    fn new<S>(index: usize, id: S, grammar: &Rc<RefCell<Grammar>>) -> Channel
        where S: Into<String>
    {
        Channel {
            index: index,
            id: id.into(),
            rules: RefCell::new(Vec::new()),
            grammar: Rc::downgrade(grammar),
        }
    }

    pub fn grammar(&self) -> Rc<RefCell<Grammar>> {
        self.grammar.upgrade().unwrap()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn rules(&self) -> Ref<Vec<usize>> {
        if self.rules.borrow().is_empty() {
            let g = self.grammar();
            self.rules.borrow_mut()
                .extend(g.borrow().rules().iter()
                    .filter(|r| r.channels.iter().any(|&c| c == self.index))
                    .map(|r| r.index));
        }
        self.rules.borrow()
    }
}

impl PartialEq for Channel {
    fn eq(&self, other: &Channel) -> bool {
        self.index == other.index
    }
}

impl Eq for Channel {}

impl PartialOrd for Channel {
    fn partial_cmp(&self, other: &Channel) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for Channel {
    fn cmp(&self, other: &Channel) -> Ordering {
        self.index.cmp(&other.index)
    }
}


#[derive(Debug)]
pub struct Grammar {
    modes: Vec<Mode>,
    channels: Vec<Channel>,
    terminals: Vec<Lexeme>,
    productions: Vec<Production>,
    rules: Vec<Rule>,
    globals: String,
    strict_mode: bool,
}

impl Grammar {
    fn new() -> Grammar {
        Grammar {
            modes: Vec::new(),
            channels: Vec::new(),
            terminals: Vec::new(),
            productions: Vec::new(),
            rules: Vec::new(),
            globals: String::new(),
            strict_mode: false,
        }
    }

    pub fn parse(r: &mut dyn CharReader) -> Result<Rc<RefCell<Grammar>>, parse::Error> {
        self::parse::parse(r)
    }

    pub fn modes(&self) -> &Vec<Mode> {
        &self.modes
    }

    pub fn mode(&self, index: usize) -> &Mode {
        &self.modes[index]
    }

    pub fn channels(&self) -> &Vec<Channel> {
        &self.channels
    }

    pub fn channel(&self, index: usize) -> &Channel {
        &self.channels[index]
    }

    pub fn terminals(&self) -> &Vec<Lexeme> {
        &self.terminals
    }

    pub fn terminal(&self, index: usize) -> &Lexeme {
        &self.terminals[index]
    }

    pub fn productions(&self) -> &Vec<Production> {
        &self.productions
    }

    pub fn production(&self, index: usize) -> &Production {
        &self.productions[index]
    }

    pub fn rules(&self) -> &Vec<Rule> {
        &self.rules
    }

    pub fn rule(&self, index: usize) -> &Rule {
        &self.rules[index]
    }

    pub fn globals(&self) -> &str {
        &self.globals
    }

    pub fn strict_mode(&self) -> bool {
        self.strict_mode
    }
}

impl std::fmt::Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            for r in self.rules().iter() {
                write!(f, "({:3})\t{:#} → {:#}\n",
                       r.index(),
                       SymbolDisp::production(r.production, self),
                       SymbolsDisp::symbols(&r.symbols, " ", self))?;
            }
        } else {
            for r in self.rules().iter() {
                write!(f, "{} → {}\n",
                       SymbolDisp::production(r.production, self),
                       SymbolsDisp::symbols(&r.symbols, " ", self))?;
            }
        }
        Ok(())
    }
}


pub struct SymbolDisp<'a> {
    symbol: Symbol,
    grammar: &'a Grammar,
}

impl<'a> SymbolDisp<'a> {
    pub fn symbol(symbol: Symbol, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol: symbol,
            grammar: grammar,
        }
    }

    pub fn production(production: usize, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol: Symbol::Production(production),
            grammar: grammar,
        }
    }

    pub fn terminal(terminal: usize, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol: Symbol::Terminal(terminal),
            grammar: grammar,
        }
    }
}

impl<'a> std::fmt::Display for SymbolDisp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.symbol {
            Symbol::Terminal(t) => {
                let ref t = self.grammar.terminals[t];
                if f.alternate() {
                    write!(f, "{}:{}", t.id, t.index)
                } else {
                    write!(f, "{}", t.id)
                }
            }
            Symbol::Production(p) => {
                let ref p = self.grammar.productions[p];
                if f.alternate() {
                    write!(f, "{}:{}", p.id, p.index)
                } else {
                    write!(f, "{}", p.id)
                }
            }
        }
    }
}


pub struct SymbolsDisp<'a> {
    symbols: Vec<SymbolDisp<'a>>,
    separator: &'a str,
}

impl<'a> SymbolsDisp<'a> {
    pub fn symbols(symbols: &'a [Symbol],
                   separator: &'a str,
                   grammar: &'a Grammar)
                   -> SymbolsDisp<'a> {
        SymbolsDisp {
            symbols: symbols.iter().map(|&s| SymbolDisp::symbol(s, grammar)).collect(),
            separator: separator,
        }
    }

    pub fn terminals(terminals: &'a [usize],
                     separator: &'a str,
                     grammar: &'a Grammar)
                     -> SymbolsDisp<'a> {
        SymbolsDisp {
            symbols: terminals.iter().map(|&t| SymbolDisp::terminal(t, grammar)).collect(),
            separator: separator,
        }
    }

    pub fn productions(productions: &'a [usize],
                       separator: &'a str,
                       grammar: &'a Grammar)
                       -> SymbolsDisp<'a> {
        SymbolsDisp {
            symbols: productions.iter().map(|&p| SymbolDisp::production(p, grammar)).collect(),
            separator: separator,
        }
    }
}

impl<'a> std::fmt::Display for SymbolsDisp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.symbols.is_empty() {
            write!(f, "\u{03B5}")?;
        } else {
            let mut iter = self.symbols.iter().peekable();
            while let Some(s) = iter.next() {
                if f.alternate() {
                    write!(f, "{:#}", s)?;
                } else {
                    write!(f, "{}", s)?;
                }
                if self.separator.len() > 0 && iter.peek().is_some() {
                    write!(f, "{}", self.separator)?;
                }
            }
        }
        Ok(())
    }
}
