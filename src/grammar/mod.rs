use super::*;

pub mod parse;


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


pub (crate) type GrammarWeakRef = Weak<RefCell<Grammar>>;

#[derive(Debug, Clone)]
pub struct GrammarRef(Rc<RefCell<Grammar>>);

impl GrammarRef {
    fn new() -> Self {
        GrammarRef(Rc::new(RefCell::new(Grammar::new())))
    }

    fn weak_ref(&self) -> GrammarWeakRef {
        Rc::downgrade(&self.0)
    }

    fn from_weak_ref(grammar: &GrammarWeakRef) -> Self {
        GrammarRef(grammar.upgrade().unwrap())
    }

    pub fn parse(r: &mut dyn CharReader) -> Result<Self, parse::Error> {
        self::parse::parse(r)
    }

    pub fn borrow(&self) -> Ref<Grammar> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<Grammar> {
        self.0.borrow_mut()
    }
}


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
            cmd,
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
    grammar: GrammarWeakRef,
}

impl Lexeme {
    fn new<S>(index: usize, id: S, regex: Regex, grammar: &GrammarRef) -> Lexeme
        where S: Into<String>
    {
        Lexeme {
            index,
            id: id.into(),
            modes: OrdSet::new(),
            precedences: Vec::new(),
            regex,
            commands: Commands::new(),
            grammar: grammar.weak_ref(),
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
    grammar: GrammarWeakRef,
}

impl Production {
    fn new<S>(index: usize, id: S, grammar: &GrammarRef) -> Production
        where S: Into<String>
    {
        Production {
            index,
            id: id.into(),
            rules: RefCell::new(Vec::new()),
            grammar: grammar.weak_ref(),
        }
    }

    pub fn grammar(&self) -> GrammarRef {
        GrammarRef::from_weak_ref(&self.grammar)
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
    grammar: GrammarWeakRef,
}

impl Rule {
    fn new(index: usize, production: usize, grammar: &GrammarRef) -> Rule {
        Rule {
            index,
            id: format!("r{}", index),
            production,
            channels: Vec::new(),
            symbols: Vec::new(),
            action: String::new(),
            grammar: grammar.weak_ref(),
        }
    }

    pub fn grammar(&self) -> GrammarRef {
        GrammarRef::from_weak_ref(&self.grammar)
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
        self.channels.contains(&channel)
    }

    fn add_channel(&mut self, channel: usize) {
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
    grammar: GrammarWeakRef,
}

impl Mode {
    fn new<S>(index: usize, id: S, grammar: &GrammarRef) -> Mode
        where S: Into<String>
    {
        Mode {
            index,
            id: id.into(),
            unmatched: 0,
            terminals: RefCell::new(Vec::new()),
            grammar: grammar.weak_ref(),
        }
    }

    pub fn grammar(&self) -> GrammarRef {
        GrammarRef::from_weak_ref(&self.grammar)
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
    grammar: GrammarWeakRef,
}

impl Channel {
    fn new<S>(index: usize, id: S, grammar: &GrammarRef) -> Channel
        where S: Into<String>
    {
        Channel {
            index,
            id: id.into(),
            rules: RefCell::new(Vec::new()),
            grammar: grammar.weak_ref(),
        }
    }

    pub fn grammar(&self) -> GrammarRef {
        GrammarRef::from_weak_ref(&self.grammar)
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



pub struct SymbolDisp<'a> {
    symbol: Symbol,
    grammar: &'a Grammar,
}

impl<'a> SymbolDisp<'a> {
    pub fn symbol(symbol: Symbol, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol,
            grammar,
        }
    }

    pub fn production(production: usize, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol: Symbol::Production(production),
            grammar,
        }
    }

    pub fn terminal(terminal: usize, grammar: &'a Grammar) -> SymbolDisp {
        SymbolDisp {
            symbol: Symbol::Terminal(terminal),
            grammar,
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
            separator,
        }
    }

    pub fn terminals(terminals: &'a [usize],
                     separator: &'a str,
                     grammar: &'a Grammar)
                     -> SymbolsDisp<'a> {
        SymbolsDisp {
            symbols: terminals.iter().map(|&t| SymbolDisp::terminal(t, grammar)).collect(),
            separator,
        }
    }

    pub fn productions(productions: &'a [usize],
                       separator: &'a str,
                       grammar: &'a Grammar)
                       -> SymbolsDisp<'a> {
        SymbolsDisp {
            symbols: productions.iter().map(|&p| SymbolDisp::production(p, grammar)).collect(),
            separator,
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
