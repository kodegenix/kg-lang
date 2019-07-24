use std::fmt;
use std::fmt::Write;
use std::iter::Chain;
use std::slice::Iter;
use std::cell::{Cell, RefCell};
use std::ops::{Deref, DerefMut};
use std::clone::Clone;
use std::cmp::{PartialOrd, Ord, PartialEq, Eq, Ordering};
use log::Level;

use super::*;
use kg_utils::collections::OrdSet;
use grammar::{Grammar, Production, Symbol, SymbolDisp, SymbolsDisp};

//FIXME (jc) handle left and right associativity, e.g. -2-3 (negation vs subtraction)

//FIXME (jc) errors and error handling
#[derive(Debug)]
pub enum Error {
    Unspecified(u32),
}

impl From<fmt::Error> for Error {
    fn from(_: fmt::Error) -> Error {
        Error::Unspecified(line!())
    }
}


#[derive(Debug)]
struct ProductionExt<'a> {
    production: &'a Production,
    first: RefCell<OrdSet<usize>>,
    follow: RefCell<OrdSet<usize>>,
    nullable: Cell<bool>,
}

impl<'a> ProductionExt<'a> {
    fn new(p: &'a Production) -> ProductionExt<'a> {
        ProductionExt {
            production: p,
            first: RefCell::new(OrdSet::new()),
            follow: RefCell::new(OrdSet::new()),
            nullable: Cell::new(false),
        }
    }
}

impl<'a> PartialEq for ProductionExt<'a> {
    fn eq(&self, other: &ProductionExt<'a>) -> bool {
        self.production == other.production
    }
}

impl<'a> Eq for ProductionExt<'a> {}

impl<'a> PartialOrd for ProductionExt<'a> {
    fn partial_cmp(&self, other: &ProductionExt<'a>) -> Option<Ordering> {
        self.production.partial_cmp(&other.production)
    }
}

impl<'a> Ord for ProductionExt<'a> {
    fn cmp(&self, other: &ProductionExt<'a>) -> Ordering {
        self.production.cmp(&other.production)
    }
}

impl<'a> Deref for ProductionExt<'a> {
    type Target = Production;

    fn deref(&self) -> &Production {
        self.production
    }
}


#[derive(Debug)]
struct GrammarExt<'a> {
    grammar: &'a Grammar,
    productions: Vec<ProductionExt<'a>>,
}

impl<'a> GrammarExt<'a> {
    fn new(g: &'a Grammar) -> GrammarExt<'a> {
        let mut g = GrammarExt {
            grammar: g,
            productions: g.productions().iter().map(|p| ProductionExt::new(p)).collect(),
        };
        g.build_first_follow_nullable();
        g
    }

    fn production(&'a self, production: usize) -> &'a ProductionExt<'a> {
        &self.productions[production]
    }

    fn build_first_follow_nullable(&mut self) {
        loop {
            let mut changed = false;

            for rule in self.grammar.rules().iter() {
                let production = self.production(rule.production());
                let mut nullable = true;

                let mut prev_p = 0;
                for s in rule.symbols().iter() {
                    match *s {
                        Symbol::Terminal(t) => {
                            if nullable {
                                changed |= production.first.borrow_mut().insert(t);
                            }
                            nullable = false;
                            if prev_p > 0 {
                                changed |= self.production(prev_p).follow.borrow_mut().insert(t);
                            }
                            prev_p = 0;
                        }
                        Symbol::Production(p) => {
                            let prod = self.production(p);
                            if nullable {
                                if production != prod {
                                    changed |= production.first.borrow_mut().append(prod.first.borrow().iter().cloned());
                                }
                                if !prod.nullable.get() {
                                    nullable = false;
                                }
                            }
                            if prev_p > 0 {
                                let prev_prod = self.production(prev_p);
                                changed |= prev_prod.follow.borrow_mut().append(prod.first.borrow().iter().cloned());
                            }
                            prev_p = p;
                        }
                    }
                }

                let mut next_p = rule.production();
                for s in rule.symbols().iter().rev() {
                    match *s {
                        Symbol::Terminal(_) => {
                            next_p = 0;
                        }
                        Symbol::Production(p) => {
                            let prod = self.production(p);
                            if next_p > 0 && next_p != p {
                                let next_prod = self.production(next_p);
                                changed |= prod.follow.borrow_mut().append(next_prod.follow.borrow().iter().cloned());
                            }
                            if prod.nullable.get() {
                                next_p = p;
                            } else {
                                next_p = 0;
                            }
                        }
                    }
                }

                if nullable && !production.nullable.get() {
                    production.nullable.set(true);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }
    }
}

impl<'a> Deref for GrammarExt<'a> {
    type Target = Grammar;

    fn deref(&self) -> &Grammar {
        self.grammar
    }
}

impl<'a> fmt::Display for GrammarExt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}\n", self.grammar)?;
            for p in self.productions.iter() {
                write!(f, "FIRST ({:#}):\t{{ {:#} }}\n",
                       SymbolDisp::production(p.index(), self),
                       SymbolsDisp::terminals(&p.first.borrow(), ", ", self))?;
            }
            write!(f, "\n")?;
            for p in self.productions.iter() {
                write!(f, "FOLLOW ({:#}):\t{{ {:#} }}\n",
                       SymbolDisp::production(p.index(), self),
                       SymbolsDisp::terminals(&p.follow.borrow(), ", ", self))?;
            }
            write!(f, "\n")?;
            for p in self.productions.iter() {
                write!(f, "nullable ({:#}):\t{:#}\n",
                       SymbolDisp::production(p.index(), self),
                       p.nullable.get())?;
            }
        } else {
            write!(f, "{}\n", self.grammar)?;
            for p in self.productions.iter() {
                write!(f, "FIRST ({}):\t{{ {} }}\n",
                       SymbolDisp::production(p.index(), self),
                       SymbolsDisp::terminals(&p.first.borrow(), ", ", self))?;
            }
            write!(f, "\n")?;
            for p in self.productions.iter() {
                write!(f, "FOLLOW ({}):\t{{ {} }}\n",
                       SymbolDisp::production(p.index(), self),
                       SymbolsDisp::terminals(&p.follow.borrow(), ", ", self))?;
            }
            write!(f, "\n")?;
            for p in self.productions.iter() {
                write!(f, "nullable ({}):\t{}\n",
                       SymbolDisp::production(p.index(), self),
                       p.nullable.get())?;
            }
        }
        Ok(())
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Item {
    rule: usize,
    position: usize,
    lookaheads: OrdSet<usize>,
}

impl Item {
    fn new(rule: usize, position: usize, grammar: &GrammarExt) -> Item {
        let rule = grammar.grammar.rule(rule);
        if rule.symbols().len() == position {
            Item {
                rule: rule.index(),
                position: position,
                lookaheads: grammar.production(rule.production()).follow.borrow().clone(),
            }
        } else {
            Item {
                rule: rule.index(),
                position: position,
                lookaheads: OrdSet::new(),
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
struct ItemSet {
    symbol: Symbol,
    kernel: OrdSet<Item>,
    closure: OrdSet<Item>,
}

impl ItemSet {
    fn new(symbol: Symbol) -> ItemSet {
        ItemSet {
            symbol: symbol,
            kernel: OrdSet::with_capacity(128),
            closure: OrdSet::with_capacity(128),
        }
    }

    fn first(production: &ProductionExt, grammar: &GrammarExt) -> ItemSet {
        let mut is = ItemSet::new(Symbol::Production(production.index()));
        for r in production.rules().iter() {
            is.add_item(Item::new(*r, 0, grammar), true);
        }
        ItemSet::closure(&is, grammar)
    }

    fn closure(itemset: &ItemSet, grammar: &GrammarExt) -> ItemSet {
        let mut is = itemset.clone();
        for item in itemset.iter() {
            let rule = grammar.rule(item.rule);
            if let Some(&Symbol::Production(p)) = rule.symbols().get(item.position) {
                if Symbol::Production(p) != is.symbol {
                    let production = grammar.production(p);
                    for r in production.rules().iter() {
                        is.add_item(Item::new(*r, 0, grammar), false);
                    }
                }
            }
        }
        if itemset.len() == is.len() {
            is
        } else {
            ItemSet::closure(&is, grammar)
        }
    }

    fn next(&self, symbol: Symbol, grammar: &GrammarExt) -> Option<ItemSet> {
        let mut itemset = ItemSet::new(symbol);
        for item in self.iter() {
            let rule = grammar.rule(item.rule);
            if let Some(s) = rule.symbols().get(item.position) {
                if *s == symbol {
                    itemset.add_item(Item::new(item.rule, item.position + 1, grammar), true);
                }
            }
        }
        if itemset.len() == 0 {
            None
        } else {
            Some(ItemSet::closure(&itemset, grammar))
        }
    }

    fn add_item(&mut self, item: Item, kernel: bool) -> bool {
        if self.contains(&item) {
            false
        } else if kernel {
            self.kernel.insert(item)
        } else {
            self.closure.insert(item)
        }
    }

    fn len(&self) -> usize {
        self.kernel.len() + self.closure.len()
    }

    fn contains(&self, item: &Item) -> bool {
        self.kernel.contains(item) || self.closure.contains(item)
    }

    fn iter(&self) -> Chain<Iter<Item>, Iter<Item>> {
        self.kernel.iter().chain(self.closure.iter())
    }
}


pub fn build(mut parser: ClrParser) -> Result<ClrParser, Error> {
    {
        let g = parser.grammar.borrow();
        let grammar = GrammarExt::new(&g);

        let mut states: Vec<RefCell<State>> = Vec::with_capacity(1024);
        states.push(RefCell::new(State::new(0)));

        let mut itemsets: Vec<ItemSet> = Vec::with_capacity(1024);
        itemsets.push(ItemSet::first(grammar.production(0), &grammar));

        fn build_transitions(s1: &mut State, s2: &State, symbol: Symbol) {
            match symbol {
                Symbol::Production(p) => {
                    s1.gotos.insert(GotoEdge::new(p, s2.index));
                }
                Symbol::Terminal(t) => {
                    s1.actions.insert(ActionEdge::new(t, Action::Shift(s2.index)));
                }
            }
        }

        let mut i = 0;

        while i < itemsets.len() {
            for term in grammar.terminals().iter() {
                if let Some(is) = itemsets[i].next(Symbol::Terminal(term.index()), &grammar) {
                    if let Some(j) = itemsets.iter().position(|e| e.kernel.eq(&is.kernel)) {
                        if i != j {
                            build_transitions(states[i].borrow_mut().deref_mut(),
                                              states[j].borrow().deref(),
                                              is.symbol);
                        }
                    } else {
                        let state = State::new(states.len());
                        build_transitions(states[i].borrow_mut().deref_mut(),
                                          &state,
                                          is.symbol);
                        states.push(RefCell::new(state));
                        itemsets.push(is);
                    }
                }
            }
            for prod in grammar.productions().iter().skip(1) {
                if let Some(is) = itemsets[i].next(Symbol::Production(prod.index()), &grammar) {
                    if let Some(j) = itemsets.iter().position(|e| e.kernel.eq(&is.kernel)) {
                        if i != j {
                            build_transitions(states[i].borrow_mut().deref_mut(),
                                              states[j].borrow().deref(),
                                              is.symbol);
                        }
                    } else {
                        let state = State::new(states.len());
                        build_transitions(states[i].borrow_mut().deref_mut(),
                                          &state,
                                          is.symbol);
                        states.push(RefCell::new(state));
                        itemsets.push(is);
                    }
                }
            }

            let ref mut state = states[i].borrow_mut();
            let ref itemset = itemsets[i];
            for item in itemset.iter() {
                let rule = grammar.rule(item.rule);
                if item.position == rule.symbols().len() {
                    if item.rule == 0 {
                        // cannot have any other actions - this is accepting state for starting rule '@'
                        if state.actions.len() != 0 {
                            return Err(Error::Unspecified(line!()));
                        }
                        state.actions.insert(ActionEdge::new(0, Action::Accept)); //can only end with $ token
                    } else {
                        for &lt in item.lookaheads.iter() {
                            let conflicts: Vec<ActionEdge> = state.actions
                                .iter()
                                .filter_map(|a| if a.terminal == lt {
                                    Some(*a)
                                } else {
                                    None
                                })
                                .collect();

                            if conflicts.is_empty() {
                                state.actions.insert(ActionEdge::new(lt, Action::Reduce(item.rule)));
                            } else {
                                for c in conflicts.iter() {
                                    match c.action {
                                        Action::Shift(s) => {
                                            trace!("conflict in state {} -> shift {} / reduce {}",
                                                   state.index,
                                                   s,
                                                   item.rule);
                                            let mut reduce = false;
                                            if rule.symbols().len() > 0 && item.position > 1 {
                                                if let Symbol::Terminal(t) =
                                                rule.symbols()[item.position - 2] {
                                                    if lt < t {
                                                        state.actions.retain(|a| a.terminal != lt);
                                                        state.actions.insert(ActionEdge::new(lt, Action::Reduce(item.rule)));
                                                        trace!("choosing reduce {}", item.rule);
                                                        reduce = true;
                                                    }
                                                }
                                            }
                                            if !reduce {
                                                trace!("choosing shift {}", s);
                                            }
                                        }
                                        Action::Reduce(r) => {
                                            trace!("conflict in state {} -> reduce {} / reduce {}",
                                                     state.index,
                                                     r,
                                                     item.rule);
                                        }
                                        Action::Accept => {
                                            return Err(Error::Unspecified(line!()));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            i += 1;
        }


        if log_enabled!(Level::Trace) {
            let mut s = String::new();
            write_generation_report(&mut s, &grammar, &itemsets, &states).unwrap();
            trace!("parser generation details\n{}", s);
        }

        parser.states.reserve(states.len());
        for state in states.drain(..) {
            parser.states.push(state.into_inner());
        }
        parser.states.shrink_to_fit();
    }

    Ok(parser)
}


fn write_generation_report(w: &mut dyn Write, grammar: &GrammarExt, itemsets: &Vec<ItemSet>, states: &Vec<RefCell<State>>) -> fmt::Result {
    write!(w, "grammar:\n{}", grammar)?;

    write!(w, "\nitemsets:\n")?;
    for (index, itemset) in itemsets.iter().enumerate() {
        write!(w, "{}: {}\n{}",
               index,
               SymbolDisp::symbol(itemset.symbol, &grammar),
               ItemSetDisp::new(itemset, &grammar))?;
    }

    write!(w, "\nstates:\n")?;
    write!(w, "{:8}", "#")?;
    for t in grammar.terminals().iter() {
        write!(w, "{:8}", format!("{}", SymbolDisp::terminal(t.index(), &grammar)))?;
    }
    for p in grammar.productions().iter().skip(1) {
        write!(w, "{:8}", format!("{}", SymbolDisp::production(p.index(), &grammar)))?;
    }
    write!(w, "\n{0:-<1$}\n", "", (grammar.terminals().len() + grammar.productions().len()) * 8)?;
    for state in states.iter() {
        write!(w, "{}\n", StateDisp::new(&state.borrow(), &grammar))?;
    }
    write!(w, "\n")?;
    Ok(())
}


struct StateDisp<'a> {
    state: &'a State,
    grammar: &'a Grammar,
}

impl<'a> StateDisp<'a> {
    fn new(state: &'a State, grammar: &'a Grammar) -> StateDisp<'a> {
        StateDisp {
            state: state,
            grammar: grammar,
        }
    }
}

impl<'a> fmt::Display for StateDisp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:<8}", self.state.index)?;
        for t in self.grammar.terminals().iter() {
            let actions: Vec<&ActionEdge> = self.state
                                                .actions
                                                .iter()
                                                .filter(|a| a.terminal == t.index())
                                                .collect();
            if actions.is_empty() {
                write!(f, "{:8}", "-")?;
            } else {
                let mut iter = actions.iter().peekable();
                let mut astr = String::new();
                while let Some(a) = iter.next() {
                    match a.action {
                        Action::Accept => write!(astr, "A")?,
                        Action::Shift(s) => write!(astr, "s{}", s)?,
                        Action::Reduce(r) => write!(astr, "r{}", r)?,
                    }
                    if iter.peek().is_some() {
                        write!(astr, "/")?;
                    }
                }
                write!(f, "{:8}", astr)?;
            }
        }
        for p in self.grammar.productions().iter().skip(1) {
            let gotos: Vec<&GotoEdge> = self.state
                                            .gotos
                                            .iter()
                                            .filter(|g| g.production == p.index())
                                            .collect();
            if gotos.is_empty() {
                write!(f, "{:<8}", "-")?;
            } else {
                let mut iter = gotos.iter().peekable();
                let mut gstr = String::new();
                while let Some(g) = iter.next() {
                    write!(gstr, "g{}", g.state)?;
                    if iter.peek().is_some() {
                        write!(gstr, "/")?;
                    }
                }
                write!(f, "{:<8}", gstr)?;
            }
        }
        Ok(())
    }
}


struct ItemDisp<'a> {
    item: &'a Item,
    grammar: &'a Grammar,
    kernel: bool,
}


impl<'a> ItemDisp<'a> {
    fn new(item: &'a Item, grammar: &'a Grammar, kernel: bool) -> ItemDisp<'a> {
        ItemDisp {
            item: item,
            grammar: grammar,
            kernel: kernel,
        }
    }
}

impl<'a> fmt::Display for ItemDisp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rule = self.grammar.rule(self.item.rule);
        if self.kernel {
            write!(f, "* ")?;
        } else {
            write!(f, "  ")?;
        }
        write!(f, "{:3} {} =",
               self.item.rule,
               SymbolDisp::production(rule.production(), self.grammar))?;
        if rule.symbols().is_empty() {
            write!(f, " \u{03B5}")?;
        } else {
            for (i, s) in rule.symbols().iter().enumerate() {
                if i == self.item.position {
                    write!(f, " • ")?;
                } else {
                    write!(f, "   ")?;
                }
                write!(f, "{}", SymbolDisp::symbol(*s, self.grammar))?;
            }
        }
        if self.item.position == rule.symbols().len() {
            write!(f, " •")?;
        }
        if !self.item.lookaheads.is_empty() {
            write!(f, "   -> {{ {} }}",
                   SymbolsDisp::terminals(&self.item.lookaheads, ", ", self.grammar))?;
        }
        Ok(())
    }
}


struct ItemSetDisp<'a> {
    items: Vec<ItemDisp<'a>>,
}

impl<'a> ItemSetDisp<'a> {
    fn new(itemset: &'a ItemSet, grammar: &'a Grammar) -> ItemSetDisp<'a> {
        let mut items: Vec<ItemDisp<'a>> = Vec::with_capacity(itemset.len());
        items.extend(itemset.kernel.iter().map(|i| ItemDisp::new(i, grammar, true)));
        items.extend(itemset.closure.iter().map(|i| ItemDisp::new(i, grammar, false)));
        items.sort_by(|a, b| a.item.cmp(&b.item));

        ItemSetDisp { items: items }
    }
}

impl<'a> fmt::Display for ItemSetDisp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in self.items.iter() {
            write!(f, "{}\n", item)?;
        }
        Ok(())
    }
}
