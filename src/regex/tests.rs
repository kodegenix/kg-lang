use super::*;

#[test]
fn should_parse_dot_as_any() {
    assert_eq!(Regex::Any, Regex::parse(".").unwrap());
}

#[test]
fn parse_nested_alternative() {
    let r = "abc(a|b)abc";
    assert_eq!(format!("{}", Regex::parse(r).unwrap()), r);
}

#[test]
fn parse_excluded_set() {
    let r = r"[^\n\r]";
    assert_eq!(format!("{}", Regex::parse(r).unwrap()), r);
}

#[test]
fn char_set_should_overlap() {
    let mut c = CharSet::new();
    c.add_range('0', '5');
    c.add_range('a', 'f');
    c.add_range('B', 'F');

    assert!(c.overlaps_range('1', '8'));
    assert!(c.overlaps_range('A', 'a'));
    assert!(c.overlaps_range('E', 'Z'));
}

#[test]
fn char_set_should_contain() {
    let mut c = CharSet::new();
    c.add_range('0', '5');
    c.add_range('a', 'f');
    c.add_range('B', 'F');

    assert!(c.contains_range('1', '4'));
    assert!(c.contains_char('b'));
    assert!(!c.contains_range('A', 'F'));
}

#[test]
fn char_set_exclude() {
    let mut c = CharSet::new();
    c.remove_range('1', '5');
    assert_eq!(format!("{}", c), "[^1-5]");
    assert_eq!(c.contains_char('0'), true);
    assert_eq!(c.contains_char('1'), false);
    assert_eq!(c.contains_char('2'), false);
    assert_eq!(c.contains_char('3'), false);
    assert_eq!(c.contains_char('4'), false);
    assert_eq!(c.contains_char('5'), false);
    assert_eq!(c.contains_char('6'), true);
    assert_eq!(c.contains_char('!'), true);
}

#[test]
fn char_set_remove() {
    let mut c = CharSet::new();
    c.add_range('1', '5');
    c.add_range('A', 'P');
    assert_eq!(format!("{}", c), "[1-5A-P]");
    c.remove_range('F', 'Z');
    assert_eq!(format!("{}", c), "[1-5A-E]");
}

#[test]
fn char_set_display_quoted() {
    let mut c = CharSet::new();
    c.add_char('-');
    c.add_char('\\');
    assert_eq!(format!("{}", c), r"[\-\\]");
    c.add_char('[');
    c.add_char(']');
    assert_eq!(format!("{}", c), r"[\-\[-\]]");
    c.add_char('^');
    assert_eq!(format!("{}", c), r"[\-\[-\^]");
}

#[test]
fn literal_display_quoted() {
    let r = Regex::Literal {
        chars: vec!['\\', '.', '+', '*', '?', '|', '[', ']', '(', ')', '{', '}', '-', '^'],
        icase: false,
    };
    assert_eq!(format!("{}", r), r"\\\.\+\*\?\|\[\]\(\)\{\}-^");
}

