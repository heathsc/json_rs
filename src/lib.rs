/*
 * This is a Native rust fast JSON parse inspired by the the excellent JSMN C library by Serge Zaitsev
 * (https://github.com/zserge/jsmn)
 *
 * This is not intended to be a clone of the original JSMN library and has several philosophical differences
 * Major differences:
 *   - Tokens are stored in a resizeable Rust array
 *   - strict parsing is always enforced (intended to be fully compliant)
 *   - handle UTF8 encoded input
 *
 * The output tokens are similar to those output from JSMN with the following differences
 *   - The start and end parameters form a closed range for all apart from Strings which use an open range (to allow for an empty string)
 *   - JSON primitives are split into Numbers, Bool and Null
 *   - Parsing of numbers strictly follows the JSON standard
 *   - true / false / null are checked in their entirety
 *   - more basic grammar checking is performed
 */

use std::{iter, str, fmt};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JType {
    Object,
    Array,
    String,
    Number,
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum JErr {
    Invalid(usize, char),
    EndOfInput,
}

impl fmt::Display for JErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EndOfInput => write!(f, "JSON incomplete - end of input reached"),
            Self::Invalid(i, c) => write!(f, "JSON error found at character {c} position {i}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum JState {
    Init,
    StartObject,
    StartArray,
    AfterComma,
    AfterColon,
    AfterValue,
    AfterKey,
}

#[derive(Debug)]
pub struct JTok {
    pub tok_type: JType,
    pub start: usize, // Start position in input array
    pub end: usize,   // End position in input array
    pub size: usize,  // Number of items in object / array
}

pub struct JParse<'a> {
    text: &'a str,     // input slice
    tokens: Vec<JTok>, // token array
    stack: Vec<(usize, JState)>,
    iter: iter::Peekable<str::CharIndices<'a>>,
    state: JState,
}

impl<'a> JParse<'a> {
    pub fn new<S: AsRef<str> + ?Sized>(text: &'a S) -> Self {
        let text = text.as_ref();
        Self {
            text,
            tokens: Vec::new(),
            stack: Vec::new(),
            iter: text.char_indices().peekable(),
            state: JState::Init,
        }
    }
    pub fn tokens(&self) -> &[JTok] {
        &self.tokens
    }
    pub fn n_tokens(&self) -> usize {
        self.tokens.len()
    }
    pub fn value_ok(&self, i: usize, c: char) -> Result<(), JErr> {
        if self.state == JState::StartObject
            || self.state == JState::AfterValue
            || self.state == JState::AfterKey
        {
            Err(JErr::Invalid(i, c))
        } else {
            Ok(())
        }
    }
    fn parse_string(&mut self) -> Result<(), JErr> {
        let mut start = None;
        while let Some((i, c)) = self.iter.next() {
            if start.is_none() {
                start = Some(i)
            }
            match c {
                '\"' => {
                    self.tokens.push(JTok {
                        tok_type: JType::String,
                        start: start.unwrap(),
                        end: i,
                        size: 0,
                    });
                    return Ok(());
                }
                '\\' => {
                    if let Some((i, c)) = self.iter.next() {
                        match c {
                            '\"' | '/' | '\\' | 'b' | 'f' | 'r' | 'n' | 't' => (),
                            // Check for \uXXXX
                            'u' => {
                                for _ in 0..4 {
                                    if let Some((i, c)) = self.iter.next() {
                                        match c {
                                            '0'..='9' | 'A'..='F' | 'a'..='f' => (),
                                            _ => return Err(JErr::Invalid(i, c)),
                                        }
                                    } else {
                                        break;
                                    }
                                }
                            }
                            _ => return Err(JErr::Invalid(i, c)),
                        }
                    } else {
                        break;
                    }
                }
                '\0'..='\x1F' => return Err(JErr::Invalid(i, c)),
                _ => (),
            }
        }
        Err(JErr::EndOfInput)
    }

    fn parse_number(&mut self, mut last: usize, c: char) -> Result<usize, JErr> {
        // Handle initial character
        let mut state = match c {
            '0' => 0,
            '-' => 1,
            '1'..='9' => 2,
            _ => panic!("Unexpected character"),
        };
        while let Some((i, c)) = self.iter.peek() {
            match state {
                0 => match c {
                    '.' => state = 3,
                    'e' | 'E' => state = 4,
                    _ => return Ok(last),
                },
                1 => match c {
                    '0' => state = 0,
                    '1'..='9' => state = 2,
                    _ => return Err(JErr::Invalid(*i, *c)),
                },
                2 => match c {
                    '0'..='9' => (),
                    '.' => state = 3,
                    'e' | 'E' => state = 4,
                    _ => return Ok(last),
                },
                3 => match c {
                    '0'..='9' => state = 5,
                    _ => return Err(JErr::Invalid(*i, *c)),
                },
                4 => match c {
                    '0'..='9' => state = 6,
                    '+' | '-' => state = 7,
                    _ => return Err(JErr::Invalid(*i, *c)),
                },
                5 => match c {
                    '0'..='9' => (),
                    'e' | 'E' => state = 4,
                    _ => return Ok(last),
                },
                6 => match c {
                    '0'..='9' => (),
                    _ => return Ok(last),
                },
                7 => match c {
                    '0'..='9' => state = 6,
                    _ => return Err(JErr::Invalid(*i, *c)),
                },
                _ => panic!("Illegal state"),
            }
            last = *i;
            let _ = self.iter.next();
        }
        if state == 0 || state == 2 || state >= 5 {
            Ok(last)
        } else {
            Err(JErr::EndOfInput)
        }
    }

    pub fn parse(&mut self) -> Result<usize, JErr> {
        while let Some((i, c)) = self.iter.next() {
            let mut prev_state = self.state;
            let mut tok_ix = self.tokens.len();
            let mut ws_flag = false;
            match c {
                ' ' | '\n' | '\t' | '\r' => ws_flag = true, // Whitespace
                '0'..='9' | '-' => {
                    self.value_ok(i, c)?;
                    // Number
                    let j = self.parse_number(i, c)?;
                    self.tokens.push(JTok {
                        tok_type: JType::Number,
                        start: i,
                        end: j,
                        size: 0,
                    });
                    self.state = JState::AfterValue;
                }
                '{' => {
                    self.value_ok(i, c)?;
                    // Object
                    self.stack.push((self.tokens.len(), self.state));
                    self.tokens.push(JTok {
                        tok_type: JType::Object,
                        start: i,
                        end: i,
                        size: 0,
                    });
                    self.state = JState::StartObject;
                }
                '}' => {
                    if self.state != JState::AfterValue && self.state != JState::StartObject {
                        return Err(JErr::Invalid(i, c));
                    }
                    if let Some((j, old_state)) = self.stack.pop() {
                        let tok = &mut self.tokens[j];
                        if tok.tok_type != JType::Object {
                            return Err(JErr::Invalid(i, c));
                        }
                        tok.end = i;
                        prev_state = old_state;
                        tok_ix = j;
                        self.state = JState::AfterValue;
                    } else {
                        return Err(JErr::Invalid(i, c));
                    }
                }
                '[' => {
                    self.value_ok(i, c)?;
                    // Array
                    self.stack.push((self.tokens.len(), self.state));
                    self.tokens.push(JTok {
                        tok_type: JType::Array,
                        start: i,
                        end: i,
                        size: 0,
                    });
                    self.state = JState::StartArray;
                }
                ']' => {
                    if self.state != JState::AfterValue && self.state != JState::StartArray {
                        return Err(JErr::Invalid(i, c));
                    }
                    if let Some((j, old_state)) = self.stack.pop() {
                        let tok = &mut self.tokens[j];
                        if tok.tok_type != JType::Array {
                            return Err(JErr::Invalid(i, c));
                        }
                        tok.end = i;
                        prev_state = old_state;
                        tok_ix = j;
                        self.state = JState::AfterValue;
                    } else {
                        return Err(JErr::Invalid(i, c));
                    }
                }
                ',' => {
                    if self.state != JState::AfterValue || self.stack.is_empty() {
                        return Err(JErr::Invalid(i, c));
                    }
                    self.state = JState::AfterComma;
                }
                ':' => {
                    if self.state != JState::AfterKey
                        || self.tokens.last().unwrap().tok_type != JType::String
                        || self.stack.is_empty()
                        || self.tokens[self.stack.last().unwrap().0].tok_type != JType::Object
                    {
                        return Err(JErr::Invalid(i, c));
                    }
                    self.state = JState::AfterColon;
                }
                '\"' => {
                    if self.state == JState::AfterValue {
                        return Err(JErr::Invalid(i, c));
                    }
                    // String
                    self.parse_string()?;
                    self.state = JState::AfterValue;
                }
                't' if (self.text[i..]).starts_with("true") => {
                    self.value_ok(i, c)?;
                    self.tokens.push(JTok {
                        tok_type: JType::Bool(true),
                        start: i,
                        end: i + 3,
                        size: 0,
                    });
                    let _ = self.iter.nth(2);
                    self.state = JState::AfterValue;
                }
                'f' if (self.text[i..]).starts_with("false") => {
                    self.value_ok(i, c)?;
                    self.tokens.push(JTok {
                        tok_type: JType::Bool(false),
                        start: i,
                        end: i + 4,
                        size: 0,
                    });
                    let _ = self.iter.nth(3);
                    self.state = JState::AfterValue;
                }
                'n' if (self.text[i..]).starts_with("null") => {
                    self.value_ok(i, c)?;
                    self.tokens.push(JTok {
                        tok_type: JType::Null,
                        start: i,
                        end: i + 3,
                        size: 0,
                    });
                    let _ = self.iter.nth(2);
                    self.state = JState::AfterValue;
                }
                _ => return Err(JErr::Invalid(i, c)),
            }
            if !ws_flag && self.state == JState::AfterValue {
                if prev_state == JState::AfterColon {
                    self.tokens[tok_ix - 1].size += 1;
                } else if let Some((ix, _state)) = self.stack.last() {
                    self.tokens[*ix].size += 1;
                    if self.tokens[*ix].tok_type == JType::Object {
                        self.state = JState::AfterKey;
                    }
                }
            }
        }
        if !self.stack.is_empty() || self.tokens.is_empty() {
            Err(JErr::EndOfInput)
        } else {
            Ok(self.n_tokens())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum ExpOut {
        String(&'static str, usize),
        Number(&'static str),
        Bool(bool),
        Null,
        Object(usize, usize, usize),
        Array(usize, usize, usize),
    }

    impl ExpOut {
        fn check(&self, jp: &JParse, ix: usize) {
            let tok = &jp.tokens[ix];
            match self {
                ExpOut::String(s, size) => {
                    assert_eq!(tok.tok_type, JType::String);
                    assert_eq!(tok.size, *size);
                    assert_eq!(*s, &jp.text[tok.start..tok.end]);
                }
                ExpOut::Number(s) => {
                    assert_eq!(tok.tok_type, JType::Number);
                    assert_eq!(*s, &jp.text[tok.start..=tok.end]);
                }
                ExpOut::Bool(flag) => assert_eq!(tok.tok_type, JType::Bool(*flag)),
                ExpOut::Null => assert_eq!(tok.tok_type, JType::Null),
                ExpOut::Object(start, end, size) => {
                    assert_eq!(tok.tok_type, JType::Object);
                    assert_eq!(tok.start, *start);
                    assert_eq!(tok.end, *end);
                    assert_eq!(tok.size, *size);
                }
                ExpOut::Array(start, end, size) => {
                    assert_eq!(tok.tok_type, JType::Array);
                    assert_eq!(tok.start, *start);
                    assert_eq!(tok.end, *end);
                    assert_eq!(tok.size, *size);
                }
            }
        }
    }

    fn check_parse(js: &str, res: Result<usize, JErr>, exp: &[ExpOut]) {
        let mut jparse = JParse::new(js);
        let r = jparse.parse();
        for (ix, tok) in jparse.tokens().iter().enumerate() {
            println!("{}\t{:?}", ix, tok);
        }
        assert_eq!(res, r);
        if r.is_ok() && !exp.is_empty() {
            assert_eq!(exp.len(), jparse.n_tokens());
            for (i, ex) in exp.iter().enumerate() {
                ex.check(&jparse, i);
            }
        }
    }

    #[test]
    fn test_parse() {
        let test_json = "{\"user\": \"johndoe\", \"admin\": false, \"uid\": 1000,\n  \"groups\": [\"users\", \"wheel\", \"audio\", \"video\"]}";
        check_parse(
            test_json,
            Ok(13),
            &[
                ExpOut::Object(0, 97, 4),
                ExpOut::String("user", 1),
                ExpOut::String("johndoe", 0),
                ExpOut::String("admin", 1),
                ExpOut::Bool(false),
                ExpOut::String("uid", 1),
                ExpOut::Number("1000"),
                ExpOut::String("groups", 1),
                ExpOut::Array(61, 96, 4),
                ExpOut::String("users", 0),
                ExpOut::String("wheel", 0),
                ExpOut::String("audio", 0),
                ExpOut::String("video", 0),
            ],
        );
    }
    #[test]
    fn test_empty_1() {
        check_parse("{}", Ok(1), &[ExpOut::Object(0, 1, 0)]);
    }
    #[test]
    fn test_empty_2() {
        check_parse("[]", Ok(1), &[ExpOut::Array(0, 1, 0)]);
    }
    #[test]
    fn test_empty_3() {
        check_parse(
            "[{},{}]",
            Ok(3),
            &[
                ExpOut::Array(0, 6, 2),
                ExpOut::Object(1, 2, 0),
                ExpOut::Object(4, 5, 0),
            ],
        );
    }
    #[test]
    fn test_object_1() {
        check_parse(
            "{\"a\":0}",
            Ok(3),
            &[
                ExpOut::Object(0, 6, 1),
                ExpOut::String("a", 1),
                ExpOut::Number("0"),
            ],
        );
    }
    #[test]
    fn test_object_2() {
        check_parse(
            "{\"a\":[]}",
            Ok(3),
            &[
                ExpOut::Object(0, 7, 1),
                ExpOut::String("a", 1),
                ExpOut::Array(5, 6, 0),
            ],
        );
    }
    #[test]
    fn test_object_3() {
        check_parse(
            "{\"a\":{},\"b\":{}}",
            Ok(5),
            &[
                ExpOut::Object(0, 14, 2),
                ExpOut::String("a", 1),
                ExpOut::Object(5, 6, 0),
                ExpOut::String("b", 1),
                ExpOut::Object(12, 13, 0),
            ],
        );
    }
    #[test]
    fn test_object_4() {
        check_parse(
            "{\n \"Day\": 26,\n \"Month\": 9,\n \"Year\": 12\n }",
            Ok(7),
            &[
                ExpOut::Object(0, 40, 3),
                ExpOut::String("Day", 1),
                ExpOut::Number("26"),
                ExpOut::String("Month", 1),
                ExpOut::Number("9"),
                ExpOut::String("Year", 1),
                ExpOut::Number("12"),
            ],
        );
    }
    #[test]
    fn test_object_5() {
        check_parse(
            "{\"a\": 0, \"b\": \"c\"}",
            Ok(5),
            &[
                ExpOut::Object(0, 17, 2),
                ExpOut::String("a", 1),
                ExpOut::Number("0"),
                ExpOut::String("b", 1),
                ExpOut::String("c", 0),
            ],
        );
    }
    #[test]
    fn test_object_6() {
        check_parse("{\"a\"\n0}", Err(JErr::Invalid(5, '0')), &[]);
    }

    #[test]
    fn test_object_7() {
        check_parse("{\"a\", 0}", Err(JErr::Invalid(4, ',')), &[]);
    }
    #[test]
    fn test_object_8() {
        check_parse("{\"a\": {2}}", Err(JErr::Invalid(7, '2')), &[]);
    }
    #[test]
    fn test_object_9() {
        check_parse("{\"a\": {2: 3}}", Err(JErr::Invalid(7, '2')), &[]);
    }
    #[test]
    fn test_object_10() {
        check_parse("{\"a\": {\"a\": 2 3}}", Err(JErr::Invalid(14, '3')), &[]);
    }
    #[test]
    fn test_object_11() {
        check_parse("{fds: 13}", Err(JErr::Invalid(1, 'f')), &[]);
    }
    #[test]
    fn test_object_12() {
        check_parse("{34, 54}", Err(JErr::Invalid(1, '3')), &[]);
    }
    #[test]
    fn test_object_13() {
        check_parse("{\"a\", \"b\"}", Err(JErr::Invalid(4, ',')), &[]);
    }
    #[test]
    fn test_object_14() {
        check_parse("{\"a\"}", Err(JErr::Invalid(4, '}')), &[]);
    }
    #[test]
    fn test_object_15() {
        check_parse("{\"a\": 1, \"b\"}", Err(JErr::Invalid(12, '}')), &[]);
    }
    #[test]
    fn test_object_16() {
        check_parse("{\"a\",\"b\":1}", Err(JErr::Invalid(4, ',')), &[]);
    }
    #[test]
    fn test_object_17() {
        check_parse("{\"a\":1,}", Err(JErr::Invalid(7, '}')), &[]);
    }
    #[test]
    fn test_object_18() {
        check_parse("{\"a\":\"b\":\"c\"}", Err(JErr::Invalid(8, ':')), &[]);
    }
    #[test]
    fn test_object_19() {
        check_parse("{,}", Err(JErr::Invalid(1, ',')), &[]);
    }

    #[test]
    fn test_array_1() {
        check_parse("[10}", Err(JErr::Invalid(3, '}')), &[]);
    }
    #[test]
    fn test_array_2() {
        check_parse(
            "[10]",
            Ok(2),
            &[ExpOut::Array(0, 3, 1), ExpOut::Number("10")],
        );
    }
    #[test]
    fn test_array_3() {
        check_parse("{\"a\": 1]", Err(JErr::Invalid(7, ']')), &[]);
    }
    #[test]
    fn test_array_4() {
        check_parse("[\"a\": 0]", Err(JErr::Invalid(4, ':')), &[]);
    }
    #[test]
    fn test_array_5() {
        check_parse("[10 , , 3]", Err(JErr::Invalid(6, ',')), &[]);
    }
    #[test]
    fn test_array_6() {
        check_parse("[10,]", Err(JErr::Invalid(4, ']')), &[]);
    }
    #[test]
    fn test_array_7() {
        check_parse("[,10]", Err(JErr::Invalid(1, ',')), &[]);
    }
    #[test]
    fn test_array_8() {
        check_parse("[10,5,", Err(JErr::EndOfInput), &[]);
    }
    #[test]
    fn test_array_9() {
        check_parse("[10,5, {\"a\": 6, \"b\":", Err(JErr::EndOfInput), &[]);
    }

    #[test]
    fn test_primitive_1() {
        check_parse(
            "{\"boolVar\" : true }",
            Ok(3),
            &[
                ExpOut::Object(0, 18, 1),
                ExpOut::String("boolVar", 1),
                ExpOut::Bool(true),
            ],
        );
    }
    #[test]
    fn test_primitive_2() {
        check_parse(
            "{\"boolVar\" : false }",
            Ok(3),
            &[
                ExpOut::Object(0, 19, 1),
                ExpOut::String("boolVar", 1),
                ExpOut::Bool(false),
            ],
        );
    }
    #[test]
    fn test_primitive_3() {
        check_parse(
            "{\"nullVar\" : null }",
            Ok(3),
            &[
                ExpOut::Object(0, 18, 1),
                ExpOut::String("nullVar", 1),
                ExpOut::Null,
            ],
        );
    }
    #[test]
    fn test_primitive_4() {
        check_parse(
            "{\"intVar\" : 12 }",
            Ok(3),
            &[
                ExpOut::Object(0, 15, 1),
                ExpOut::String("intVar", 1),
                ExpOut::Number("12"),
            ],
        );
    }
    #[test]
    fn test_primitive_5() {
        check_parse(
            "{\"floatVar\" : 12.345e4}",
            Ok(3),
            &[
                ExpOut::Object(0, 22, 1),
                ExpOut::String("floatVar", 1),
                ExpOut::Number("12.345e4"),
            ],
        );
    }
    #[test]
    fn test_primitive_6() {
        check_parse("{\"boolVar\" : truer }", Err(JErr::Invalid(17, 'r')), &[]);
    }
    #[test]
    fn test_primitive_7() {
        check_parse("{\"boolVar\" : tru }", Err(JErr::Invalid(13, 't')), &[]);
    }
    #[test]
    fn test_primitive_8() {
        check_parse("{\"boolVar\" : True }", Err(JErr::Invalid(13, 'T')), &[]);
    }
    #[test]
    fn test_primitive_9() {
        check_parse("{\"floatVar\" : 01.4 }", Err(JErr::Invalid(15, '1')), &[]);
    }
    #[test]
    fn test_primitive_10() {
        check_parse("{\"floatVar\" : 1.4.3 }", Err(JErr::Invalid(17, '.')), &[]);
    }
    #[test]
    fn test_primitive_11() {
        check_parse(
            "{\"intVar\" : 0 }",
            Ok(3),
            &[
                ExpOut::Object(0, 14, 1),
                ExpOut::String("intVar", 1),
                ExpOut::Number("0"),
            ],
        );
    }
    #[test]
    fn test_primitive_12() {
        check_parse(
            "{\"intVar\" : -43.2 }",
            Ok(3),
            &[
                ExpOut::Object(0, 18, 1),
                ExpOut::String("intVar", 1),
                ExpOut::Number("-43.2"),
            ],
        );
    }
    #[test]
    fn test_string_1() {
        check_parse(
            "{\"strVar\" : \"hello world\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 25, 1),
                ExpOut::String("strVar", 1),
                ExpOut::String("hello world", 0),
            ],
        );
    }
    #[test]
    fn test_string_2() {
        check_parse(
            "{\"strVar\" : \"escapes: \\/\\r\\n\\t\\b\\f\\\"\\\\\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 39, 1),
                ExpOut::String("strVar", 1),
                ExpOut::String("escapes: \\/\\r\\n\\t\\b\\f\\\"\\\\", 0),
            ],
        );
    }
    #[test]
    fn test_string_3() {
        check_parse(
            "{\"strVar\": \"\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 13, 1),
                ExpOut::String("strVar", 1),
                ExpOut::String("", 0),
            ],
        );
    }
    #[test]
    fn test_string_4() {
        check_parse(
            "{\"a\":\"\\uAbcD\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 13, 1),
                ExpOut::String("a", 1),
                ExpOut::String("\\uAbcD", 0),
            ],
        );
    }
    #[test]
    fn test_string_5() {
        check_parse(
            "{\"a\":\"str\\u0000\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 16, 1),
                ExpOut::String("a", 1),
                ExpOut::String("str\\u0000", 0),
            ],
        );
    }
    #[test]
    fn test_string_6() {
        check_parse(
            "{\"a\":\"\\uFFFFstr\"}",
            Ok(3),
            &[
                ExpOut::Object(0, 16, 1),
                ExpOut::String("a", 1),
                ExpOut::String("\\uFFFFstr", 0),
            ],
        );
    }
    #[test]
    fn test_string_7() {
        check_parse(
            "{\"a\":[\"\\u0280\"]}",
            Ok(4),
            &[
                ExpOut::Object(0, 15, 1),
                ExpOut::String("a", 1),
                ExpOut::Array(5, 14, 1),
                ExpOut::String("\\u0280", 0),
            ],
        );
    }
    #[test]
    fn test_string_8() {
        check_parse(
            "{\"a\":\"str\\uFFGFstr\"}",
            Err(JErr::Invalid(13, 'G')),
            &[],
        );
    }
    #[test]
    fn test_string_9() {
        check_parse("{\"a\":\"str\\u@FfF\"}", Err(JErr::Invalid(11, '@')), &[]);
    }
    #[test]
    fn test_string_10() {
        check_parse("{\"a\":[\"\\u028\"]}", Err(JErr::Invalid(12, '\"')), &[]);
    }
    #[test]
    fn test_string_11() {
        check_parse("{\"a\":[\"\\u028]}", Err(JErr::Invalid(12, ']')), &[]);
    }
    #[test]
    fn test_string_12() {
        check_parse("\"Non-terminated", Err(JErr::EndOfInput), &[]);
    }
    #[test]
    fn test_issue() {
        let test_json = "{ \"height\":10, \"layers\":[ { \"data\":[6,6], \"height\":10, \
    		\"name\":\"Calque de Tile 1\", \"opacity\":1, \"type\":\"tilelayer\", \
   			\"visible\":true, \"width\":10, \"x\":0, \"y\":0 }], \
      		\"orientation\":\"orthogonal\", \"properties\": { }, \"tileheight\":32, \
   		    \"tilesets\":[ { \"firstgid\":1, \"image\":\"..\\/images\\/tiles.png\", \
  		    \"imageheight\":64, \"imagewidth\":160, \"margin\":0, \
     	    \"name\":\"Tiles\", \
      		\"properties\":{}, \"spacing\":0, \"tileheight\":32, \"tilewidth\":32 \
      		}], \
      		\"tilewidth\":32, \"version\":1, \"width\":10 }";
        check_parse(test_json, Ok(61), &[]);
    }
}
