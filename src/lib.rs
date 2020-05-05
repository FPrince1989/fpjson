use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::{CharIndices, FromStr};

#[derive(Debug, PartialEq)]
pub enum FPValue {
    Null,
    False,
    True,
    Number(f64),
    String(String),
    Array(Vec<FPValue>),
    Object(HashMap<String, FPValue>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    ExpectValue,
    InvalidValue,
    RootNotSingular,
    NumberTooBig,
    MissQuotationMark,
    InvalidStringEscape,
    InvalidStringChar,
    InvalidUnicodeHex,
    InvalidUnicodeSurrogate,
    InvalidUTF8,
    MissCommaOrSquareBracket,
    MissKey,
    MissColon,
    MissCommaOrCurlyBracket,
}

type Result<T> = std::result::Result<T, ParseError>;

struct FPContext<'a> {
    json: &'a str,
    stack: Vec<char>,
}

impl FPContext<'_> {
    pub fn new(json: &str) -> FPContext {
        FPContext {
            json,
            stack: Vec::default(),
        }
    }

    pub fn parse_whitespace(&mut self) -> Result<()> {
        let mut index = self.json.len();
        for (i, c) in self.json.chars().enumerate() {
            if c != ' ' && c != '\t' && c != '\n' && c != '\r' {
                index = i;
                break;
            }
        }
        self.json = &self.json[index..];

        Result::Ok(())
    }

    fn parse_literal(
        &mut self,
        value: &mut FPValue,
        literal: &str,
        fp_value: FPValue,
    ) -> Result<()> {
        let len = literal.len();
        if self.json.len() >= len && &self.json[0..len] == literal {
            self.json = &self.json[len..];
            *value = fp_value;

            Result::Ok(())
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    fn parse_number(&mut self, value: &mut FPValue) -> Result<()> {
        macro_rules! assert_digit {
            ($current:expr) => {
                if !$current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                    let (index, _) = $current.unwrap_or((self.json.len(), char::default()));
                    self.json = &self.json[index..];
                    return Result::Err(ParseError::InvalidValue);
                }
            };
        }

        let mut iter = self.json.char_indices();
        let mut current = iter.next();

        // 负号处理
        if let Some((_, '-')) = current {
            current = iter.next();
        }
        // 整数部分处理
        if let Some((_, '0')) = current {
            current = iter.next();
        } else {
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = iter.next();
            }
        }
        // 小数部分处理
        if let Some((_, '.')) = current {
            current = iter.next();
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = iter.next();
            }
        }
        // 指数部分处理
        if current.map_or(false, |(_, c)| c == 'e' || c == 'E') {
            current = iter.next();
            if current.map_or(false, |(_, c)| c == '+' || c == '-') {
                current = iter.next();
            }
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = iter.next();
            }
        }

        let (index, _) = current.unwrap_or((self.json.len(), char::default()));

        if let Ok(number) = f64::from_str(&self.json[..index]) {
            if number.is_infinite() {
                return Result::Err(ParseError::NumberTooBig);
            }
            *value = FPValue::Number(number);
            self.json = &self.json[index..];

            Result::Ok(())
        } else {
            self.json = &self.json[index..];
            Result::Err(ParseError::InvalidValue)
        }
    }

    //noinspection RsExtraSemicolon
    fn parse_string(&mut self, value: &mut FPValue) -> Result<()> {
        macro_rules! return_result {
            ($iter:expr, $option_err:expr) => {
                let index = $iter.next().map_or(self.json.len(), |(index, _)| index);
                self.json = &self.json[index..];
                self.stack.clear();
                if $option_err.is_none() {
                    return Result::Ok(());
                } else {
                    return Result::Err($option_err.unwrap());
                }
            };
        }
        debug_assert!(self.json.starts_with('"'));

        let mut iter = self.json.char_indices();
        iter.next();
        while let Some((_, c)) = iter.next() {
            match c {
                '\\' => {
                    if let Some((_, next_char)) = iter.next() {
                        match next_char {
                            '"' | '\\' | '/' => self.stack.push(next_char),
                            'b' => self.stack.push(8 as char),
                            'f' => self.stack.push(12 as char),
                            'n' => self.stack.push('\n'),
                            'r' => self.stack.push('\r'),
                            't' => self.stack.push('\t'),
                            'u' => {
                                let high_surrogate = FPContext::parse_hex4(&mut iter)?;
                                let mut code_point = high_surrogate as u32;
                                if high_surrogate >= 0xD800 && high_surrogate <= 0xDBFF {
                                    let success = iter.next().map_or(false, |(_, c)| c == '\\')
                                        && iter.next().map_or(false, |(_, c)| c == 'u')
                                        && match FPContext::parse_hex4(&mut iter) {
                                            Ok(low_surrogate @ 0xDC00..=0xDFFF) => {
                                                code_point = 0x10000
                                                    + (high_surrogate as u32 - 0xD800) * 0x400
                                                    + (low_surrogate as u32 - 0xDC00);
                                                true
                                            }
                                            _ => false,
                                        };
                                    if !success {
                                        return_result!(
                                            iter,
                                            Some(ParseError::InvalidUnicodeSurrogate)
                                        );
                                    }
                                }
                                if let Ok(c) = char::try_from(code_point) {
                                    self.stack.push(c);
                                } else {
                                    return_result!(iter, Some(ParseError::InvalidUTF8));
                                }
                            }
                            _ => {
                                return_result!(iter, Some(ParseError::InvalidStringEscape));
                            }
                        }
                    } else {
                        break;
                    }
                }
                '"' => {
                    *value = FPValue::String(self.stack.iter().collect());
                    return_result!(iter, Option::<ParseError>::None);
                }
                '\0' => {
                    return_result!(iter, Some(ParseError::MissQuotationMark));
                }
                _ => {
                    if c >= '\x20' {
                        self.stack.push(c);
                    } else {
                        return_result!(iter, Some(ParseError::InvalidStringChar));
                    }
                }
            }
        }

        return_result!(iter, Some(ParseError::MissQuotationMark));
    }

    fn parse_hex4(char_indices: &mut CharIndices) -> Result<u16> {
        let mut result = 0;
        for _ in 0..4 {
            let success = char_indices.next().map_or(false, |(_, c)| {
                c.to_digit(16).map_or(false, |d| {
                    result <<= 4;
                    result += d as u16;
                    true
                })
            });
            if !success {
                return Result::Err(ParseError::InvalidUnicodeHex);
            }
        }
        Result::Ok(result)
    }

    fn parse_array(&mut self, value: &mut FPValue) -> Result<()> {
        debug_assert!(self.json.starts_with('['));
        self.json = &self.json[1..];
        let mut array = vec![];
        self.parse_whitespace()?;
        if let Some((i, ']')) = self.json.char_indices().next() {
            self.json = &self.json[i + 1..];
            *value = FPValue::Array(array);
            return Result::Ok(());
        }
        loop {
            let mut temp_value = FPValue::Null;
            self.parse_value(&mut temp_value)?;
            array.push(temp_value);
            self.parse_whitespace()?;
            let mut iter = self.json.char_indices();
            match iter.next() {
                Some((i, ',')) => {
                    self.json = &self.json[i + 1..];
                    self.parse_whitespace()?;
                }
                Some((i, ']')) => {
                    self.json = &self.json[i + 1..];
                    *value = FPValue::Array(array);
                    return Result::Ok(());
                }
                _ => {
                    return Result::Err(ParseError::MissCommaOrSquareBracket);
                }
            }
        }
    }

    fn parse_object(&mut self, value: &mut FPValue) -> Result<()> {
        debug_assert!(self.json.starts_with('{'));
        self.json = &self.json[1..];
        let mut map = HashMap::new();
        self.parse_whitespace()?;
        if let Some((i, '}')) = self.json.char_indices().next() {
            self.json = &self.json[i + 1..];
            *value = FPValue::Object(map);
            return Result::Ok(());
        }
        loop {
            let mut map_key = FPValue::Null;
            if !self.json.starts_with('"') {
                return Result::Err(ParseError::MissKey);
            }

            self.parse_string(&mut map_key)?;
            self.parse_whitespace()?;
            if self.json.starts_with(':') {
                self.json = &self.json[1..];
                self.parse_whitespace()?;
            } else {
                return Result::Err(ParseError::MissColon);
            }

            let mut map_value = FPValue::Null;
            self.parse_value(&mut map_value)?;
            let map_key = match map_key {
                FPValue::String(s) => s,
                _ => unreachable!(),
            };
            map.insert(map_key, map_value);
            self.parse_whitespace()?;
            let mut iter = self.json.char_indices();
            match iter.next() {
                Some((i, ',')) => {
                    self.json = &self.json[i + 1..];
                    self.parse_whitespace()?;
                }
                Some((i, '}')) => {
                    self.json = &self.json[i + 1..];
                    *value = FPValue::Object(map);
                    return Result::Ok(());
                }
                _ => {
                    return Result::Err(ParseError::MissCommaOrCurlyBracket);
                }
            }
        }
    }

    pub fn parse_value(&mut self, value: &mut FPValue) -> Result<()> {
        match self.json.chars().next() {
            None => Result::Err(ParseError::ExpectValue),
            Some('n') => self.parse_literal(value, "null", FPValue::Null),
            Some('t') => self.parse_literal(value, "true", FPValue::True),
            Some('f') => self.parse_literal(value, "false", FPValue::False),
            Some('"') => self.parse_string(value),
            Some('[') => self.parse_array(value),
            Some('{') => self.parse_object(value),
            _ => self.parse_number(value),
        }
    }
}

pub fn parse(value: &mut FPValue, json: &'static str) -> Result<()> {
    let mut context = FPContext::new(json);
    *value = FPValue::Null;
    context.parse_whitespace()?;
    match context.parse_value(value) {
        Ok(()) => {
            context.parse_whitespace()?;
            if !context.json.is_empty() {
                *value = FPValue::Null;
                Result::Err(ParseError::RootNotSingular)
            } else {
                Result::Ok(())
            }
        }
        Err(e) => Result::Err(e),
    }
}

#[cfg(test)]
#[allow(clippy::cognitive_complexity)]
mod tests {
    use crate::{parse, FPValue, ParseError};
    use std::collections::HashMap;

    macro_rules! test_parse_error {
        ($json:expr, $error:expr) => {
            let mut v = FPValue::False;

            assert_eq!(parse(&mut v, $json).unwrap_err(), $error);
            assert_eq!(v, FPValue::Null);
        };
    }

    macro_rules! test_parse_number {
        ($json:expr, $number:expr) => {
            let mut v = FPValue::Null;

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v, FPValue::Number($number));
        };
    }

    macro_rules! test_parse_string {
        ($json:expr, $expect_str:expr) => {
            let mut v = FPValue::Null;

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v, FPValue::String(String::from($expect_str)));
        };
    }

    macro_rules! test_parse_array {
        ($json:expr, $vec:expr) => {
            let mut v = FPValue::Null;

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v, FPValue::Array($vec));
        };
    }

    macro_rules! test_parse_object {
        ($json:expr, $map:expr) => {
            let mut v = FPValue::Null;

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v, FPValue::Object($map));
        };
    }

    #[test]
    fn test_parse_null() {
        let mut v = FPValue::Null;
        assert!(parse(&mut v, "null").is_ok());
        assert_eq!(v, FPValue::Null);

        v = FPValue::False;
        assert_eq!(parse(&mut v, "null  \r\n").is_ok(), true);
        assert_eq!(v, FPValue::Null);
    }

    #[test]
    fn test_parse_true() {
        let mut v = FPValue::Null;
        assert!(parse(&mut v, "true").is_ok());
        assert_eq!(v, FPValue::True);

        v = FPValue::Null;
        assert_eq!(parse(&mut v, "\t true  \r\n").is_ok(), true);
        assert_eq!(v, FPValue::True);
    }

    #[test]
    fn test_parse_false() {
        let mut v = FPValue::Null;
        assert_eq!(parse(&mut v, "false").is_ok(), true);
        assert_eq!(v, FPValue::False);

        v = FPValue::Null;
        assert_eq!(parse(&mut v, "\t false  \r\n").is_ok(), true);
        assert_eq!(v, FPValue::False);
    }

    #[test]
    fn test_parse_except_value() {
        test_parse_error!("", ParseError::ExpectValue);
        test_parse_error!(" ", ParseError::ExpectValue);
        test_parse_error!(" \r \t \n", ParseError::ExpectValue);
    }

    #[test]
    fn test_parse_invalid_value() {
        test_parse_error!("nul", ParseError::InvalidValue);
        test_parse_error!("?", ParseError::InvalidValue);
        // invalid number
        test_parse_error!("+0", ParseError::InvalidValue);
        test_parse_error!("+1", ParseError::InvalidValue);
        test_parse_error!(".123", ParseError::InvalidValue);
        test_parse_error!("1.", ParseError::InvalidValue);
        test_parse_error!("INF", ParseError::InvalidValue);
        test_parse_error!("inf", ParseError::InvalidValue);
        test_parse_error!("NAN", ParseError::InvalidValue);
        test_parse_error!("nan", ParseError::InvalidValue);
    }

    #[test]
    fn test_parse_root_not_singular() {
        test_parse_error!("null x", ParseError::RootNotSingular);
        test_parse_error!("null ?", ParseError::RootNotSingular);
        test_parse_error!("null \r\n\tx", ParseError::RootNotSingular);
        // invalid number
        test_parse_error!("0123", ParseError::RootNotSingular);
        test_parse_error!("0x0", ParseError::RootNotSingular);
        test_parse_error!("0x123", ParseError::RootNotSingular);
    }

    #[test]
    fn test_parse_number() {
        test_parse_number!("0", 0.0);
        test_parse_number!("-0", 0.0);
        test_parse_number!("-0.0", 0.0);
        test_parse_number!("1", 1.0);
        test_parse_number!("-1.0", -1.0);
        test_parse_number!("1.5", 1.5);
        test_parse_number!("-1.5", -1.5);
        test_parse_number!("4.1416", 4.1416);
        test_parse_number!("1E10", 1E10);
        test_parse_number!("1e10", 1e10);
        test_parse_number!("1E+10", 1E+10);
        test_parse_number!("1E-10", 1E-10);
        test_parse_number!("-1E10", -1E10);
        test_parse_number!("-1e10", -1e10);
        test_parse_number!("-1E+10", -1E+10);
        test_parse_number!("-1E-10", -1E-10);
        test_parse_number!("1.234E+10", 1.234E+10);
        test_parse_number!("1.234E-10", 1.234E-10);
        test_parse_number!("1e-10000", 0.0);
        test_parse_number!("1.0000000000000002", 1.000_000_000_000_000_2);
        test_parse_number!("4.9406564584124654E-324", 5E-_324);
        test_parse_number!("2.2250738585072009E-308", 2.225_073_858_507_201E-_308);
        test_parse_number!("2.2250738585072014E-308", 2.225_073_858_507_201_4E-_308);
        test_parse_number!("1.7976931348623157E308", 1.797_693_134_862_315_7E308);
        test_parse_number!("-2.2250738585072009E-308", -2.225_073_858_507_201E-_308);
        test_parse_number!("-2.2250738585072014E-308", -2.225_073_858_507_201_4E-_308);
        test_parse_number!("-1.7976931348623157E308", -1.797_693_134_862_315_7E308);
    }

    #[test]
    fn test_parse_number_too_big() {
        test_parse_error!("1e309", ParseError::NumberTooBig);
        test_parse_error!("-1e309", ParseError::NumberTooBig);
    }

    #[test]
    fn test_parse_string() {
        test_parse_string!("\"\"", "");
        test_parse_string!("\"hello world\"", "hello world");
        test_parse_string!("\"hello \\\"world\"", "hello \"world");
        test_parse_string!("\"hello \\/world\"", "hello /world");
        test_parse_string!("\"hello \\bworld\"", "hello \u{0008}world");
        test_parse_string!("\"hello \\fworld\"", "hello \u{000C}world");
        test_parse_string!("\"hello \\nworld\"", "hello \nworld");
        test_parse_string!("\"hello \\rworld\"", "hello \rworld");
        test_parse_string!("\"hello \\tworld\"", "hello \tworld");
        test_parse_string!("\"hello 𝄞 world\"", "hello 𝄞 world");

        test_parse_string!(
            "\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"",
            "\" \\ / \u{0008} \u{000C} \n \r \t"
        );

        test_parse_string!("\"𝄞\"", "𝄞");
        test_parse_string!("\"Hello\\u0000World\"", "Hello\0World");
        test_parse_string!("\"\\uD834\\uDD1E\"", "𝄞");
        test_parse_string!("\"\\ud834\\uDd1e\"", "𝄞");
        test_parse_string!("\"\\u0024\"", "$");
        test_parse_string!("\"\\u00A2\"", "¢");
        test_parse_string!("\"\\u20AC\"", "€");
    }

    #[test]
    fn test_parse_missing_quotation_mark() {
        test_parse_error!("\"", ParseError::MissQuotationMark);
        test_parse_error!("\"afjladflaf\01231231", ParseError::MissQuotationMark);
        test_parse_error!("\"123ad", ParseError::MissQuotationMark);
    }

    #[test]
    fn test_parse_invalid_string_escape() {
        test_parse_error!("\"\\v\"", ParseError::InvalidStringEscape);
        test_parse_error!("\"\\'\"", ParseError::InvalidStringEscape);
        test_parse_error!("\"\\0\"", ParseError::InvalidStringEscape);
        test_parse_error!("\"\\x12\"", ParseError::InvalidStringEscape);
    }

    #[test]
    fn test_parse_invalid_string_char() {
        test_parse_error!("\"\x01\"", ParseError::InvalidStringChar);
        test_parse_error!("\"\x1F\"", ParseError::InvalidStringChar);
    }

    #[test]
    fn test_parse_invalid_unicode_hex() {
        test_parse_error!("\"\\u\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u0\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u01\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u012\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u/000\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\uG000\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u0/00\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u0G00\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u00/0\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u00G0\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u000/\"", ParseError::InvalidUnicodeHex);
        test_parse_error!("\"\\u000G\"", ParseError::InvalidUnicodeHex);
    }

    #[test]
    fn test_parse_invalid_unicode_surrogate() {
        test_parse_error!("\"\\uD800\"", ParseError::InvalidUnicodeSurrogate);
        test_parse_error!("\"\\uDBFF\"", ParseError::InvalidUnicodeSurrogate);
        test_parse_error!("\"\\uD800\\\\\"", ParseError::InvalidUnicodeSurrogate);
        test_parse_error!("\"\\uD800\\uDBFF\"", ParseError::InvalidUnicodeSurrogate);
        test_parse_error!("\"\\uD800\\uE000\"", ParseError::InvalidUnicodeSurrogate);
    }

    #[test]
    fn test_parse_miss_comma_or_square_bracket() {
        test_parse_error!("[  1", ParseError::MissCommaOrSquareBracket);
        test_parse_error!("[  1  , [1,2 ,3]", ParseError::MissCommaOrSquareBracket);
    }

    #[test]
    fn test_parse_array() {
        test_parse_array!("[ ]", vec![]);
        test_parse_array!(
            "[1, [1, 2, \"3\"]]",
            vec![
                FPValue::Number(1.0),
                FPValue::Array(vec![
                    FPValue::Number(1.0),
                    FPValue::Number(2.0),
                    FPValue::String("3".to_string())
                ])
            ]
        );
        test_parse_array!(
            "[ null , false , true , 123 , \"abc\" ]",
            vec![
                FPValue::Null,
                FPValue::False,
                FPValue::True,
                FPValue::Number(123.0),
                FPValue::String("abc".to_string())
            ]
        );
        test_parse_array!(
            "[ [ ] , [ 0 ] , [ 0 , 1 ] , [ 0 , 1 , 2 ] ]",
            vec![
                FPValue::Array(vec![]),
                FPValue::Array(vec![FPValue::Number(0.0)]),
                FPValue::Array(vec![FPValue::Number(0.0), FPValue::Number(1.0)]),
                FPValue::Array(vec![
                    FPValue::Number(0.0),
                    FPValue::Number(1.0),
                    FPValue::Number(2.0)
                ]),
            ]
        );
        test_parse_array!(
            "[ [ ] , [ 0 ] , [ 0 , 1 ] , [ 0 , 1 , 2 ], { \t } ]",
            vec![
                FPValue::Array(vec![]),
                FPValue::Array(vec![FPValue::Number(0.0)]),
                FPValue::Array(vec![FPValue::Number(0.0), FPValue::Number(1.0)]),
                FPValue::Array(vec![
                    FPValue::Number(0.0),
                    FPValue::Number(1.0),
                    FPValue::Number(2.0)
                ]),
                FPValue::Object(HashMap::new()),
            ]
        );
    }

    #[test]
    fn test_parse_miss_key() {
        test_parse_error!("{:1,", ParseError::MissKey);
        test_parse_error!("{1:1,", ParseError::MissKey);
        test_parse_error!("{true:1,", ParseError::MissKey);
        test_parse_error!("{false:1,", ParseError::MissKey);
        test_parse_error!("{null:1,", ParseError::MissKey);
        test_parse_error!("{[]:1,", ParseError::MissKey);
        test_parse_error!("{{}:1,", ParseError::MissKey);
        test_parse_error!("{\"a\":1,", ParseError::MissKey);
    }

    #[test]
    fn test_parse_miss_colon() {
        test_parse_error!("{\"a\"}", ParseError::MissColon);
        test_parse_error!("{\"a\",\"b\"}", ParseError::MissColon);
    }

    #[test]
    fn test_parse_miss_comma_or_curly_bracket() {
        test_parse_error!("{\"a\":1", ParseError::MissCommaOrCurlyBracket);
        test_parse_error!("{\"a\":1]", ParseError::MissCommaOrCurlyBracket);
        test_parse_error!("{\"a\":1 \"b\"", ParseError::MissCommaOrCurlyBracket);
        test_parse_error!("{\"a\":{}", ParseError::MissCommaOrCurlyBracket);
    }

    #[test]
    fn test_parse_object() {
        let result = HashMap::new();
        test_parse_object!("{ }", result);

        let mut result = HashMap::new();
        result.insert("n".to_string(), FPValue::Null);
        result.insert("f".to_string(), FPValue::False);
        result.insert("t".to_string(), FPValue::True);
        result.insert("i".to_string(), FPValue::Number(123.0));
        result.insert("s".to_string(), FPValue::String("abc".to_string()));
        result.insert(
            "a".to_string(),
            FPValue::Array(vec![
                FPValue::Number(1.0),
                FPValue::Number(2.0),
                FPValue::Number(3.0),
            ]),
        );
        let mut sub_map = HashMap::new();
        sub_map.insert("1".to_string(), FPValue::Number(1.0));
        sub_map.insert("2".to_string(), FPValue::Number(2.0));
        sub_map.insert("3".to_string(), FPValue::Number(3.0));
        result.insert("o".to_string(), FPValue::Object(sub_map));
        test_parse_object!(
            " { 
        \"n\" : null , 
        \"f\" : false , 
        \"t\" : true , 
        \"i\" : 123 , 
        \"s\" : \"abc\", 
        \"a\" : [ 1, 2, 3 ],
        \"o\" : { \"1\" : 1, \"2\" : 2, \"3\" : 3 }
         } ",
            result
        );
    }
}
