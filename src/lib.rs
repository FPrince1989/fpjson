use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum FPType {
    Null,
    False,
    True,
    Number(f64),
    String(String),
    Array,
    Object,
}

#[derive(Debug)]
pub struct FPValue {
    fp_type: FPType,
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

    fn parse_literal(&mut self, value: &mut FPValue, literal: &str, fp_type: FPType) -> Result<()> {
        let len = literal.len();
        if self.json.len() >= len && &self.json[0..len] == literal {
            self.json = &self.json[len..];
            value.fp_type = fp_type;

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
            }
        }

        let mut char_indices_iter = self.json.char_indices();
        let mut current = char_indices_iter.next();


        // 负号处理
        if let Some((_, '-')) = current {
            current = char_indices_iter.next();
        }
        // 整数部分处理
        if let Some((_, '0')) = current {
            current = char_indices_iter.next();
        } else {
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = char_indices_iter.next();
            }
        }
        // 小数部分处理
        if let Some((_, '.')) = current {
            current = char_indices_iter.next();
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = char_indices_iter.next();
            }
        }
        // 指数部分处理
        if current.map_or(false, |(_, c)| c == 'e' || c == 'E') {
            current = char_indices_iter.next();
            if current.map_or(false, |(_, c)| c == '+' || c == '-') {
                current = char_indices_iter.next();
            }
            assert_digit!(current);
            while current.map_or(false, |(_, c)| c.is_ascii_digit()) {
                current = char_indices_iter.next();
            }
        }

        let (index, _) = current.unwrap_or((self.json.len(), char::default()));

        if let Ok(number) = f64::from_str(&self.json[..index]) {
            if number.is_infinite() {
                return Result::Err(ParseError::NumberTooBig);
            }
            value.fp_type = FPType::Number(number);
            self.json = &self.json[index..];

            Result::Ok(())
        } else {
            self.json = &self.json[index..];
            Result::Err(ParseError::InvalidValue)
        }
    }

    fn parse_string(&mut self, value: &mut FPValue) -> Result<()> {
        let mut char_indices_iter = self.json.char_indices();
        let first = char_indices_iter.next();
        debug_assert!(first.unwrap().1 == '"');
        while let Some((i, c)) = char_indices_iter.next() {
            match c {
                '\\' => {
                    let next = char_indices_iter.next();
                    if let Some((_, next_char)) = next {
                        match next_char {
                            '"' => self.stack.push(next_char),
                            '\\' => self.stack.push(next_char),
                            '/' => self.stack.push(next_char),
                            'b' => self.stack.push(8 as char),
                            'f' => self.stack.push(12 as char),
                            'n' => self.stack.push('\n'),
                            'r' => self.stack.push('\r'),
                            't' => self.stack.push('\t'),
                            _ => {
                                self.json = &self.json[i + 2..];
                                self.stack.clear();
                                return Result::Err(ParseError::InvalidStringEscape);
                            }
                        }
                    } else {
                        break;
                    }
                }
                '"' => {
                    value.fp_type = FPType::String(self.stack.iter().collect());
                    self.json = &self.json[i + 1..];
                    self.stack.clear();
                    return Result::Ok(());
                }
                '\0' => {
                    self.json = &self.json[i + 1..];
                    self.stack.clear();
                    return Result::Err(ParseError::MissQuotationMark);
                }
                _ => {
                    if c.escape_default().len() == 1 {
                        self.stack.push(c);
                    } else {
                        self.json = &self.json[i + 1..];
                        self.stack.clear();
                        return Result::Err(ParseError::InvalidStringChar);
                    }
                }
            }
        }

        self.json = &self.json[self.json.len()..];
        self.stack.clear();
        Result::Err(ParseError::MissQuotationMark)
    }

    pub fn parse_value(&mut self, value: &mut FPValue) -> Result<()> {
        if let Some(c) = self.json.chars().next() {
            match c {
                'n' => self.parse_literal(value, "null", FPType::Null),
                't' => self.parse_literal(value, "true", FPType::True),
                'f' => self.parse_literal(value, "false", FPType::False),
                '"' => self.parse_string(value),
                _ => self.parse_number(value),
            }
        } else {
            Result::Err(ParseError::ExpectValue)
        }
    }
}

pub fn parse(value: &mut FPValue, json: &'static str) -> Result<()> {
    let mut context = FPContext::new(json);
    value.fp_type = FPType::Null;
    context.parse_whitespace()?;
    match context.parse_value(value) {
        Ok(()) => {
            context.parse_whitespace()?;
            if !context.json.is_empty() {
                value.fp_type = FPType::Null;
                Result::Err(ParseError::RootNotSingular)
            } else {
                Result::Ok(())
            }
        }
        Err(e) => Result::Err(e)
    }
}


#[cfg(test)]
mod tests {
    use crate::{FPType, FPValue, parse, ParseError};

    macro_rules! test_parse_error {
        ($json:expr, $error:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False
            };

            assert_eq!(parse(&mut v, $json).unwrap_err(), $error);
            assert_eq!(v.fp_type, FPType::Null);
        };
    }

    macro_rules! test_parse_number {
        ($json:expr, $number:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False
            };

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v.fp_type, FPType::Number($number));
        };
    }

    macro_rules! test_parse_string {
        ($json:expr, $expect_str:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False
            };

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v.fp_type, FPType::String(String::from($expect_str)));
        };
    }

    #[test]
    fn test_parse_null() {
        let mut v = FPValue {
            fp_type: FPType::False
        };
        assert!(parse(&mut v, "null").is_ok());
        assert_eq!(v.fp_type, FPType::Null);

        v.fp_type = FPType::False;
        assert_eq!(parse(&mut v, "null  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::Null);
    }

    #[test]
    fn test_parse_true() {
        let mut v = FPValue {
            fp_type: FPType::Null
        };
        assert!(parse(&mut v, "true").is_ok());
        assert_eq!(v.fp_type, FPType::True);

        v.fp_type = FPType::Null;
        assert_eq!(parse(&mut v, "\t true  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::True);
    }

    #[test]
    fn test_parse_false() {
        let mut v = FPValue {
            fp_type: FPType::True
        };
        assert_eq!(parse(&mut v, "false").is_ok(), true);
        assert_eq!(v.fp_type, FPType::False);

        v.fp_type = FPType::Null;
        assert_eq!(parse(&mut v, "\t false  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::False);
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
    #[allow(clippy::cognitive_complexity)]
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

        test_parse_string!("\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"", "\" \\ / \u{0008} \u{000C} \n \r \t" );
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
}
