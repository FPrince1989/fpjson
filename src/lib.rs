use std::convert::TryFrom;
use std::str::{CharIndices, FromStr};

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
    InvalidUnicodeHex,
    InvalidUnicodeSurrogate,
    InvalidUTF8,
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
            };
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

        let mut char_indices_iter = self.json.char_indices();
        let first = char_indices_iter.next();
        debug_assert!(first.unwrap().1 == '"');
        while let Some((_, c)) = char_indices_iter.next() {
            match c {
                '\\' => {
                    if let Some((_, next_char)) = char_indices_iter.next() {
                        match next_char {
                            '"' | '\\' | '/' => self.stack.push(next_char),
                            'b' => self.stack.push(8 as char),
                            'f' => self.stack.push(12 as char),
                            'n' => self.stack.push('\n'),
                            'r' => self.stack.push('\r'),
                            't' => self.stack.push('\t'),
                            'u' => {
                                let high_surrogate = FPContext::parse_hex4(&mut char_indices_iter)?;
                                let mut code_point = high_surrogate as u32;
                                if high_surrogate >= 0xD800 && high_surrogate <= 0xDBFF {
                                    if char_indices_iter.next().map_or(true, |(_, c)| c != '\\')
                                        || char_indices_iter.next().map_or(true, |(_, c)| c != 'u')
                                    {
                                        return_result!(
                                            char_indices_iter,
                                            Some(ParseError::InvalidUnicodeSurrogate)
                                        );
                                    }
                                    if let Ok(low_surrogate) =
                                        FPContext::parse_hex4(&mut char_indices_iter)
                                    {
                                        if low_surrogate >= 0xDC00 && low_surrogate <= 0xDFFF {
                                            code_point = 0x10000
                                                + (high_surrogate as u32 - 0xD800) * 0x400
                                                + (low_surrogate as u32 - 0xDC00);
                                        } else {
                                            return_result!(
                                                char_indices_iter,
                                                Some(ParseError::InvalidUnicodeSurrogate)
                                            );
                                        }
                                    } else {
                                        return_result!(
                                            char_indices_iter,
                                            Some(ParseError::InvalidUnicodeSurrogate)
                                        );
                                    }
                                }
                                if let Ok(c) = char::try_from(code_point) {
                                    self.stack.push(c);
                                } else {
                                    return_result!(
                                        char_indices_iter,
                                        Some(ParseError::InvalidUTF8)
                                    );
                                }
                            }
                            _ => {
                                return_result!(
                                    char_indices_iter,
                                    Some(ParseError::InvalidStringEscape)
                                );
                            }
                        }
                    } else {
                        break;
                    }
                }
                '"' => {
                    value.fp_type = FPType::String(self.stack.iter().collect());
                    return_result!(char_indices_iter, Option::<ParseError>::None);
                }
                '\0' => {
                    return_result!(char_indices_iter, Some(ParseError::MissQuotationMark));
                }
                _ => {
                    if c >= '\x20' {
                        self.stack.push(c);
                    } else {
                        return_result!(char_indices_iter, Some(ParseError::InvalidStringChar));
                    }
                }
            }
        }

        return_result!(char_indices_iter, Some(ParseError::MissQuotationMark));
    }

    fn parse_hex4(char_indices: &mut CharIndices) -> Result<u16> {
        let mut result = 0;
        for _ in 0..4 {
            if let Some((_, c)) = char_indices.next() {
                if let Some(d) = c.to_digit(16) {
                    result <<= 4;
                    result += d as u16;
                } else {
                    return Result::Err(ParseError::InvalidUnicodeHex);
                }
            } else {
                return Result::Err(ParseError::InvalidUnicodeHex);
            }
        }
        Result::Ok(result)
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
        Err(e) => Result::Err(e),
    }
}

#[cfg(test)]
#[allow(clippy::cognitive_complexity)]
mod tests {
    use crate::{parse, FPType, FPValue, ParseError};

    macro_rules! test_parse_error {
        ($json:expr, $error:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False,
            };

            assert_eq!(parse(&mut v, $json).unwrap_err(), $error);
            assert_eq!(v.fp_type, FPType::Null);
        };
    }

    macro_rules! test_parse_number {
        ($json:expr, $number:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False,
            };

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v.fp_type, FPType::Number($number));
        };
    }

    macro_rules! test_parse_string {
        ($json:expr, $expect_str:expr) => {
            let mut v = FPValue {
                fp_type: FPType::False,
            };

            assert!(parse(&mut v, $json).is_ok());
            assert_eq!(v.fp_type, FPType::String(String::from($expect_str)));
        };
    }

    #[test]
    fn test_parse_null() {
        let mut v = FPValue {
            fp_type: FPType::False,
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
            fp_type: FPType::Null,
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
            fp_type: FPType::True,
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
}
