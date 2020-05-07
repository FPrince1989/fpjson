use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::{CharIndices, FromStr};

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    False,
    True,
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
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

struct Context<'a> {
    json: &'a str,
    stack: Vec<char>,
}

impl Context<'_> {
    pub fn new(json: &str) -> Context {
        Context {
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

    fn parse_literal(&mut self, literal: &str, fp_value: JsonValue) -> Result<JsonValue> {
        let len = literal.len();
        if self.json.len() >= len && &self.json[0..len] == literal {
            self.json = &self.json[len..];

            Result::Ok(fp_value)
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    fn parse_number(&mut self) -> Result<JsonValue> {
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
            self.json = &self.json[index..];

            Result::Ok(JsonValue::Number(number))
        } else {
            self.json = &self.json[index..];
            Result::Err(ParseError::InvalidValue)
        }
    }

    //noinspection RsExtraSemicolon
    fn parse_string(&mut self) -> Result<JsonValue> {
        macro_rules! return_result {
            ($iter:expr, $option_err:expr) => {
                let index = $iter.next().map_or(self.json.len(), |(index, _)| index);
                self.json = &self.json[index..];
                if $option_err.is_none() {
                    let str = self.stack.iter().collect::<String>();
                    self.stack.clear();
                    return Result::Ok(JsonValue::String(str));
                } else {
                    self.stack.clear();
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
                                let high_surrogate = Context::parse_hex4(&mut iter)?;
                                let mut code_point = high_surrogate as u32;
                                if high_surrogate >= 0xD800 && high_surrogate <= 0xDBFF {
                                    let success = iter.next().map_or(false, |(_, c)| c == '\\')
                                        && iter.next().map_or(false, |(_, c)| c == 'u')
                                        && match Context::parse_hex4(&mut iter) {
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

    fn parse_array(&mut self) -> Result<JsonValue> {
        debug_assert!(self.json.starts_with('['));
        self.json = &self.json[1..];
        let mut array = vec![];
        self.parse_whitespace()?;
        if let Some((i, ']')) = self.json.char_indices().next() {
            self.json = &self.json[i + 1..];
            return Result::Ok(JsonValue::Array(array));
        }
        loop {
            array.push(self.parse_value()?);
            self.parse_whitespace()?;
            let mut iter = self.json.char_indices();
            match iter.next() {
                Some((i, ',')) => {
                    self.json = &self.json[i + 1..];
                    self.parse_whitespace()?;
                }
                Some((i, ']')) => {
                    self.json = &self.json[i + 1..];
                    return Result::Ok(JsonValue::Array(array));
                }
                _ => {
                    return Result::Err(ParseError::MissCommaOrSquareBracket);
                }
            }
        }
    }

    fn parse_object(&mut self) -> Result<JsonValue> {
        debug_assert!(self.json.starts_with('{'));
        self.json = &self.json[1..];
        let mut map = HashMap::new();
        self.parse_whitespace()?;
        if let Some((i, '}')) = self.json.char_indices().next() {
            self.json = &self.json[i + 1..];
            return Result::Ok(JsonValue::Object(map));
        }
        loop {
            if !self.json.starts_with('"') {
                return Result::Err(ParseError::MissKey);
            }

            let map_key = match self.parse_string()? {
                JsonValue::String(str) => str,
                _ => unreachable!(),
            };
            self.parse_whitespace()?;
            if self.json.starts_with(':') {
                self.json = &self.json[1..];
                self.parse_whitespace()?;
            } else {
                return Result::Err(ParseError::MissColon);
            }

            map.insert(map_key, self.parse_value()?);
            self.parse_whitespace()?;
            let mut iter = self.json.char_indices();
            match iter.next() {
                Some((i, ',')) => {
                    self.json = &self.json[i + 1..];
                    self.parse_whitespace()?;
                }
                Some((i, '}')) => {
                    self.json = &self.json[i + 1..];
                    return Result::Ok(JsonValue::Object(map));
                }
                _ => {
                    return Result::Err(ParseError::MissCommaOrCurlyBracket);
                }
            }
        }
    }

    pub fn parse_value(&mut self) -> Result<JsonValue> {
        match self.json.chars().next() {
            None => Result::Err(ParseError::ExpectValue),
            Some('n') => self.parse_literal("null", JsonValue::Null),
            Some('t') => self.parse_literal("true", JsonValue::True),
            Some('f') => self.parse_literal("false", JsonValue::False),
            Some('"') => self.parse_string(),
            Some('[') => self.parse_array(),
            Some('{') => self.parse_object(),
            _ => self.parse_number(),
        }
    }
}

impl JsonValue {
    pub fn stringify(&self) -> String {
        match self {
            JsonValue::Null => String::from("null"),
            JsonValue::True => String::from("true"),
            JsonValue::False => String::from("false"),
            JsonValue::Number(n) => format!("{}", n),
            JsonValue::String(s) => JsonValue::stringify_string(s),
            JsonValue::Array(vec) => {
                let mut result = String::from("[");
                for (i, value) in vec.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(value.stringify().as_str());
                }
                result.push(']');
                result
            }
            JsonValue::Object(map) => {
                let mut result = String::from("{");
                for (i, (k, v)) in map.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(
                        format!(
                            "{}:{}",
                            JsonValue::stringify_string(k.as_str()),
                            v.stringify()
                        )
                        .as_str(),
                    );
                }
                result.push('}');
                result
            }
        }
    }

    fn stringify_string(s: &str) -> String {
        let mut result = String::from("\"");
        for c in s.chars() {
            match c {
                '"' | '/' | '\\' => {
                    result.push('\\');
                    result.push(c);
                }
                '\t' => {
                    result.push('\\');
                    result.push('t');
                }
                '\r' => {
                    result.push('\\');
                    result.push('r');
                }
                '\n' => {
                    result.push('\\');
                    result.push('n');
                }
                '\x08' => {
                    result.push('\\');
                    result.push('b');
                }
                '\x0C' => {
                    result.push('\\');
                    result.push('f');
                }
                c if c < '\x20' => result.push_str(format!("\\u00{:02x}", c as u8).as_str()),
                _ => result.push(c),
            }
        }
        result.push('"');
        result
    }
}

pub fn parse(json: &str) -> Result<JsonValue> {
    let mut context = Context::new(json);
    context.parse_whitespace()?;
    match context.parse_value() {
        Ok(value) => {
            context.parse_whitespace()?;
            if !context.json.is_empty() {
                Result::Err(ParseError::RootNotSingular)
            } else {
                Result::Ok(value)
            }
        }
        Err(e) => Result::Err(e),
    }
}
