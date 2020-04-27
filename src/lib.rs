#[derive(Debug, Eq, PartialEq)]
enum FPType {
    Null,
    False,
    True,
    Number,
    String,
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
}

type Result<T> = std::result::Result<T, ParseError>;

struct FPContext<'a> {
    json: &'a str,
}

impl FPContext<'_> {
    pub fn new(json: &str) -> FPContext {
        FPContext {
            json
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
        self.json = &self.json[index..self.json.len()];

        Result::Ok(())
    }

    fn parse_literal(&mut self, value: &mut FPValue, literal: &str, fp_type: FPType) -> Result<()> {
        let len = literal.len();
        if self.json.len() >= len && &self.json[0..len] == literal {
            self.json = &self.json[len..self.json.len()];
            value.fp_type = fp_type;

            Result::Ok(())
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    pub fn parse_value(&mut self, value: &mut FPValue) -> Result<()> {
        if let Some(c) = self.json.chars().next() {
            match c {
                'n' => self.parse_literal(value, "null", FPType::Null),
                't' => self.parse_literal(value, "true", FPType::True),
                'f' => self.parse_literal(value, "false", FPType::False),
                _ => Result::Err(ParseError::InvalidValue),
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

    #[test]
    fn test_parse_null() {
        let mut v = FPValue {
            fp_type: FPType::False
        };
        assert!(parse(&mut v, "null").is_ok());
        assert_eq!(v.fp_type, FPType::Null);
    }

    #[test]
    fn test_parse_true() {
        let mut v = FPValue {
            fp_type: FPType::Null
        };
        assert!(parse(&mut v, "true").is_ok());
        assert_eq!(v.fp_type, FPType::True);
    }

    #[test]
    fn test_parse_false() {
        let mut v = FPValue {
            fp_type: FPType::True
        };
        assert_eq!(parse(&mut v, "false").is_ok(), true);
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
    }

    #[test]
    fn test_parse_root_not_singular() {
        test_parse_error!("null x", ParseError::RootNotSingular);
        test_parse_error!("null ?", ParseError::RootNotSingular);
        test_parse_error!("null \r\n\tx", ParseError::RootNotSingular);

        let mut v = FPValue {
            fp_type: FPType::False
        };

        assert_eq!(parse(&mut v, "null  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::Null);

        assert_eq!(parse(&mut v, "\t true  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::True);
    }
}
