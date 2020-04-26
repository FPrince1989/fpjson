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

    fn parse_null(&mut self, value: &mut FPValue) -> Result<()> {
        if self.json.len() >= 4 && &self.json[0..4] == "null" {
            self.json = &self.json[4..self.json.len()];
            value.fp_type = FPType::Null;

            Result::Ok(())
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    fn parse_true(&mut self, value: &mut FPValue) -> Result<()> {
        if self.json.len() >= 4 && &self.json[0..4] == "true" {
            self.json = &self.json[4..self.json.len()];
            value.fp_type = FPType::True;

            Result::Ok(())
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    fn parse_false(&mut self, value: &mut FPValue) -> Result<()> {
        if self.json.len() >= 5 && &self.json[0..5] == "false" {
            self.json = &self.json[5..self.json.len()];
            value.fp_type = FPType::False;

            Result::Ok(())
        } else {
            Result::Err(ParseError::InvalidValue)
        }
    }

    pub fn parse_value(&mut self, value: &mut FPValue) -> Result<()> {
        if let Some(c) = self.json.chars().next() {
            match c {
                'n' => self.parse_null(value),
                't' => self.parse_true(value),
                'f' => self.parse_false(value),
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
        let mut v = FPValue {
            fp_type: FPType::False
        };

        assert_eq!(parse(&mut v, "").unwrap_err(), ParseError::ExpectValue);
        assert_eq!(v.fp_type, FPType::Null);

        v.fp_type = FPType::False;
        assert_eq!(parse(&mut v, " \r \t \n").unwrap_err(), ParseError::ExpectValue);
        assert_eq!(v.fp_type, FPType::Null);
    }

    #[test]
    fn test_parse_invalid_value() {
        let mut v = FPValue {
            fp_type: FPType::False
        };


        assert_eq!(parse(&mut v, "nul").unwrap_err(), ParseError::InvalidValue);
        assert_eq!(v.fp_type, FPType::Null);

        v.fp_type = FPType::False;
        assert_eq!(parse(&mut v, "?").unwrap_err(), ParseError::InvalidValue);
        assert_eq!(v.fp_type, FPType::Null);
    }

    #[test]
    fn test_parse_root_not_singular() {
        let mut v = FPValue {
            fp_type: FPType::False
        };


        assert_eq!(parse(&mut v, "null x").unwrap_err(), ParseError::RootNotSingular);
        assert_eq!(v.fp_type, FPType::Null);

        assert_eq!(parse(&mut v, "null  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::Null);

        assert_eq!(parse(&mut v, "\t true  \r\n").is_ok(), true);
        assert_eq!(v.fp_type, FPType::True);
    }
}
