extern crate fpjson;

use fpjson::{parse, JsonValue, ParseError};
use std::collections::HashMap;

macro_rules! test_parse_error {
    ($json:expr, $error:expr) => {
        assert_eq!(parse($json).unwrap_err(), $error);
    };
}

macro_rules! test_parse_number {
    ($json:expr, $number:expr) => {
        let result = parse($json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), JsonValue::Number($number));
    };
}

macro_rules! test_parse_string {
    ($json:expr, $expect_str:expr) => {
        let result = parse($json);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            JsonValue::String(String::from($expect_str))
        );
    };
}

macro_rules! test_parse_array {
    ($json:expr, $vec:expr) => {
        let result = parse($json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), JsonValue::Array($vec));
    };
}

macro_rules! test_parse_object {
    ($json:expr, $map:expr) => {
        let result = parse($json);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), JsonValue::Object($map));
    };
}

macro_rules! test_roundtrip {
    ($json:expr) => {
        let result = parse($json).unwrap();
        let t = result.stringify();
        let result2 = parse(t.as_str()).unwrap();
        assert_eq!(result, result2);
    };
}

#[test]
fn test_parse_null() {
    let result = parse("null");
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), JsonValue::Null);

    let result = parse("null \r\t");
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), JsonValue::Null);
}

#[test]
fn test_parse_true() {
    let result = parse("true");
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), JsonValue::True);

    let result = parse("\t true  \r\n");
    assert_eq!(result.is_ok(), true);
    assert_eq!(result.unwrap(), JsonValue::True);
}

#[test]
fn test_parse_false() {
    let result = parse("false");
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), JsonValue::False);

    let result = parse("\t false  \r\n");
    assert_eq!(result.is_ok(), true);
    assert_eq!(result.unwrap(), JsonValue::False);
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

#[allow(clippy::cognitive_complexity)]
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

#[allow(clippy::cognitive_complexity)]
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
            JsonValue::Number(1.0),
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::Number(2.0),
                JsonValue::String("3".to_string())
            ])
        ]
    );
    test_parse_array!(
        "[ null , false , true , 123 , \"abc\" ]",
        vec![
            JsonValue::Null,
            JsonValue::False,
            JsonValue::True,
            JsonValue::Number(123.0),
            JsonValue::String("abc".to_string())
        ]
    );
    test_parse_array!(
        "[ [ ] , [ 0 ] , [ 0 , 1 ] , [ 0 , 1 , 2 ] ]",
        vec![
            JsonValue::Array(vec![]),
            JsonValue::Array(vec![JsonValue::Number(0.0)]),
            JsonValue::Array(vec![JsonValue::Number(0.0), JsonValue::Number(1.0)]),
            JsonValue::Array(vec![
                JsonValue::Number(0.0),
                JsonValue::Number(1.0),
                JsonValue::Number(2.0)
            ]),
        ]
    );
    test_parse_array!(
        "[ [ ] , [ 0 ] , [ 0 , 1 ] , [ 0 , 1 , 2 ], { \t } ]",
        vec![
            JsonValue::Array(vec![]),
            JsonValue::Array(vec![JsonValue::Number(0.0)]),
            JsonValue::Array(vec![JsonValue::Number(0.0), JsonValue::Number(1.0)]),
            JsonValue::Array(vec![
                JsonValue::Number(0.0),
                JsonValue::Number(1.0),
                JsonValue::Number(2.0)
            ]),
            JsonValue::Object(HashMap::new()),
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
    test_parse_object!("{ }", HashMap::new());

    let mut result = HashMap::new();
    result.insert("n".to_string(), JsonValue::Null);
    result.insert("f".to_string(), JsonValue::False);
    result.insert("t".to_string(), JsonValue::True);
    result.insert("i".to_string(), JsonValue::Number(123.0));
    result.insert("s".to_string(), JsonValue::String("abc".to_string()));
    result.insert(
        "a".to_string(),
        JsonValue::Array(vec![
            JsonValue::Number(1.0),
            JsonValue::Number(2.0),
            JsonValue::Number(3.0),
        ]),
    );
    let mut sub_map = HashMap::new();
    sub_map.insert("1".to_string(), JsonValue::Number(1.0));
    sub_map.insert("2".to_string(), JsonValue::Number(2.0));
    sub_map.insert("3".to_string(), JsonValue::Number(3.0));
    result.insert("o".to_string(), JsonValue::Object(sub_map));
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

#[test]
fn test_stringify_literal() {
    test_roundtrip!("null");
    test_roundtrip!("true");
    test_roundtrip!("false");
}

#[test]
fn test_stringify_number() {
    test_roundtrip!("0");
    test_roundtrip!("-0");
    test_roundtrip!("1");
    test_roundtrip!("-1");
    test_roundtrip!("1.5");
    test_roundtrip!("-1.5");
    test_roundtrip!("3.25");
    test_roundtrip!("1e+20");
    test_roundtrip!("1.234e+20");
    test_roundtrip!("1.234e-20");
    test_roundtrip!("1.0000000000000002"); /* the smallest number > 1 */
    test_roundtrip!("4.9406564584124654e-324"); /* minimum denormal */
    test_roundtrip!("-4.9406564584124654e-324");
    test_roundtrip!("2.2250738585072009e-308"); /* Max subnormal double */
    test_roundtrip!("-2.2250738585072009e-308");
    test_roundtrip!("2.2250738585072014e-308"); /* Min normal positive double */
    test_roundtrip!("-2.2250738585072014e-308");
    test_roundtrip!("1.7976931348623157e+308"); /* Max double */
    test_roundtrip!("-1.7976931348623157e+308");
}

#[test]
fn test_stringify_string() {
    test_roundtrip!("\"hello 𝄞 world\"");
    test_roundtrip!("\"\"");
    test_roundtrip!("\"Hello\"");
    test_roundtrip!("\"Hello\\nWorld\"");
    test_roundtrip!("\"\\\" \\\\ / \\b \\f \\n \\r \\t\"");
    test_roundtrip!("\"Hello\\u0000World\"");
    test_roundtrip!("\"Hello\\u0019World\"");
}

#[test]
fn test_stringify_array() {
    test_roundtrip!("[1,2,3]");
    test_roundtrip!("[1,[2,null,[true],false],3]");
    test_roundtrip!("[]");
    test_roundtrip!("[null,false,true,123,\"abc\",[1,2,3]]");
}

#[test]
fn test_stringify_object() {
    test_roundtrip!("{\"1\":1}");
    test_roundtrip!("{}");
    test_roundtrip!("{\"n\":null,\"f\":false,\"t\":true,\"i\":123,\"s\":\"abc\",\"a\":[1,2,3],\"o\":{\"1\":1,\"2\":2,\"3\":3}}");
}
