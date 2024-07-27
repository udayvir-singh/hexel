//! todo

use std::{
    fmt::{self, Display},
    num::NonZeroU64,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    ptr::addr_of_mut,
    str::FromStr,
};

use bigdecimal::{
    num_bigint::{Sign, ToBigInt},
    Context, Num, Signed, ToPrimitive,
};
use num_bigint::{BigInt as LBigInt, RandBigInt, Sign as LSign};
use rand::thread_rng;

pub use bigdecimal::{num_bigint::BigInt, BigDecimal, One, RoundingMode, Zero};

use super::consts::*;

/* -------------------- *
 *        MACROS        *
 * -------------------- */
macro_rules! impl_ops {
    ($name:ident ($field:ident), Neg :: neg) => {
        impl Neg for $name {
            type Output = Self;
            fn neg(self) -> Self {
                Self::new(Neg::neg(self.$field))
            }
        }
    };
    ($name:ident ($field:ident), $trait:ident :: $method:ident) => {
        impl $trait for $name {
            type Output = Self;
            fn $method(self, rhs: Self) -> Self {
                Self::new($trait::$method(self.$field, rhs.$field))
            }
        }
    };
    ($name:ident ($field:ident), $($trait:ident :: $method:ident),* $(,)?) => {
        $(impl_ops!($name($field), $trait :: $method);)*
    };
}


/* -------------------- *
 *        NUMBER        *
 * -------------------- */
/// todo
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Number {
    value: BigDecimal,
}

impl_ops!(
    Number(value),
    Add::add,
    Sub::sub,
    Mul::mul,
    Rem::rem,
    Neg::neg,
);

impl Div for Number {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self {
        self.div_with_scale(rhs, DEFAULT_NORMAL_MATH_SCALE).unwrap()
    }
}

impl Display for Number {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value.normalized())
    }
}

impl Zero for Number {
    #[inline]
    fn zero() -> Self {
        Self::new(BigDecimal::zero())
    }

    #[inline]
    fn is_zero(&self) -> bool {
        self.value.is_zero()
    }
}

impl One for Number {
    #[inline]
    fn one() -> Self {
        Self::new(BigDecimal::one())
    }
}

impl<T: Into<BigDecimal>> From<T> for Number {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value.into())
    }
}

impl FromStr for Number {
    type Err = String;

    #[inline]
    fn from_str(source: &str) -> Result<Self, Self::Err> {
        NumberParser::new(source.chars().collect()).parse()
    }
}

impl Number {
    /* -------------------- *
     *          NEW         *
     * -------------------- */
    /// todo
    pub fn new(value: BigDecimal) -> Self {
        Self { value }
    }


    /* -------------------- *
     *         INTO         *
     * -------------------- */
    /// todo
    pub fn into_bigdecimal(self) -> BigDecimal {
        self.value
    }

    /// todo
    pub fn to_biginteger(&self) -> Result<BigInt, String> {
        if self.value.is_integer() {
            Ok(self.value.to_bigint().unwrap())
        } else {
            Err(format!("Integer cannot have fractional part: {}", self))
        }
    }

    /// todo
    pub fn to_integer(&self) -> Result<i64, String> {
        let big_int = self.to_biginteger()?;

        big_int.to_i64().ok_or(format!("Integer too large for 64 bits: {}", big_int))
    }

    /// todo
    pub fn to_clamped_integer(&self) -> Result<i64, String> {
        let big_int = self.to_biginteger()?;

        Ok(big_int.clamp(i64::MIN.into(), i64::MAX.into()).to_i64().unwrap())
    }


    /* -------------------- *
     *       MAPPINGS       *
     * -------------------- */
    /// todo
    pub fn is_positive(&self) -> bool {
        self.value.is_positive()
    }

    /// todo
    pub fn is_negative(&self) -> bool {
        self.value.is_negative()
    }

    /// todo
    pub fn is_integer(&self) -> bool {
        self.value.is_integer()
    }

    /// todo
    pub fn int_digits(&self) -> i64 {
        self.value.digits() as i64 - self.value.fractional_digit_count()
    }

    /// todo
    pub fn abs(&self) -> Self {
        Self::new(self.value.abs())
    }

    /// todo
    pub fn signum(&self) -> Self {
        Self::new(self.value.signum())
    }

    /// todo
    pub fn floor(&self) -> Self {
        Self::new(self.value.with_scale_round(0, RoundingMode::Floor))
    }

    /// todo
    pub fn ceil(&self) -> Self {
        Self::new(self.value.with_scale_round(0, RoundingMode::Ceiling))
    }

    /// todo
    pub fn round(&self, scale: i64, mode: RoundingMode) -> Self {
        Self::new(self.value.with_scale_round(scale, mode))
    }

    /// todo
    pub fn with_scale(&self, scale: i64) -> Self {
        Self::new(self.value.with_scale(scale))
    }

    /// todo
    pub fn square(&self) -> Self {
        Self::new(self.value.square())
    }

    /// todo
    pub fn sqrt(&self, scale: i64) -> Option<Self> {
        let precision = (self.int_digits() / 2) + scale.max(0) + 5;
        let context = Context::new(
            NonZeroU64::new(precision as u64).unwrap(),
            RoundingMode::Floor,
        );

        self.value
            .sqrt_with_context(&context)
            .map(|x| Self::new(x.with_scale(scale)))
    }

    /// todo
    pub fn cube(&self) -> Self {
        Self::new(self.value.cube())
    }

    /// todo
    pub fn cbrt(&self, scale: i64) -> Self {
        let precision = (self.int_digits() / 2) + scale.max(0) + 5;
        let context = Context::new(
            NonZeroU64::new(precision as u64).unwrap(),
            RoundingMode::Floor,
        );

        Self::new(self.value.cbrt_with_context(&context).with_scale(scale))
    }

    /// todo
    pub fn div_with_scale(self, other: Self, scale: i64) -> Option<Self> {
        if other.is_zero() {
            None
        } else {
            Some(Self::new(div_bigdecimal(self.value, other.value, scale)))
        }
    }

    /// todo
    pub fn inverse(self, scale: i64) -> Option<Self> {
        Self::one().div_with_scale(self, scale)
    }

    /// todo
    pub fn min(self, other: Self) -> Self {
        Self::new(self.value.min(other.value))
    }

    /// todo
    pub fn max(self, other: Self) -> Self {
        Self::new(self.value.max(other.value))
    }

    /// todo
    pub fn clamp(self, min: Self, max: Self) -> Self {
        Self::new(self.value.clamp(min.value, max.value))
    }


    /* -------------------- *
     *    EXPONENTIATION    *
     * -------------------- */
    /// todo
    pub fn exp(self, scale: i64) -> Self {
        if self.is_negative() {
            // handle negative value: e^(-x) = (1/e)^x
            let e_inv = Self::new(parse_constant(BIG_E_INV, scale));

            e_inv.pow(self.abs(), scale)
        } else if self.is_integer() {
            // calculate: e^n = e * e * ... * e (n times)
            let integer = self.to_clamped_integer().unwrap() as usize;
            let max_scale = scale + pow_bigdecimal(3.into(), integer, 0).digits() as i64;
            let base = parse_constant(BIG_E, max_scale);

            Self::new(pow_bigdecimal(base, integer, max_scale).with_scale(scale))
        } else if self > Self::one() {
            // calculate: e^(n+f) = e^n * e^f where n > 1 and 0 < f < 1
            let integer = self.with_scale(0);
            let fraction = Self::new(&self.value - &integer.value);

            let a = integer.exp(scale + 5);
            let b = fraction.exp(scale + a.value.digits() as i64);

            (a * b).with_scale(scale)
        } else {
            // calculate: e^f where 0 < f < 1 using Taylor series
            let mut result = BigDecimal::one();
            let mut prev_result = result.clone();
            let mut pow = BigDecimal::one();

            for n in 1.. {
                pow *= &self.value;
                result += div_bigdecimal(pow.clone(), factorial(n), scale + 5);

                if prev_result == result {
                    break;
                }

                prev_result = result.clone();
            }

            Self::new(prev_result.with_scale(scale))
        }
    }

    /// todo
    pub fn pow(self, exponent: Self, scale: i64) -> Self {
        if exponent.is_negative() {
            // handle negative value: x^(-y) = (1/x)^y
            self.inverse(scale).unwrap().pow(exponent.abs(), scale)
        } else if exponent.is_integer() {
            // calculate: x^n = x * x * ... * x (n times)
            let integer = exponent.to_clamped_integer().unwrap() as usize;
            let max_scale = scale + pow_bigdecimal(
                self.value.with_scale_round(0, RoundingMode::Up), integer, 0,
            ).digits() as i64;

            Self::new(pow_bigdecimal(self.value, integer, max_scale).with_scale(scale))
        } else if exponent > Self::one() {
            // calculate: x^(n+f) = x^n * x^f where n > 1 and 0 < f < 1
            let integer = exponent.with_scale(0);
            let fraction = Self::new(exponent.value - &integer.value);

            let a = self.clone().pow(integer, scale);
            let b = self.pow(fraction, scale + a.value.digits() as i64);

            (a * b).with_scale(scale)
        } else {
            // calculate: x^f = e^(f*ln(x)) where 0 < f < 1
            let max_scale = scale + 5;
            let ln_x = self.ln(max_scale).unwrap();

            (exponent * ln_x).exp(max_scale)
                .round(max_scale - 3, RoundingMode::HalfDown)
                .with_scale(scale)
        }
    }


    /* -------------------- *
     *       LOGARITHM      *
     * -------------------- */
    /// todo
    pub fn ln(self, scale: i64) -> Result<Self, String> {
        // early return for invalid values
        if self.value.is_negative() {
            return Err(format!(
                "Cannot calculate logarithm of a negative number: {}",
                self,
            ));
        }

        if self.value.is_zero() {
            return Err("Cannot calculate logarithm for zero".to_string());
        }

        // shortcut for known values
        if self.value == 2.into() {
            return Ok(Self::new(parse_constant(BIG_LN_2, scale)));
        }

        if self.value == 10.into() {
            return if scale < 0 {
                Ok(Self::zero())
            } else {
                Ok(Self::new(parse_constant(BIG_LN_10, scale)))
            };
        }

        // optimize large value: ln(a * 10^n) = ln(a) + n*ln(10)
        if self.value > 10.into() {
            let (mantissa, exponent) = self.clone().into_mantissa_and_exponent();

            let s = scale + (exponent.ilog10() + 5) as i64;
            let a = Self::from_str_unchecked(&mantissa).ln(s).unwrap();
            let b = Self::new(exponent * parse_constant(BIG_LN_10, s));

            return Ok((a + b).with_scale(scale));
        }

        // parse constants
        let e = parse_constant(BIG_E, scale + 5);
        let e_inv = parse_constant(BIG_E_INV, scale + 5);

        // shrink input value: x < e and x >= 1
        let mut value = self.value;
        let mut count = BigDecimal::zero();

        while value >= e {
            value *= &e_inv;
            count += 1;
        }

        while value < BigDecimal::one() {
            value *= &e;
            count -= 1;
        }

        // calculate: y = (x - 1) / (x + 1) where x >= 1
        let y = div_bigdecimal(&value - 1, &value + 1, scale + 5);
        let y_sq = y.clone() * &y;

        // calculate: ln(x) using Taylor series
        let mut pow = y.clone();
        let mut result = pow.clone();
        let mut prev_result = result.clone();

        for k in 1.. {
            pow *= &y_sq;
            result += &pow / ((2 * k) + 1);

            pow = pow.with_scale(scale + 5);
            result = result.with_scale(scale + 5);

            let trimmed_result = result.with_scale(scale + 1);

            if prev_result == trimmed_result {
                break;
            }

            prev_result = trimmed_result;
        }

        Ok(Self::new(count + (2 * result)).with_scale(scale))
    }

    /// todo
    pub fn log(self, base: Self, scale: i64) -> Result<Self, String> {
        // early return on invalid bases
        if base.is_negative() {
            return Err(format!(
                "Cannot calculate logarithm with a negative base of {}: {}",
                base,
                self,
            ));
        }

        if base.is_zero() {
            return Err(format!(
                "Cannot calculate logarithm with zero as base: {}",
                self,
            ));
        }

        if base.is_one() {
            return Err(format!(
                "Cannot calculate logarithm with one as base: {}",
                self,
            ));
        }

        // shortcut for known value: log_b(b) = 1
        if self.value == base.value {
            return Ok(Self::one());
        }

        // calculate: log_b(x) = ln(x) / ln(b)
        let ln_b = base.ln(scale + 5).unwrap();
        let ln_x = self.ln(scale + 5)?;

        Ok(ln_x.div_with_scale(ln_b, scale).unwrap())
    }


    /* -------------------- *
     *     TRIGONOMETRY     *
     * -------------------- */
    /// todo
    pub fn rad(self, scale: i64) -> Self {
        let constant = parse_constant(BIG_PI_BY_180, scale + self.int_digits());

        Self::new((self.value * constant).with_scale(scale))
    }

    /// todo
    pub fn deg(self, scale: i64) -> Self {
        let constant = parse_constant(BIG_180_BY_PI, scale + self.int_digits());

        Self::new((self.value * constant).with_scale(scale))
    }

    /// todo
    pub fn sin(self, scale: i64) -> Self {
        // shortcut for known value: sin(0) = 0
        if self.is_zero() {
            return Self::zero();
        }

        // handle negative value: sin(-x) = -sin(x)
        if self.is_negative() {
            return self.abs().sin(scale).neg();
        }

        // optimize large value: sin(2pi * x) = sin(x)
        let int_digits = self.int_digits();
        let two_pi = parse_constant(BIG_2PI, scale + int_digits);

        if self.value >= two_pi {
            return Self::new(self.value % two_pi).sin(scale);
        }

        // optimize large value: sin(x) = -sin(x - pi) where x >= pi
        let pi = parse_constant(BIG_PI, scale + int_digits);

        if self.value >= pi {
            return Self::new(self.value - pi).sin(scale).neg();
        }

        // calculate Taylor series for sin wave
        self.trig_taylor(1, scale + 5)
            .round(scale + 2, RoundingMode::HalfDown)
            .with_scale(scale)
    }

    /// todo
    pub fn cos(self, scale: i64) -> Self {
        // shortcut for known value: cos(0) = 1
        if self.is_zero() {
            return Self::one();
        }

        // handle negative value: cos(-x) = cos(x)
        if self.is_negative() {
            return self.abs().cos(scale);
        }

        // optimize large value: cos(2pi * x) = cos(x)
        let int_digits = self.int_digits();
        let two_pi = parse_constant(BIG_2PI, scale + int_digits);

        if self.value >= two_pi {
            return Self::new(&self.value % two_pi).cos(scale);
        }

        // optimize large value: cos(x) = -cos(x - pi) where x >= pi
        let pi = parse_constant(BIG_PI, scale + int_digits);

        if self.value >= pi {
            return Self::new(self.value - pi).cos(scale).neg();
        }

        // calculate Taylor series for cos wave
        self.trig_taylor(0, scale + 5)
            .round(scale + 2, RoundingMode::HalfDown)
            .with_scale(scale)
    }

    /// todo
    pub fn tan(self, scale: i64) -> Option<Self> {
        // shortcut for known value: tan(0) = 0
        if self.is_zero() {
            return Some(Self::zero());
        }

        // calculate: tan(x) = sin(x) / cos(x)
        let cos = self.clone().cos(scale + 1);

        if cos.with_scale(scale).is_zero() {
            None
        } else {
            self.sin(scale + 1).div_with_scale(cos, scale)
        }
    }


    /* -------------------- *
     *        RANDOM        *
     * -------------------- */
    /// todo
    pub fn random(&self, other: &Self) -> Result<Self, String> {
        // utils
        let to_lint = |x: &Self| {
            let (sign, uint) = x
                .with_scale(0)
                .value
                .to_bigint()
                .unwrap()
                .into_parts();

            let lsign = match sign {
                Sign::Minus => LSign::Minus,
                Sign::NoSign => LSign::NoSign,
                Sign::Plus => LSign::Plus,
            };

            LBigInt::from_slice(lsign, &uint.to_u32_digits())
        };

        let to_number = |x: LBigInt| {
            let (lsign, luint) = x.into_parts();

            let sign = match lsign {
                LSign::Minus => Sign::Minus,
                LSign::NoSign => Sign::NoSign,
                LSign::Plus => Sign::Plus,
            };

            Number::new(BigDecimal::new(
                BigInt::from_slice(sign, &luint.to_u32_digits()),
                0,
            ))
        };

        // parse bounds
        let lbound = to_lint(self);
        let rbound = to_lint(other) + LBigInt::one();

        if lbound >= rbound {
            return Err(format!(
                "Expected trunc(lbound: {}) <= trunc(rbound: {}) for RNG",
                self, other,
            ))
        }

        // generate random number
        let number = thread_rng().gen_bigint_range(&lbound, &rbound);

        Ok(to_number(number))
    }


    /* -------------------- *
     *         UTILS        *
     * -------------------- */
    fn from_str_unchecked(src: &str) -> Self {
        Self::new(BigDecimal::from_str(src).unwrap())
    }

    fn into_mantissa_and_exponent(self) -> (String, i64) {
        let (big_int, scale) = self.value.into_bigint_and_exponent();
        let digits = big_int.to_string();

        let mantissa = [&digits[..1], ".", &digits[1..]].concat();
        let exponent = (digits.len() as i64 - 1) - scale;

        (mantissa, exponent)
    }

    fn trig_taylor(&self, exp_offset: usize, scale: i64) -> Self {
        let mut result = BigDecimal::zero();
        let mut prev_result = result.clone();
        let mut prev_exp = 0;
        let mut prev_pow = BigDecimal::one();

        for n in 0.. {
            let exp = 2 * n + exp_offset;
            let mut pow = prev_pow;

            for _ in 0..exp - prev_exp {
                pow *= &self.value;
            }

            let element = div_bigdecimal(pow.clone(), factorial(exp), scale);

            if n & 1 == 0 {
                result += element;
            } else {
                result -= element;
            }

            let trimmed_result = result.with_scale(scale);

            if prev_result == trimmed_result {
                break;
            }

            prev_exp = exp;
            prev_pow = pow;
            prev_result = trimmed_result;
        }

        Self::new(prev_result)
    }
}


/* -------------------- *
 *        PARSER        *
 * -------------------- */
struct NumberParser {
    source: Vec<char>,
    cursor: usize,
}

impl NumberParser {
    /* -------------------- *
     *       INTERFACE      *
     * -------------------- */
    fn new(source: Vec<char>) -> Self {
        Self { source, cursor: 0 }
    }

    fn parse(mut self) -> Result<Number, String> {
        // parse sign
        let sign = self.parse_sign();

        // parse number
        match self.peek_chars(2) {
            // handle non-decimal number
            ['0', prefix @ ('x' | 'X' | 'o' | 'O' | 'b' | 'B')] => {
                let prefix = *prefix;

                self.inc_cursor(2);

                if self.active() {
                    self.parse_non_decimal(sign, prefix)
                } else {
                    Err(format!(
                        "Missing digits after prefix in number {:?}",
                        self.stringify_source()
                    ))
                }
            }
            // handle invalid prefix
            ['0', prefix @ ('a'..='d' | 'f'..='z' | 'A'..='D' | 'F'..='Z')] => {
                Err(format!(
                    "Invalid prefix \"0{}\" at start of number {:?}",
                    prefix,
                    self.stringify_source(),
                ))
            }
            // handle decimal number
            ['0'..='9' | '.' | ',' | '_', ..] => {
                self.parse_decimal(sign)
            }
            // handle invalid number
            [char, ..] => {
                Err(format!(
                    "Invalid character \"{}\" at start of number {:?}",
                    char,
                    self.stringify_source(),
                ))
            },
            // handle empty string
            [] => {
                Err("Cannot parse empty string as number".to_string())
            }
        }
    }


    /* -------------------- *
     *      NON DECIMAL     *
     * -------------------- */
    fn parse_non_decimal(&mut self, sign: String, prefix: char) -> Result<Number, String> {
        // handle prefix based variables
        let (radix, digits_cond): (_, fn(char) -> bool) = match prefix {
            'x' | 'X' => (16, |x| matches!(x, '0'..='9' | 'a'..='f' | 'A'..='F')),
            'o' | 'O' => (8, |x| matches!(x, '0'..='7')),
            'b' | 'B' => (2, |x| matches!(x, '0' | '1')),
            _ => unreachable!(),
        };

        // collect digits
        let digits = self.collect_digits(digits_cond)?;

        // handle invalid characters
        if self.active() {
            let rest: String = self.source[self.cursor..].iter().collect();

            return Err(format!(
                "Invalid character{} {:?} at the end of number {:?}",
                if rest.len() > 1 { "s" } else { "" },
                rest,
                self.stringify_source(),
            ));
        }

        // generate number
        let number = [sign.to_string(), digits].concat();
        let big_int = BigInt::from_str_radix(&number, radix).unwrap();

        Ok(Number::new(BigDecimal::new(big_int, 0)))
    }


    /* -------------------- *
     *        DECIMAL       *
     * -------------------- */
    fn parse_decimal(&mut self, sign: String) -> Result<Number, String> {
        // collect integer
        let integer = self.collect_digits(|x| matches!(x, '0'..='9'))?;

        // collect fraction
        let mut fraction = if self.active() && self.peek_char() == '.' {
            self.inc_cursor(1);
            self.collect_digits(|x| matches!(x, '0'..='9'))?
        } else {
            String::new()
        };

        while fraction.ends_with('0') {
            fraction.pop();
        }

        // validate digits
        if integer.is_empty() && fraction.is_empty() {
            return Err(format!(
                "Missing digits in number {:?}",
                self.stringify_source()
            ));
        }

        // parse exponent
        let exponent: i64 = if self.active() && matches!(self.peek_char(), 'e' | 'E') {
            self.inc_cursor(1);
            self.parse_exponent()?
        } else {
            0
        };

        // handle invalid characters
        if self.active() {
            let rest: String = self.source[self.cursor..].iter().collect();

            return Err(format!(
                "Invalid character{} {:?} at the end of number {:?}",
                if rest.len() > 1 { "s" } else { "" },
                rest,
                self.stringify_source(),
            ));
        }

        // generate number
        let scale = fraction.len() as i64 - exponent;
        let number = [sign, integer, fraction].concat();
        let big_int = BigInt::from_str(&number).unwrap();

        Ok(Number::new(BigDecimal::new(big_int, scale)))
    }

    fn parse_exponent(&mut self) -> Result<i64, String> {
        // parse sign
        let sign = self.parse_sign();

        // collect exponent
        let exponent = self.collect_digits(|x| matches!(x, '0'..='9'))?;

        // validate exponent
        if exponent.is_empty() {
            let err = if self.active() {
                let rest: String = self.source[self.cursor..].iter().collect();

                format!(
                    "Invalid character{} {:?} after exponent in number {:?}",
                    if rest.len() > 1 { "s" } else { "" },
                    rest,
                    self.stringify_source(),
                )
            } else {
                format!(
                    "Missing digits after exponent in number {:?}",
                    self.stringify_source(),
                )
            };

            return Err(err);
        }

        // parse exponent
        Ok([sign, exponent].concat().parse::<i64>().unwrap())
    }


    /* -------------------- *
     *         UTILS        *
     * -------------------- */
    fn stringify_source(&self) -> String {
        self.source.iter().collect()
    }

    fn active(&self) -> bool {
        self.cursor < self.source.len()
    }

    fn inc_cursor(&mut self, n: usize) {
        self.cursor += n;
    }

    fn peek_char(&self) -> char {
        self.source[self.cursor]
    }

    fn peek_chars(&self, n: usize) -> &[char] {
        &self.source[self.cursor..(self.cursor + n).min(self.source.len())]
    }

    fn next_char(&mut self) -> char {
        let char = self.source[self.cursor];
        self.inc_cursor(1);
        char
    }

    fn parse_sign(&mut self) -> String {
        let mut sign = '+';

        while self.active() {
            match (sign, self.peek_char()) {
                (_, '+') => (),
                ('+', '-') => sign = '-',
                ('-', '-') => sign = '+',
                _ => break,
            }

            self.inc_cursor(1);
        }

        sign.to_string()
    }

    fn collect_digits(&mut self, cond: fn(char) -> bool) -> Result<String, String> {
        let is_sep_char = |x| matches!(x, ',' | '_');
        let is_sep_byte = |x| matches!(x, b',' | b'_');
        let mut digits = String::new();

        while self.active() && (cond(self.peek_char()) || is_sep_char(self.peek_char())) {
            let char = self.next_char();

            digits.push(char);

            if self.active() && is_sep_char(char) && is_sep_char(self.peek_char()) {
                return Err(format!(
                    "Cannot have trailing separator \"{}\" after {:?} in number {:?}",
                    self.peek_char(),
                    digits,
                    self.stringify_source(),
                ));
            }
        }

        if digits.as_bytes().first().cloned().is_some_and(is_sep_byte) {
            return Err(format!(
                "Cannot have separator at start of digits {:?} in number {:?}",
                digits,
                self.stringify_source(),
            ));
        }

        if digits.as_bytes().last().cloned().is_some_and(is_sep_byte) {
            return Err(format!(
                "Cannot have separator at end of digits {:?} in number {:?}",
                digits,
                self.stringify_source(),
            ));
        }

        Ok(digits.chars().filter(|x| !is_sep_char(*x)).collect())
    }
}


/* -------------------- *
 *         UTILS        *
 * -------------------- */
fn parse_constant(raw: &str, scale: i64) -> BigDecimal {
    let int_digits = raw.bytes().take_while(|x| *x != b'.').count();
    let constant = &raw[..(int_digits + 1 + scale.max(0) as usize).min(raw.len())];

    BigDecimal::from_str(constant).unwrap()
}

fn factorial(x: usize) -> BigDecimal {
    static mut STORE: Vec<BigDecimal> = Vec::new();

    let store = unsafe { &mut *addr_of_mut!(STORE) };

    if store.len() == 0 {
        store.push(BigDecimal::one());
    }

    if store.len() <= x {
        for i in store.len()..=x {
            store.push(i as u64 * &store[i - 1]);
        }
    }

    store[x].clone()
}

fn div_bigdecimal(x: BigDecimal, y: BigDecimal, scale: i64) -> BigDecimal {
    let (xi, xs) = x.into_bigint_and_exponent();
    let (yi, ys) = y.into_bigint_and_exponent();

    impl_division(xi, yi, xs - ys, scale)
}

fn pow_bigdecimal(mut base: BigDecimal, mut exponent: usize, max_scale: i64) -> BigDecimal {
    if exponent.is_zero() {
        return BigDecimal::one();
    }

    if base.is_zero() || base.is_one() {
        return base;
    }

    while exponent & 1 == 0 {
        base *= base.clone();
        base = base.with_scale(max_scale);
        exponent >>= 1;
    }

    let mut result = base.clone();

    while exponent > 1 {
        exponent >>= 1;
        base *= base.clone();
        base = base.with_scale(max_scale);

        if exponent & 1 == 1 {
            result *= base.clone();
            result = result.with_scale(max_scale);
        }
    }

    result
}


/* -------------------- *
 *         PATCH        *
 * -------------------- */
/// Copied from lib.rs in bigdecimal crate and modified to support final_scale parameter.
#[allow(clippy::all)]
fn impl_division(mut num: BigInt, den: BigInt, mut initial_scale: i64, final_scale: i64) -> BigDecimal {
    // quick zero check
    if num.is_zero() {
        return BigDecimal::new(num, 0);
    }

    match (num.is_negative(), den.is_negative()) {
        (true, true) => return impl_division(num.neg(), den.neg(), initial_scale, final_scale),
        (true, false) => return -impl_division(num.neg(), den, initial_scale, final_scale),
        (false, true) => return -impl_division(num, den.neg(), initial_scale, final_scale),
        (false, false) => (),
    }

    // shift digits until numerator is larger than denominator (set scale appropriately)
    while num < den {
        initial_scale += 1;
        num *= 10;
    }

    // first division
    let mut quotient = &num / &den;
    let mut remainder = num % &den;

    // division complete
    if remainder.is_zero() {
        return BigDecimal::new(quotient, initial_scale);
    }

    // shift remainder by 1 decimal;
    // quotient will be 1 digit upon next division
    remainder *= 10;

    while !remainder.is_zero() && initial_scale <= final_scale {
        let q = &remainder / &den;
        let r = remainder % &den;
        quotient = quotient * 10 + q;
        remainder = r * 10;
        initial_scale += 1;
    }

    return BigDecimal::new(quotient, initial_scale).with_scale(final_scale);
}
