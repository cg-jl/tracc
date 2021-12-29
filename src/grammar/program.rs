use super::{Parse, ParseRes, Parser};
use crate::ast::{Function, Program};

impl<'source> Parse<'source> for Program<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        Function::parse(parser).map(Program)
    }
}
