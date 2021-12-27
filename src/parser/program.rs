use super::{Parse, ParseRes, Parser};
use crate::ast::{Function, Program};

impl<'source> Parse<'source> for Program {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        Function::parse(parser).map(Program)
    }
}
