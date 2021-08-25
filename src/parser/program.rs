use super::{Parse, Parser, ParseRes};
use crate::ast::{Function, Program};

impl Parse for Program {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        Function::parse(parser).map(Program)
    }
}
