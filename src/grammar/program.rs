use super::{Parse, ParseRes, Parser};
use crate::ast::Program;

impl<'source> Parse<'source> for Program<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        let mut vec = Vec::new();
        // TODO: redefinitions.
        while parser.peek_token()?.is_some() {
            vec.push(parser.parse()?);
        }
        Ok(Program(vec))
    }
}
