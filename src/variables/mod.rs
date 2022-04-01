use crate::ast::{BinaryOp, Block, Expr, Function, Identifier, Program, Statement};
use crate::codegen;
use crate::error::SourceMetadata;
use crate::error::{self, Span};
use crate::grammar::lexer::Source;
use std::collections::HashMap;
use std::fmt;

// the purpose of this is to walk the entire block looking for variables,
// and check all the expressions in case they have bad refs.
// furthermore, it's going to remove all the variables which are unused.

// the error is put through the pipeline before the data goes into the codegen unit.
// now, the codegen unit has to know the number of variables in advance.

// NOTE: all variables are currently integers. no type info is taken into account.

// NOTE: this is a result of the bad error formatting library I'm using.
#[derive(Debug)]
pub enum VarError {
    Undeclared(String),
    Redeclared(String),
}
impl fmt::Display for VarError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undeclared(name) => write!(f, "unknown variable: `{}`", name),
            Self::Redeclared(name) => {
                write!(f, "variable `{}` was already declared in this scope", name)
            }
        }
    }
}

impl std::error::Error for VarError {}

// fn walk_block<'code>(block: Vec<(Statement<'code>, Span)>, )

type IndexMap<'code> = HashMap<&'code str, usize>;

pub fn convert_program<'code>(
    program: Program<'code>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Program<'code>, error::Error<VarError>> {
    let Program(functions) = program;
    functions
        .into_iter()
        .map(|f| convert_function(f, source_meta))
        .collect::<Result<Vec<_>, _>>()
        .map(codegen::hlir::Program)
}

struct Env<'code> {
    pub offset: usize,
    pub max_offset: usize,
    pub indices: Vec<IndexMap<'code>>,
}

impl<'code> Env<'code> {
    pub fn new() -> Self {
        Self {
            offset: 0,
            max_offset: 0,
            indices: Vec::new(),
        }
    }

    fn current_map(&mut self) -> &mut IndexMap<'code> {
        if self.indices.is_empty() {
            self.push_map();
        }
        self.indices.last_mut().unwrap()
    }

    fn add(&mut self, name: &'code str) -> VarRes<usize> {
        let offset = self.offset;
        if self.current_map().insert(name, offset).is_some() {
            return Err(error::Error::new(VarError::Redeclared(name.to_string())));
        }
        self.offset += 1;
        self.max_offset = self.offset.max(self.max_offset);
        Ok(offset)
    }

    fn get(&self, name: &str) -> Option<usize> {
        self.indices
            .iter()
            .rev()
            .find_map(|map| map.get(name))
            .copied()
    }

    fn push_map(&mut self) {
        self.indices.push(IndexMap::new())
    }

    fn pop_map(&mut self) {
        self.offset -= self.indices.pop().expect("popping from empty env").len();
    }
}

fn convert_function<'code>(
    func: Function<'code>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Function<'code>, error::Error<VarError>> {
    let Function {
        name: Identifier(name),
        body: Block { statements: block },
    } = func;

    let mut env = Env::new();
    let body = assign_block_indices(block, &mut env, source_meta)?;

    Ok(codegen::hlir::Function {
        name,
        body,
        var_amt: env.max_offset,
    })
}

type VarRes<T> = Result<T, error::Error<VarError>>;

fn assign_block_indices<'code>(
    block: Vec<(Statement<'code>, Span)>,
    env: &mut Env<'code>,
    source_meta: &SourceMetadata,
) -> VarRes<Vec<codegen::hlir::Statement>> {
    env.push_map();
    let result = block
        .into_iter()
        .map(|(stmt, _)| assign_statement_indices(stmt, env, source_meta))
        .fold(Ok(Vec::new()), |v, other| {
            let mut v = v?;
            let other = other?;
            v.reserve(other.len());
            v.extend(other);
            Ok(v)
        });
    env.pop_map();
    result
}

fn assign_statement_indices<'code>(
    stmt: Statement<'code>,
    env: &mut Env<'code>,
    source_meta: &SourceMetadata,
) -> VarRes<Vec<codegen::hlir::Statement>> {
    match stmt {
        Statement::Return((expr, expr_span)) => {
            let ret_expr = assign_expr_indices(expr, env, source_meta)
                .map_err(|err| err.with_backup_source(expr_span, source_meta))?;
            Ok(vec![codegen::hlir::Statement::Return(ret_expr)])
        }
        Statement::SingleExpr((expr, expr_span)) => {
            let single_expr = assign_expr_indices(expr, env, source_meta)
                .map_err(|err| err.with_backup_source(expr_span, source_meta))?;

            Ok(vec![codegen::hlir::Statement::Single(single_expr)])
        }
        Statement::DeclareVar {
            name: Source { span, source: name },
            init,
        } => {
            // first, compile the init expr if it is there
            let init = init
                .map(|(expr, expr_span)| {
                    assign_expr_indices(expr, env, source_meta)
                        .map_err(|err| err.with_backup_source(expr_span, source_meta))
                })
                .transpose()?;

            // now we can safely add our variable to the block
            let index = env
                .add(name)
                .map_err(|err| err.with_backup_source(span, source_meta))?;

            // wrap the init in an assign statement and we're good to go!
            Ok(init
                .map(|init| {
                    codegen::hlir::Statement::Single(codegen::hlir::Expr::Binary {
                        operator: BinaryOp::Assignment { op: None },
                        lhs: Box::new(codegen::hlir::Expr::Variable { index }),
                        rhs: Box::new(init),
                        branch_depends_on_result: false,
                    })
                })
                .into_iter()
                .collect())
        }
        Statement::Block(block) => {
            let block = assign_block_indices(block, env, source_meta)?;
            Ok(block)
        }
        Statement::IfStatement {
            condition: (condition, condition_span),
            true_branch: (true_branch, _),
            false_branch,
        } => {
            let condition = assign_expr_indices(condition, env, source_meta)
                .map_err(|err| err.with_backup_source(condition_span, source_meta))?;
            let true_branch = assign_statement_indices(*true_branch, env, source_meta)?;
            let false_branch = false_branch
                .map(|(false_branch, _)| assign_statement_indices(*false_branch, env, source_meta))
                .transpose()?;
            Ok(vec![codegen::hlir::Statement::IfStatement {
                condition,
                true_branch,
                false_branch,
            }])
        }
    }
}

fn assign_expr_indices<'code>(
    expr: Expr<'code>,
    scope: &Env<'code>,
    source_meta: &SourceMetadata,
) -> VarRes<codegen::hlir::Expr> {
    match expr {
        Expr::Constant(ctant) => Ok(codegen::hlir::Expr::Constant(ctant)),
        Expr::Variable {
            name: Source {
                span: _,
                source: name,
            },
        } => {
            let index = scope
                .get(name)
                .ok_or_else(|| error::Error::new(VarError::Undeclared(name.to_string())))?;
            Ok(codegen::hlir::Expr::Variable { index })
        }
        Expr::Binary {
            operator,
            lhs: (lhs, lhs_span),
            rhs: (rhs, rhs_span),
        } => {
            let lhs = assign_expr_indices(*lhs, scope, source_meta)
                .map_err(|err| err.with_backup_source(lhs_span, source_meta))?;
            let rhs = assign_expr_indices(*rhs, scope, source_meta)
                .map_err(|err| err.with_backup_source(rhs_span, source_meta))?;
            Ok(codegen::hlir::Expr::Binary {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                branch_depends_on_result: false,
            })
        }
        Expr::Unary {
            operator,
            expr: (expr, expr_span),
        } => {
            let expr = assign_expr_indices(*expr, scope, source_meta)
                .map_err(|err| err.with_backup_source(expr_span, source_meta))?;
            Ok(codegen::hlir::Expr::Unary {
                operator,
                inner: Box::new(expr),
            })
        }
    }
}
