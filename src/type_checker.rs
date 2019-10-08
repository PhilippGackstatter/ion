use crate::types::{Declaration, Expression, Program, Statement, Token};

#[derive(Debug)]
pub struct TypeCheckerError {
    pub token: Token,
    pub message: String,
}

#[derive(Debug)]
enum Type {
    Integer(usize),
    Str(usize),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;

        match (self, other) {
            (&Integer(_), &Integer(_)) => true,
            (&Str(_), &Str(_)) => true,
            _ => false,
        }
    }
}

impl Type {
    fn get_token_index(&self) -> usize {
        match self {
            Type::Integer(index) => *index,
            Type::Str(index) => *index,
        }
    }
}

type TypeResult = Result<Type, TypeCheckerError>;

pub struct TypeChecker<'a> {
    tokens: &'a Vec<Token>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        TypeChecker { tokens }
    }

    pub fn check(&self, prog: &Program) -> Result<(), TypeCheckerError> {
        for decl in prog.iter() {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn check_decl(&self, decl: &Declaration) -> Result<(), TypeCheckerError> {
        match decl {
            Declaration::StatementDecl(stmt) => {
                self.check_stmt(stmt)?;
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn check_stmt(&self, stmt: &Statement) -> Result<(), TypeCheckerError> {
        match stmt {
            Statement::ExpressionStmt(expr) => {
                self.check_expr(expr)?;
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn check_expr(&self, expr: &Expression) -> TypeResult {
        match expr {
            Expression::Binary(lexpr, _, rexpr) => {
                let ltype = self.check_expr(lexpr)?;
                let rtype = self.check_expr(rexpr)?;
                if ltype == rtype {
                    Ok(Type::Integer(0))
                } else {
                    Err(TypeCheckerError {
                        token: self.tokens[rtype.get_token_index()].clone(),
                        message: format!(
                            "Types {:?} and {:?} are not compatible in binary operation",
                            ltype, rtype
                        ),
                    })
                }
            }
            Expression::Integer(_, token_index) => Ok(Type::Integer(*token_index)),
            Expression::Str(_, token_index) => Ok(Type::Str(*token_index)),
            _ => unimplemented!(),
        }
    }
}
