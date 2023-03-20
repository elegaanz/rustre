use rustre_parser::ast::{AstToken, BinaryExpression, UnaryExpression, ConstantNode, ExpressionNode, IfExpressionNode, WithExpressionNode, VariadicExpr};
use rustre_parser::SyntaxNode;

/// A non-type-checked expression tree
///
/// May be stored in an arena eventually for better performance
#[derive(Debug, Clone, PartialEq)]
pub enum BakedExpression {
    Error(SyntaxNode),

    LiteralInt(i32),
    LiteralReal(f32),
    LiteralBool(bool),
    Identifier(String),

    Parenthesised(Box<BakedExpression>),

    Pre(Box<BakedExpression>),
    Add(Box<BakedExpression>, Box<BakedExpression>),

    Xor(Box<BakedExpression>, Box<BakedExpression>),
    Or(Box<BakedExpression>, Box<BakedExpression>),
    And(Box<BakedExpression>, Box<BakedExpression>),

    Not(Box<BakedExpression>),
    Neg(Box<BakedExpression>),
    Current(Box<BakedExpression>),
    Lt(Box<BakedExpression>, Box<BakedExpression>),
    Lte(Box<BakedExpression>, Box<BakedExpression>),
    Gt(Box<BakedExpression>, Box<BakedExpression>),
    Gte(Box<BakedExpression>, Box<BakedExpression>),
    Eq(Box<BakedExpression>, Box<BakedExpression>),
    Neq(Box<BakedExpression>, Box<BakedExpression>),
    Impl(Box<BakedExpression>, Box<BakedExpression>),
    Div(Box<BakedExpression>, Box<BakedExpression>),
    Mod(Box<BakedExpression>, Box<BakedExpression>),
    Sub(Box<BakedExpression>, Box<BakedExpression>),
    Mul(Box<BakedExpression>, Box<BakedExpression>),
    Power(Box<BakedExpression>, Box<BakedExpression>),
    Fby(Box<BakedExpression>, Box<BakedExpression>),
    When(Box<BakedExpression>, Box<BakedExpression>),
    Arrow(Box<BakedExpression>, Box<BakedExpression>),
    Int(Box<BakedExpression>),
    Real(Box<BakedExpression>),
    If(Box<BakedExpression>, Box<BakedExpression>, Box<BakedExpression>),
    With(Box<BakedExpression>, Box<BakedExpression>, Box<BakedExpression>),
    Diese(Vec<BakedExpression>),
    Nor(Vec<BakedExpression>),
    // TODO add remaining expressions
    // TODO add SyntaxNode (or any way to access the original syntax for diagnostics)
}

impl BakedExpression {
    pub fn bake(node: ExpressionNode) -> Result<Self, &'static str> {
        match node {
            ExpressionNode::ConstantNode(node) => {
                Self::bake_literal(node)
            },
            ExpressionNode::NotExpressionNode(node) => {
                Self::bake_unary(Self::Not, &node)
            },
            ExpressionNode::NegExpressionNode(node) => {
                Self::bake_unary(Self::Neg, &node)
            },
            ExpressionNode::PreExpressionNode(node) => {
                Self::bake_unary(Self::Pre, &node)
            },
            ExpressionNode::CurrentExpressionNode(node) => {
                Self::bake_unary(Self::Current, &node)
            },
            ExpressionNode::IntExpressionNode(node) => {
                Self::bake_unary(Self::Int, &node)
            },
            ExpressionNode::RealExpressionNode(node) => {
                Self::bake_unary(Self::Real, &node)
            },
            ExpressionNode::WhenExpressionNode(node) => {
                Self::bake_binary(Self::When, &node)
            },
            ExpressionNode::FbyExpressionNode(node) => {
                Self::bake_binary(Self::Fby, &node)
            },
            ExpressionNode::ArrowExpressionNode(node) => {
                Self::bake_binary(Self::Arrow, &node)
            },
            ExpressionNode::AndExpressionNode(node) => {
                Self::bake_binary(Self::And, &node)
            },
            ExpressionNode::OrExpressionNode(node) => {
                Self::bake_binary(Self::Or, &node)
            },
            ExpressionNode::XorExpressionNode(node) => {
                Self::bake_binary(Self::Xor, &node)
            },
            ExpressionNode::ImplExpressionNode(node) => {
                Self::bake_binary(Self::Impl, &node)
            },
            ExpressionNode::EqExpressionNode(node) => {
                Self::bake_binary(Self::Eq, &node)
            },
            ExpressionNode::NeqExpressionNode(node) => {
                Self::bake_binary(Self::Neq, &node)
            },
            ExpressionNode::LtExpressionNode(node) => {
                Self::bake_binary(Self::Lt, &node)
            },
            ExpressionNode::LteExpressionNode(node) => {
                Self::bake_binary(Self::Lte, &node)
            },
            ExpressionNode::GtExpressionNode(node) => {
                Self::bake_binary(Self::Gt, &node)
            },
            ExpressionNode::GteExpressionNode(node) => {
                Self::bake_binary(Self::Gte, &node)
            },
            ExpressionNode::AddExpressionNode(node) => {
                Self::bake_binary(Self::Add, &node)
            },
            ExpressionNode::SubExpressionNode(node) => {
                Self::bake_binary(Self::Sub, &node)
            },
            ExpressionNode::MulExpressionNode(node) => {
                Self::bake_binary(Self::Mul, &node)
            },
            ExpressionNode::DivExpressionNode(node) => {
                Self::bake_binary(Self::Div, &node)
            },
            ExpressionNode::ModExpressionNode(node) => {
                Self::bake_binary(Self::Mod, &node)
            },
            ExpressionNode::PowerExpressionNode(node) => {
                Self::bake_binary(Self::Power, &node)
            },
            ExpressionNode::IfExpressionNode(node) => {
                Self::bake_if(&node)
            },
            ExpressionNode::WithExpressionNode(node) => {
                Self::bake_with(&node)
            },
            ExpressionNode::DieseExpressionNode(node) => {
                Self::bake_variadic(Self::Diese, &node)
            }
            ExpressionNode::NorExpressionNode(node) => {
                Self::bake_variadic(Self::Nor, &node)
            }
            ExpressionNode::IdentExpressionNode(node) => {
                let id_node = node.id_node().unwrap();
                Ok(Self::Identifier(id_node.ident().unwrap().text().into()))
            }
            ExpressionNode::ParExpressionNode(paren) => {
                if let Some(inner_expr) = paren.expression_node() {
                    Ok(Self::Parenthesised(Box::new(Self::bake(inner_expr)?))) // TODO Error
                } else {
                    Err("expected expression inside parenthesis")
                }
            }
        }
    }

    fn bake_literal(node: ConstantNode) -> Result<Self, &'static str> {
        if node.is_false() {
            Ok(Self::LiteralBool(false))
        } else if node.is_true() {
            Ok(Self::LiteralBool(true))
        } else if let Some(int) = node.i_const() {
            Ok(Self::LiteralInt(int.text().parse().unwrap()))
        } else if let Some(real) = node.r_const() {
            Ok(Self::LiteralReal(real.text().parse().unwrap()))
        } else {
            Err("unparseable constant expression")
        }
    }

    fn bake_binary(
        factory: fn(Box<Self>, Box<Self>) -> Self,
        expr: &impl BinaryExpression,
    ) -> Result<Self, &'static str> {
        match (expr.left(), expr.right()) {
            (Some(op1), Some(op2)) => Ok(factory(
                // FIXME maybe insert Error instead of failing
                Box::new(Self::bake(op1)?),
                Box::new(Self::bake(op2)?),
            )),
            (Some(_), None) => Err("missing second operand"),
            (None, Some(_)) => Err("missing first operands"),
            (None, None) => Err("missing both operands"),
        }
    }

    fn bake_unary(
        factory: fn(Box<Self>) -> Self,
        expr: &impl UnaryExpression,
    ) -> Result<Self, &'static str> {
        match expr.operand() {
            Some(op) => Ok(factory(Box::new(Self::bake(op)?))),
            None => Err("missing operand"),
        }
    }

    fn bake_if(
        expr: &IfExpressionNode,
    ) -> Result<Self, &'static str> {
        let condition = expr.cond().unwrap();
        let then = expr.if_body().unwrap();
        let otherwise = expr.else_body().unwrap();

        Ok(Self::If(
            Box::new(Self::bake(condition)?),
            Box::new(Self::bake(then)?),
            Box::new(Self::bake(otherwise)?),
        ))
    }

    fn bake_with(
        expr: &WithExpressionNode,
    ) -> Result<Self, &'static str> {
        let condition = expr.cond().unwrap();
        let then = expr.with_body().unwrap();
        let otherwise = expr.else_body().unwrap();

        Ok(Self::With(
            Box::new(Self::bake(condition)?),
            Box::new(Self::bake(then)?),
            Box::new(Self::bake(otherwise)?),
        ))
    }

    fn bake_variadic(
        factory: fn(Vec<Self>) -> Self,
        expr: &impl VariadicExpr,
    ) -> Result<Self, &'static str> {
        let mut res = Vec::new();
        for sub_expr in expr.list().unwrap().all_expression_node() {
            res.push(Self::bake(sub_expr)?);
        }
        Ok(factory(res))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rustre_parser::{*, ast::AstNode};

    fn parse(source: &str) -> ExpressionNode {
        let tokens = lex(source)
            .map(|(tok, span)| (tok, &source[span]))
            .collect::<Vec<_>>();

        let (_, children) = parser::expression::parse_expression(tokens.as_slice().into())
            .unwrap_or_else(|_| panic!());

        let (node, errors) = children.into_root_node(lexer::Token::Root);
        assert_eq!(errors.len(), 0, "parse errors");

        ExpressionNode::cast(dbg!(node.children().next().unwrap())).unwrap()
    }

    #[test]
    fn parse_1_plus_1() {
        let expression = parse("1 + 1");
        let expression = BakedExpression::bake(expression).unwrap();

        assert_eq!(
            expression,
            BakedExpression::Add(
                BakedExpression::LiteralInt(1).into(),
                BakedExpression::LiteralInt(1).into()
            )
        );
    }
}
