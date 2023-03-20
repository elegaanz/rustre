use rustre_parser::ast::{AstToken, BinaryExpression, ConstantNode, ExpressionNode};
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
    // TODO add remaining expressions
    // TODO add SyntaxNode (or any way to access the original syntax for diagnostics)
}

impl BakedExpression {
    pub fn bake(node: ExpressionNode) -> Result<Self, &'static str> {
        match node {
            ExpressionNode::ConstantNode(constant) => Self::bake_literal(constant),
            ExpressionNode::IdentExpressionNode(expr_id_node) => {
                let id_node = expr_id_node.id_node().unwrap();
                Ok(Self::Identifier(id_node.ident().unwrap().text().into()))
            }
            ExpressionNode::ParExpressionNode(paren) => {
                if let Some(inner_expr) = paren.expression_node() {
                    Ok(Self::Parenthesised(Box::new(Self::bake(inner_expr)?))) // TODO Error
                } else {
                    Err("expected expression inside parenthesis")
                }
            }
            ExpressionNode::AndExpressionNode(node) => Self::bake_binary(Self::And, &node),
            ExpressionNode::OrExpressionNode(node) => Self::bake_binary(Self::Or, &node),
            ExpressionNode::XorExpressionNode(node) => Self::bake_binary(Self::Xor, &node),
            ExpressionNode::AddExpressionNode(node) => Self::bake_binary(Self::Add, &node),
            _ => unimplemented!("unknown expression token {node:?}"),
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

        ExpressionNode::cast(node.children().next().unwrap()).unwrap()
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
