use rustre_parser::ast::{AstNode, AstToken, ConstantNode, ExpressionNode};
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
        if let Some(constant) = node.constant_node() {
            return Self::bake_literal(constant);
        }

        if let Some(expr_id_node) = node.ident_expression_node() {
            let id_node = expr_id_node.id_node().unwrap();
            return Ok(Self::Identifier(id_node.ident().unwrap().text().into()));
        }

        if let Some(paren) = node.par_expression_node() {
            return if let Some(inner_expr) = paren.expression_node() {
                Ok(Self::Parenthesised(Box::new(Self::bake(inner_expr)?))) // TODO Error
            } else {
                Err("expected expression inside parenthesis")
            };
        }

        let node = node.syntax();
        let inner = node.children().next().expect("nothing inside"); // TODO

        use rustre_parser::lexer::Token::*;
        match inner.kind() {
            AddExpressionNode => Self::bake_binary(Self::Add, &inner),
            XorExpressionNode => Self::bake_binary(Self::Xor, &inner),
            OrExpressionNode => Self::bake_binary(Self::Or, &inner),
            AndExpressionNode => Self::bake_binary(Self::And, &inner),
            other => unimplemented!("unknown expression token {other:?}"),
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
        syntax: &SyntaxNode,
    ) -> Result<Self, &'static str> {
        let mut op1 = None;
        let mut operator = None;
        let mut op2 = None;

        for child in syntax
            .children_with_tokens()
            .filter(|c| c.kind().is_non_trivia())
            .take(3)
        {
            let expr = child.as_node().cloned().and_then(ExpressionNode::cast);

            match (expr, operator.is_none()) {
                (Some(expr), true) => assert!(op1.replace(expr).is_none()),
                (Some(expr), false) => assert!(op2.replace(expr).is_none()),
                _ => assert!(operator.replace(child).is_none()),
            }
        }

        match (op1, op2) {
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
    use rustre_parser::*;

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
