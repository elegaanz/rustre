use crate::expression::BakedExpression;
use bimap::BiMap;
use petgraph::prelude::*;
use rustre_parser::ast::{
    AstNode, AstToken, EqualsEquationNode, Ident, NodeNode, NodeProfileNode, TypeNode,
};
use rustre_parser::SyntaxNode;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

pub type NodeIndex = petgraph::graph::NodeIndex<u32>;

// FIXME this error type is bad
#[derive(Debug)]
pub enum Error {
    Unsupported(&'static str, SyntaxNode),
    DuplicateVar(Ident),
    Untyped(Vec<Ident>),

    // TODO specialize
    ExpressionBaking(&'static str),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Error,

    Input,
    LiteralInt(i32),
    LiteralReal(f32),
    LiteralBool(bool),
    TupleElement(u32),

    Pre,

    Add,
    Sub,

    Xor,
    Or,
    And,
    Not,
    Neg,
    Current,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    Impl,
    Div,
    Mod,
    Mul,
    Power,
    Fby,
    When,
    Arrow,
    Int,
    Real,
    If,
    With,
    Diese,
    Nor
}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::LiteralInt(v) => v.hash(state),
            Self::LiteralReal(v) => v.to_ne_bytes().hash(state),
            Self::LiteralBool(v) => v.hash(state),
            Self::TupleElement(v) => v.hash(state),
            _ => (),
        }
    }
}

#[derive(Clone)]
pub struct NodeGraph {
    /// Direct graph of expressions, connected by an operand index
    pub graph: DiGraph<Expression, u8, u32>,

    /// Mapping of variable/parameter names to graph node ID
    pub bindings: BiMap<String, NodeIndex>,
}

// Probably very bad but it should be ok for the moment
impl Hash for NodeGraph {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for node in self.graph.raw_nodes() {
            node.weight.hash(state);
        }

        for edge in self
            .graph
            .edge_indices()
            .zip(self.graph.edge_weights())
            .enumerate()
        {
            edge.hash(state);
            self.graph.edge_endpoints(edge.1 .0).hash(state);
        }

        for binding in &self.bindings {
            binding.hash(state);
        }
    }
}

impl Display for NodeGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut graph_mapped = self.graph.map(
            |ni, n| match n {
                Expression::Input => {
                    Cow::Borrowed(self.bindings.get_by_right(&ni).expect("binding not found"))
                }
                other => Cow::Owned(format!("{other:?}")),
            },
            |_, o| o.to_string(),
        );

        for (binding, index) in &self.bindings {
            let node = graph_mapped.add_node(Cow::Owned(format!("≔{binding}")));
            graph_mapped.add_edge(node, *index, "".to_string());
        }

        let dot = petgraph::dot::Dot::new(&graph_mapped);
        Display::fmt(&dot, f)
    }
}

#[derive(Default)]
pub struct NodeGraphBuilder {
    pub errors: Vec<Error>,

    /// Local variables, including parameters and return variables
    local_vars: HashSet<String>,
}

impl NodeGraphBuilder {
    fn declare_local_var(&mut self, ident: &Ident) {
        if !self.local_vars.insert(ident.text().to_owned()) {
            self.errors.push(Error::DuplicateVar(ident.clone()));
        }
    }

    fn try_resolve_params(&mut self, syntax: NodeProfileNode) -> HashMap<Ident, TypeNode> {
        let Some(syntax_params) = syntax.params() else {
            return Default::default();
        };

        let mut params = HashMap::new();

        for syntax_param in syntax_params.all_var_decl_node() {
            if let Some(syntax_clock) = syntax_param.clock_expression_node() {
                self.errors.push(Error::Unsupported(
                    "clock expression",
                    syntax_clock.syntax().clone(),
                ));
            }

            for syntax_typed_ids in syntax_param.all_typed_ids_node() {
                if let Some(typ) = syntax_typed_ids.type_node() {
                    for ident in syntax_typed_ids.all_ident() {
                        self.declare_local_var(&ident);
                        params.insert(ident, typ.clone());
                    }
                } else {
                    self.errors
                        .push(Error::Untyped(syntax_typed_ids.all_ident().collect()));
                }
            }
        }

        params
    }

    fn add_baked_expression(
        &mut self,
        graph: &mut NodeGraph,
        baked_expr: &BakedExpression,
    ) -> NodeIndex {
        let mut add_node = |expr: Expression, operands: &[&BakedExpression]| {
            let node = graph.graph.add_node(expr);
            for (idx, operand) in operands.iter().enumerate() {
                let sub_expr = self.add_baked_expression(graph, operand);
                graph.graph.add_edge(node, sub_expr, idx as u8);
            }
            node
        };

        match baked_expr {
            BakedExpression::Error(_) => graph.graph.add_node(Expression::Error),
            BakedExpression::LiteralInt(v) => graph.graph.add_node(Expression::LiteralInt(*v)),
            BakedExpression::LiteralReal(v) => graph.graph.add_node(Expression::LiteralReal(*v)),
            BakedExpression::LiteralBool(v) => graph.graph.add_node(Expression::LiteralBool(*v)),
            BakedExpression::Identifier(v) => *graph.bindings.get_by_left(v).expect("can't find"), // TODO
            BakedExpression::Parenthesised(a) => self.add_baked_expression(graph, a),
            BakedExpression::Pre(a) => add_node(Expression::Pre, &[a]),
            BakedExpression::Add(a, b) => add_node(Expression::Add, &[a, b]),
            BakedExpression::Xor(a, b) => add_node(Expression::Xor, &[a, b]),
            BakedExpression::Or(a, b) => add_node(Expression::Or, &[a, b]),
            BakedExpression::And(a, b) => add_node(Expression::Add, &[a, b]),
            BakedExpression::Not(v) => add_node(Expression::Not, &[v]),
            BakedExpression::Neg(v) => add_node(Expression::Neg, &[v]),
            BakedExpression::Current(v) => add_node(Expression::Current, &[v]),
            BakedExpression::Lte(a, b) => add_node(Expression::Lte, &[a, b]),
            BakedExpression::Lt(a, b) => add_node(Expression::Lt, &[a, b]),
            BakedExpression::Gte(a, b) => add_node(Expression::Gte, &[a, b]),
            BakedExpression::Gt(a, b) => add_node(Expression::Gt, &[a, b]),
            BakedExpression::Eq(a, b) => add_node(Expression::Eq, &[a, b]),
            BakedExpression::Neq(a, b) => add_node(Expression::Neq, &[a, b]),
            BakedExpression::Mul(a, b) => add_node(Expression::Mul, &[a, b]),
            BakedExpression::Div(a, b) => add_node(Expression::Div, &[a, b]),
            BakedExpression::Mod(a, b) => add_node(Expression::Mod, &[a, b]),
            BakedExpression::Sub(a, b) => add_node(Expression::Sub, &[a, b]),
            BakedExpression::Impl(a, b) => add_node(Expression::Impl, &[a, b]),
            BakedExpression::Power(a, b) => add_node(Expression::Power, &[a, b]),
            BakedExpression::Fby(a, b) => add_node(Expression::Fby, &[a, b]),
            BakedExpression::When(a, b) => add_node(Expression::When, &[a, b]),
            BakedExpression::Arrow(a, b) => add_node(Expression::Arrow, &[a, b]),
            BakedExpression::Int(a) => add_node(Expression::Int, &[a]),
            BakedExpression::Real(b) => add_node(Expression::Real, &[b]),
            BakedExpression::If(cond, if_body, else_body) => add_node(Expression::If, &[cond, if_body, else_body]),
            BakedExpression::With(cond, if_body, else_body) => add_node(Expression::With, &[cond, if_body, else_body]),
            BakedExpression::Diese(v) => add_node(Expression::Diese, &v.iter().collect::<Vec<_>>()),
            BakedExpression::Nor(v) => add_node(Expression::Nor, &v.iter().collect::<Vec<_>>()),
        }
    }

    fn try_add_equals_equation(&mut self, graph: &mut NodeGraph, equals_node: EqualsEquationNode) {
        // We silently return here, the parser should have already complained about missing syntax
        let Some(left) = equals_node.left_node() else { return };
        let Some(right) = equals_node.expression_node() else { return };

        let right = match BakedExpression::bake(right) {
            Ok(right) => right,
            Err(err) => {
                self.errors.push(Error::ExpressionBaking(err));
                return;
            }
        };

        let expr = self.add_baked_expression(graph, &right);

        let NodeGraph { graph, bindings } = graph;

        let left_list = left.all_left_item_node().collect::<Vec<_>>();
        match left_list.as_slice() {
            [] => (),
            [one_left] => {
                if let Some(id_node) = one_left.id_node() {
                    let res =
                        bindings.insert_no_overwrite(id_node.ident().unwrap().text().into(), expr);

                    if res.is_err() {
                        todo!("already set");
                    }
                }
            }
            left_list => {
                for (idx, one_left) in left_list.into_iter().enumerate() {
                    let tuple_el = Expression::TupleElement(idx as u32);
                    let tuple_el_node = graph.add_node(tuple_el);
                    graph.add_edge(tuple_el_node, expr, 0);

                    if let Some(id_node) = one_left.id_node() {
                        let res = bindings
                            .insert_no_overwrite(id_node.ident().unwrap().text().into(), expr);

                        if res.is_err() {
                            todo!("already set");
                        }
                    }
                }
            }
        }
    }

    pub fn try_parse_node_graph(&mut self, syntax: &NodeNode) -> NodeGraph {
        let profile = syntax.node_profile_node();
        let params = profile
            .map(|profile| self.try_resolve_params(profile))
            .unwrap_or_default();

        let mut node_graph = NodeGraph {
            graph: DiGraph::new(),
            bindings: BiMap::new(),
        };

        for (param, _) in params {
            let node = node_graph.graph.add_node(Expression::Input);
            node_graph.bindings.insert(param.text().to_owned(), node);
        }

        let expressions = syntax
            .body_node()
            .map(|b| b.all_equals_equation_node().collect::<Vec<_>>())
            .unwrap_or_default();

        for equals in expressions {
            self.try_add_equals_equation(&mut node_graph, equals);
        }

        node_graph
    }
}
