use rustre_parser::ast::*;
use yeter::Database;

/// **Query** Resolves a type declaration by name
#[yeter::query]
pub fn resolve_type_decl(db: &Database, name: IdNode) -> Option<OneTypeDeclNode> {
    let name = name.ident()?;
    let name = name.text();

    let files = super::parsed_files(db);

    files
        .iter()
        .flat_map(|root| root.all_type_decl_node())
        .flat_map(|decl| decl.all_one_type_decl_node())
        .find(|decl| matches!(decl.ident(), Some(n) if n.text() == name))
}

#[yeter::query]
pub fn find_node(db: &Database, node_name: String) -> Option<NodeNode> {
    for file in super::parsed_files(db).as_slice() {
        for node in file.all_node_node() {
            if node.id_node()?.ident()?.text() == node_name {
                return Some(node.clone());
            }
        }
    }

    None
}

// TODO handle packages correctly
#[derive(Clone, Debug, Hash)]
pub struct NameResolveQuery {
    ident: IdNode,
    in_node: Option<NodeNode>,
}

pub enum ResolvedRuntimeNode<Var, Input = Var> {
    Const(OneConstantDeclNode),
    Param(Input),
    ReturnParam(Var),
    Var(Var),
}

/// **Query**
#[yeter::query]
pub fn resolve_const_node(db: &Database, query: NameResolveQuery) -> Option<OneConstantDeclNode> {
    let local_scope = query
        .in_node
        .iter()
        .flat_map(|in_node| in_node.all_one_constant_decl_node());

    // TODO statics

    let files = super::parsed_files(db);
    let global_scope = files
        .iter()
        .flat_map(|root| root.all_constant_decl_node())
        .flat_map(|const_decl| const_decl.all_one_constant_decl_node());

    local_scope
        .chain(global_scope)
        .find(|one_const| one_const.all_id_node().any(|i| i == query.ident))
}

/// **Query**
#[yeter::query]
pub fn resolve_const_expr_node(db: &Database, query: NameResolveQuery) -> Option<ExpressionNode> {
    resolve_const_node(db, query)
        .as_ref()
        .as_ref()
        .and_then(OneConstantDeclNode::expression_node)
}

/// **Query**
#[yeter::query]
pub fn resolve_runtime_node(
    db: &Database,
    query: NameResolveQuery,
) -> Option<ResolvedRuntimeNode<TypedIdsNode>> {
    if let (Some(ident), Some(in_node)) = (query.ident.ident(), &query.in_node) {
        let sig = super::get_signature(db, in_node.clone());

        let params_c = std::iter::repeat(ResolvedRuntimeNode::Param as fn(_) -> _);
        let params = sig.params.iter().cloned().zip(params_c);

        let return_params_c = std::iter::repeat(ResolvedRuntimeNode::ReturnParam as fn(_) -> _);
        let return_params = sig.return_params.iter().cloned().zip(return_params_c);

        let local_vars_c = std::iter::repeat(ResolvedRuntimeNode::Var as fn(_) -> _);
        let local_vars = query
            .in_node
            .iter()
            .flat_map(|in_node| in_node.all_var_decl_node().next())
            .flat_map(|var| var.all_typed_ids_node())
            .zip(local_vars_c);

        let local = params
            .chain(return_params)
            .chain(local_vars)
            .find(|(ids, _)| ids.all_ident().any(|i| i == ident));

        if let Some((local, constructor)) = local {
            return Some(constructor(local));
        }
    }

    // Fallback to constants
    resolve_const_node(db, query)
        .as_ref()
        .clone()
        .map(ResolvedRuntimeNode::Const)
}
