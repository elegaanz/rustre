//! Visitors and walkers for [`ast::ExpressionNode`s][ExpressionNode]

use crate::ast::*;

/// Visitor for [ExpressionNodes][crate::ast::ExpressionNode]
pub trait ExpressionVisitor<O> {
    fn visit_constant(&mut self, e: ConstantNode) -> O;
    fn visit_ident(&mut self, e: IdentExpressionNode) -> O;
    fn visit_not(&mut self, e: NotExpressionNode) -> O;
    fn visit_neg(&mut self, e: NegExpressionNode) -> O;
    fn visit_pre(&mut self, e: PreExpressionNode) -> O;
    fn visit_current(&mut self, e: CurrentExpressionNode) -> O;
    fn visit_int(&mut self, e: IntExpressionNode) -> O;
    fn visit_real(&mut self, e: RealExpressionNode) -> O;
    fn visit_when(&mut self, e: WhenExpressionNode) -> O;
    fn visit_fby(&mut self, e: FbyExpressionNode) -> O;
    fn visit_arrow(&mut self, e: ArrowExpressionNode) -> O;
    fn visit_and(&mut self, e: AndExpressionNode) -> O;
    fn visit_or(&mut self, e: OrExpressionNode) -> O;
    fn visit_xor(&mut self, e: XorExpressionNode) -> O;
    fn visit_impl(&mut self, e: ImplExpressionNode) -> O;
    fn visit_eq(&mut self, e: EqExpressionNode) -> O;
    fn visit_neq(&mut self, e: NeqExpressionNode) -> O;
    fn visit_lt(&mut self, e: LtExpressionNode) -> O;
    fn visit_lte(&mut self, e: LteExpressionNode) -> O;
    fn visit_gt(&mut self, e: GtExpressionNode) -> O;
    fn visit_gte(&mut self, e: GteExpressionNode) -> O;
    fn visit_div(&mut self, e: DivExpressionNode) -> O;
    fn visit_mod(&mut self, e: ModExpressionNode) -> O;
    fn visit_sub(&mut self, e: SubExpressionNode) -> O;
    fn visit_add(&mut self, e: AddExpressionNode) -> O;
    fn visit_mul(&mut self, e: MulExpressionNode) -> O;
    fn visit_power(&mut self, e: PowerExpressionNode) -> O;
    fn visit_if(&mut self, e: IfExpressionNode) -> O;
    fn visit_with(&mut self, e: WithExpressionNode) -> O;
    fn visit_diese(&mut self, e: DieseExpressionNode) -> O;
    fn visit_nor(&mut self, e: NorExpressionNode) -> O;
    fn visit_par(&mut self, e: ParExpressionNode) -> O;
    fn visit_call_by_pos(&mut self, e: CallByPosExpressionNode) -> O;
    fn visit_hat(&mut self, e: HatExpressionNode) -> O;
}

macro_rules! walk_rec1 {
    ($self:ident.$visit:ident($e:ident)) => {{
        let op = $e.operand();
        $self.$visit($e);
        $self.walk_expr_opt(op);
    }};
}

macro_rules! walk_rec2 {
    ($self:ident.$visit:ident($e:ident)) => {{
        let op1 = $e.left();
        let op2 = $e.right();
        $self.$visit($e);
        $self.walk_expr_opt(op1);
        $self.walk_expr_opt(op2);
    }};
}

/// Walker for [ExpressionNodes][ExpressionNode]
///
/// Walkers are specialized visitors that return nothing.
///
/// # Usage
///
/// To use this trait, implement it on a structure and call [`ExpressionWalker::walk_expr`] with an
/// expression. The expression will be fully traversed recursively and the various methods of this
/// trait will be called in preorder.
pub trait ExpressionWalker: ExpressionVisitor<()> {
    fn walk_constant(&mut self, _e: ConstantNode) {}
    fn walk_ident(&mut self, _e: IdentExpressionNode) {}
    fn walk_not(&mut self, _e: NotExpressionNode) {}
    fn walk_neg(&mut self, _e: NegExpressionNode) {}
    fn walk_pre(&mut self, _e: PreExpressionNode) {}
    fn walk_current(&mut self, _e: CurrentExpressionNode) {}
    fn walk_int(&mut self, _e: IntExpressionNode) {}
    fn walk_real(&mut self, _e: RealExpressionNode) {}
    fn walk_when(&mut self, _e: WhenExpressionNode) {}
    fn walk_fby(&mut self, _e: FbyExpressionNode) {}
    fn walk_arrow(&mut self, _e: ArrowExpressionNode) {}
    fn walk_and(&mut self, _e: AndExpressionNode) {}
    fn walk_or(&mut self, _e: OrExpressionNode) {}
    fn walk_xor(&mut self, _e: XorExpressionNode) {}
    fn walk_impl(&mut self, _e: ImplExpressionNode) {}
    fn walk_eq(&mut self, _e: EqExpressionNode) {}
    fn walk_neq(&mut self, _e: NeqExpressionNode) {}
    fn walk_lt(&mut self, _e: LtExpressionNode) {}
    fn walk_lte(&mut self, _e: LteExpressionNode) {}
    fn walk_gt(&mut self, _e: GtExpressionNode) {}
    fn walk_gte(&mut self, _e: GteExpressionNode) {}
    fn walk_div(&mut self, _e: DivExpressionNode) {}
    fn walk_mod(&mut self, _e: ModExpressionNode) {}
    fn walk_sub(&mut self, _e: SubExpressionNode) {}
    fn walk_add(&mut self, _e: AddExpressionNode) {}
    fn walk_mul(&mut self, _e: MulExpressionNode) {}
    fn walk_power(&mut self, _e: PowerExpressionNode) {}
    fn walk_if(&mut self, _e: IfExpressionNode) {}
    fn walk_with(&mut self, _e: WithExpressionNode) {}
    fn walk_diese(&mut self, _e: DieseExpressionNode) {}
    fn walk_nor(&mut self, _e: NorExpressionNode) {}
    fn walk_par(&mut self, _e: ParExpressionNode) {}
    fn walk_call_by_pos(&mut self, _e: CallByPosExpressionNode) {}
    fn walk_hat(&mut self, _e: HatExpressionNode) {}

    /// Recursively walk over an expression and its sub-expression, calling `walk_*` methods
    #[deny(unused_variables)] // We don't want to miss a recursion case
    fn walk_expr(&mut self, e: ExpressionNode) {
        match e {
            ExpressionNode::ConstantNode(e) => {
                self.walk_constant(e);
            }
            ExpressionNode::IdentExpressionNode(e) => {
                self.walk_ident(e);
            }
            ExpressionNode::NotExpressionNode(e) => walk_rec1!(self.walk_not(e)),
            ExpressionNode::NegExpressionNode(e) => walk_rec1!(self.walk_neg(e)),
            ExpressionNode::PreExpressionNode(e) => walk_rec1!(self.walk_pre(e)),
            ExpressionNode::CurrentExpressionNode(e) => walk_rec1!(self.walk_current(e)),
            ExpressionNode::IntExpressionNode(e) => walk_rec1!(self.walk_int(e)),
            ExpressionNode::RealExpressionNode(e) => walk_rec1!(self.walk_real(e)),
            ExpressionNode::WhenExpressionNode(e) => {
                self.walk_when(e);
            }
            ExpressionNode::FbyExpressionNode(e) => walk_rec2!(self.walk_fby(e)),
            ExpressionNode::ArrowExpressionNode(e) => walk_rec2!(self.walk_arrow(e)),
            ExpressionNode::AndExpressionNode(e) => walk_rec2!(self.walk_and(e)),
            ExpressionNode::OrExpressionNode(e) => walk_rec2!(self.walk_or(e)),
            ExpressionNode::XorExpressionNode(e) => walk_rec2!(self.walk_xor(e)),
            ExpressionNode::ImplExpressionNode(e) => walk_rec2!(self.walk_impl(e)),
            ExpressionNode::EqExpressionNode(e) => walk_rec2!(self.walk_eq(e)),
            ExpressionNode::NeqExpressionNode(e) => walk_rec2!(self.walk_neq(e)),
            ExpressionNode::LtExpressionNode(e) => walk_rec2!(self.walk_lt(e)),
            ExpressionNode::LteExpressionNode(e) => walk_rec2!(self.walk_lte(e)),
            ExpressionNode::GtExpressionNode(e) => walk_rec2!(self.walk_gt(e)),
            ExpressionNode::GteExpressionNode(e) => walk_rec2!(self.walk_gte(e)),
            ExpressionNode::DivExpressionNode(e) => walk_rec2!(self.walk_div(e)),
            ExpressionNode::ModExpressionNode(e) => walk_rec2!(self.walk_mod(e)),
            ExpressionNode::SubExpressionNode(e) => walk_rec2!(self.walk_sub(e)),
            ExpressionNode::AddExpressionNode(e) => walk_rec2!(self.walk_add(e)),
            ExpressionNode::MulExpressionNode(e) => walk_rec2!(self.walk_mul(e)),
            ExpressionNode::PowerExpressionNode(e) => walk_rec2!(self.walk_power(e)),
            ExpressionNode::IfExpressionNode(e) => {
                let op1 = e.cond();
                let op2 = e.if_body();
                let op3 = e.else_body();
                self.walk_if(e);
                self.walk_expr_opt(op1);
                self.walk_expr_opt(op2);
                self.walk_expr_opt(op3);
            }
            ExpressionNode::WithExpressionNode(e) => {
                let op1 = e.cond();
                let op2 = e.with_body();
                let op3 = e.else_body();
                self.walk_with(e);
                self.walk_expr_opt(op1);
                self.walk_expr_opt(op2);
                self.walk_expr_opt(op3);
            }
            ExpressionNode::DieseExpressionNode(e) => {
                let list = e.list();
                self.walk_diese(e);
                for e in list.iter().flat_map(|el| el.all_expression_node()) {
                    self.walk_expr(e);
                }
            }
            ExpressionNode::NorExpressionNode(e) => {
                let list = e.list();
                self.walk_nor(e);
                for e in list.iter().flat_map(|el| el.all_expression_node()) {
                    self.walk_expr(e);
                }
            }
            ExpressionNode::ParExpressionNode(e) => {
                let expr = e.expression_node();
                self.walk_par(e);
                self.walk_expr_opt(expr);
            }
            ExpressionNode::CallByPosExpressionNode(e) => {
                let args = e.args();
                self.walk_call_by_pos(e);
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            ExpressionNode::HatExpressionNode(e) => walk_rec2!(self.walk_hat(e)),
        }
    }

    /// Like [walk_expr][Self::walk_expr], but takes an option
    fn walk_expr_opt(&mut self, e: Option<ExpressionNode>) {
        if let Some(e) = e {
            self.walk_expr(e);
        }
    }
}

/// A walker is implicitly a specialized visitor
impl<T: ExpressionWalker> ExpressionVisitor<()> for T {
    fn visit_constant(&mut self, e: ConstantNode) {
        self.walk_constant(e);
    }

    fn visit_ident(&mut self, e: IdentExpressionNode) {
        self.walk_ident(e);
    }

    fn visit_not(&mut self, e: NotExpressionNode) {
        self.walk_not(e);
    }

    fn visit_neg(&mut self, e: NegExpressionNode) {
        self.walk_neg(e);
    }

    fn visit_pre(&mut self, e: PreExpressionNode) {
        self.walk_pre(e);
    }

    fn visit_current(&mut self, e: CurrentExpressionNode) {
        self.walk_current(e);
    }

    fn visit_int(&mut self, e: IntExpressionNode) {
        self.walk_int(e);
    }

    fn visit_real(&mut self, e: RealExpressionNode) {
        self.walk_real(e);
    }

    fn visit_when(&mut self, e: WhenExpressionNode) {
        self.walk_when(e);
    }

    fn visit_fby(&mut self, e: FbyExpressionNode) {
        self.walk_fby(e);
    }

    fn visit_arrow(&mut self, e: ArrowExpressionNode) {
        self.walk_arrow(e);
    }

    fn visit_and(&mut self, e: AndExpressionNode) {
        self.walk_and(e);
    }

    fn visit_or(&mut self, e: OrExpressionNode) {
        self.walk_or(e);
    }

    fn visit_xor(&mut self, e: XorExpressionNode) {
        self.walk_xor(e);
    }

    fn visit_impl(&mut self, e: ImplExpressionNode) {
        self.walk_impl(e);
    }

    fn visit_eq(&mut self, e: EqExpressionNode) {
        self.walk_eq(e);
    }

    fn visit_neq(&mut self, e: NeqExpressionNode) {
        self.walk_neq(e);
    }

    fn visit_lt(&mut self, e: LtExpressionNode) {
        self.walk_lt(e);
    }

    fn visit_lte(&mut self, e: LteExpressionNode) {
        self.walk_lte(e);
    }

    fn visit_gt(&mut self, e: GtExpressionNode) {
        self.walk_gt(e);
    }

    fn visit_gte(&mut self, e: GteExpressionNode) {
        self.walk_gte(e);
    }

    fn visit_div(&mut self, e: DivExpressionNode) {
        self.walk_div(e);
    }

    fn visit_mod(&mut self, e: ModExpressionNode) {
        self.walk_mod(e);
    }

    fn visit_sub(&mut self, e: SubExpressionNode) {
        self.walk_sub(e);
    }

    fn visit_add(&mut self, e: AddExpressionNode) {
        self.walk_add(e);
    }

    fn visit_mul(&mut self, e: MulExpressionNode) {
        self.walk_mul(e);
    }

    fn visit_power(&mut self, e: PowerExpressionNode) {
        self.walk_power(e);
    }

    fn visit_if(&mut self, e: IfExpressionNode) {
        self.walk_if(e);
    }

    fn visit_with(&mut self, e: WithExpressionNode) {
        self.walk_with(e);
    }

    fn visit_diese(&mut self, e: DieseExpressionNode) {
        self.walk_diese(e);
    }

    fn visit_nor(&mut self, e: NorExpressionNode) {
        self.walk_nor(e);
    }

    fn visit_par(&mut self, e: ParExpressionNode) {
        self.walk_par(e);
    }

    fn visit_call_by_pos(&mut self, e: CallByPosExpressionNode) {
        self.walk_call_by_pos(e);
    }

    fn visit_hat(&mut self, e: HatExpressionNode) {
        self.walk_hat(e);
    }
}
