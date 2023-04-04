use std::rc::Rc;

use inkwell::values::BasicValue;
use kaede_ast::expr::{ExprKind, Ident};
use kaede_ast::stmt::{
    Assign, AssignKind, Block, Let, LetKind, NormalLet, Stmt, StmtKind, TupleUnpack,
};
use kaede_span::Span;
use kaede_type::{is_same_type, Mutability, RefrenceType, Ty, TyKind};

use crate::expr::build_tuple_indexing;
use crate::value::Value;
use crate::{
    error::{CodegenError, CodegenResult},
    expr::build_expression,
    get_loaded_pointer,
    tcx::SymbolTable,
    CompileUnitContext,
};

pub fn build_block(cucx: &mut CompileUnitContext, block: &Block) -> CodegenResult<()> {
    cucx.tcx.push_symbol_table(SymbolTable::new());

    for stmt in &block.body {
        build_statement(cucx, stmt)?;
    }

    cucx.tcx.pop_symbol_table();

    Ok(())
}

pub fn build_statement(cucx: &mut CompileUnitContext, node: &Stmt) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(cucx);

    builder.build(node)?;

    Ok(())
}

/// Duplicate the type, change the mutability, and return the duplicated type
pub fn change_mutability_dup(ty: Rc<Ty>, mutability: Mutability) -> Rc<Ty> {
    let mut var_ty = (*ty).clone();

    var_ty.mutability = mutability;

    if let TyKind::Reference(rty) = var_ty.kind.as_ref() {
        let new_refee_ty = Ty {
            kind: rty.refee_ty.kind.clone(),
            mutability,
        };

        var_ty.kind = TyKind::Reference(RefrenceType {
            refee_ty: new_refee_ty.into(),
        })
        .into();
    };

    var_ty.into()
}

struct StmtBuilder<'a, 'ctx, 'm, 'c> {
    cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>,
}

impl<'a, 'ctx, 'm, 'c> StmtBuilder<'a, 'ctx, 'm, 'c> {
    fn new(cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>) -> Self {
        Self { cucx }
    }

    /// Generate statement code
    fn build(&mut self, stmt: &Stmt) -> CodegenResult<()> {
        match &stmt.kind {
            StmtKind::Expr(e) => {
                if matches!(e.kind, ExprKind::If(_)) {
                    self.cucx.is_if_statement = true;
                    build_expression(self.cucx, e)?;
                    self.cucx.is_if_statement = false;
                } else {
                    build_expression(self.cucx, e)?;
                }
            }

            StmtKind::Let(node) => self.let_(node)?,

            StmtKind::Assign(node) => self.assign(node)?,
        }

        Ok(())
    }

    fn assign(&mut self, node: &Assign) -> CodegenResult<()> {
        let left = build_expression(self.cucx, &node.lhs)?;

        if left.get_type().mutability.is_not() {
            return Err(CodegenError::CannotAssignTwiceToImutable { span: node.span });
        }

        let ptr_to_left =
            match get_loaded_pointer(&left.get_value().as_instruction_value().unwrap()) {
                Some(p) => p,
                None => return Err(CodegenError::InvalidLeftOfAssignment { span: node.span }),
            };

        let right = build_expression(self.cucx, &node.rhs)?;

        if !is_same_type(&left.get_type(), &right.get_type()) {
            return Err(CodegenError::MismatchedTypes {
                types: (
                    left.get_type().kind.to_string(),
                    right.get_type().kind.to_string(),
                ),
                span: node.span,
            });
        }

        match node.kind {
            AssignKind::Simple => self
                .cucx
                .builder
                .build_store(ptr_to_left, right.get_value()),
        };

        Ok(())
    }

    fn let_(&mut self, node: &Let) -> CodegenResult<()> {
        match &node.kind {
            LetKind::NormalLet(node) => self.normal_let(node),
            LetKind::TupleUnpack(node) => self.tuple_unpacking(node),
        }
    }

    fn normal_let_internal(
        &mut self,
        name: &Ident,
        mutability: Mutability,
        init: Option<Value<'ctx>>,
        specified_ty: Rc<Ty>,
        span: Span,
    ) -> CodegenResult<()> {
        if let Some(init) = init {
            if matches!(init.get_type().kind.as_ref(), TyKind::Reference(_))
                && mutability.is_mut()
                && init.get_type().mutability.is_not()
            {
                return Err(CodegenError::CannotAssignImmutableToMutable { span });
            }

            let var_ty = change_mutability_dup(init.get_type(), mutability);

            let alloca = if specified_ty.kind.is_inferred() {
                // No type information was available, so infer from an initializer
                let alloca = self.cucx.create_entry_block_alloca(name.as_str(), &var_ty);

                self.cucx
                    .tcx
                    .add_symbol(name.name.clone(), (alloca, var_ty));

                alloca
            } else {
                // Type information is available

                // Check if an initializer type and type match
                if !is_same_type(&specified_ty, &init.get_type()) {
                    return Err(CodegenError::MismatchedTypes {
                        types: (
                            specified_ty.kind.to_string(),
                            init.get_type().kind.to_string(),
                        ),
                        span,
                    });
                }

                let alloca = self.cucx.create_entry_block_alloca(name.as_str(), &var_ty);

                self.cucx
                    .tcx
                    .add_symbol(name.name.clone(), (alloca, var_ty));

                alloca
            };

            // Initialization
            self.cucx.builder.build_store(alloca, init.get_value());

            return Ok(());
        }

        todo!()
    }

    fn normal_let(&mut self, node: &NormalLet) -> CodegenResult<()> {
        let value = match &node.init {
            Some(e) => Some(build_expression(self.cucx, e)?),
            None => None,
        };

        self.normal_let_internal(
            &node.name,
            node.mutability,
            value,
            node.ty.clone(),
            node.span,
        )
    }

    fn tuple_unpacking(&mut self, node: &TupleUnpack) -> CodegenResult<()> {
        let tuple = build_expression(self.cucx, &node.init)?;

        let tuple_ref_ty = tuple.get_type();

        let (tuple_len, tuple_mutability) =
            if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
                match rty.refee_ty.kind.as_ref() {
                    TyKind::Tuple(tuple_ty) => (tuple_ty.len(), tuple_ref_ty.mutability),

                    _ => {
                        return Err(CodegenError::InitializerTupleUnpackingMustBeTuple {
                            span: node.init.span,
                        })
                    }
                }
            } else {
                return Err(CodegenError::InitializerTupleUnpackingMustBeTuple {
                    span: node.init.span,
                });
            };

        if node.names.len() != tuple_len {
            return Err(CodegenError::NumberOfTupleFieldsDoesNotMatch {
                lens: (node.names.len(), tuple_len),
                span: node.span,
            });
        }

        // Unpacking
        for (index, name_and_mutability) in node.names.iter().enumerate() {
            let (name, mutability) = match name_and_mutability {
                Some(x) => (&x.0, x.1),

                // Ignore field
                None => continue,
            };

            if mutability.is_mut() && tuple_mutability.is_not() {
                todo!("ERROR")
            }

            self.unpack_one_tuple_field(&tuple, index as u64, name, mutability, node.span)?;
        }

        Ok(())
    }

    fn unpack_one_tuple_field(
        &mut self,
        tuple: &Value<'ctx>,
        index: u64,
        unpacked_name: &Ident,
        unpacked_mutability: Mutability,
        span: Span,
    ) -> CodegenResult<()> {
        assert!(matches!(
            tuple.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let tuple_ref_ty = tuple.get_type();

        let tuple_ty = if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
            rty
        } else {
            unreachable!();
        };

        let unpacked_value = build_tuple_indexing(
            self.cucx,
            tuple.get_value().into_pointer_value(),
            index,
            &tuple_ty.refee_ty,
            span,
        )?;

        self.normal_let_internal(
            unpacked_name,
            unpacked_mutability,
            Some(unpacked_value),
            Ty {
                kind: TyKind::Inferred.into(),
                mutability: unpacked_mutability,
            }
            .into(),
            span,
        )?;

        Ok(())
    }
}
