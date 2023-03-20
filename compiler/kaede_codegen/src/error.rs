use kaede_span::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("{}:{} '{}' was not declared in this scope", span.start.line, span.start.column, .name)]
    Undeclared { name: String, span: Span },

    #[error("{}:{} Variable '{}' declared void", span.start.line, span.start.column, .name)]
    VoidVariable { name: String, span: Span },

    #[error("{}:{} `break` outside of a loop", span.start.line, span.start.column)]
    BreakOutsideOfLoop { span: Span },

    #[error("{}:{} Mismatched types", span.start.line, span.start.column)]
    MismatchedTypes { span: Span },

    #[error("{}:{} Invalid left-hand side of assignment", span.start.line, span.start.column)]
    InvalidLeftOfAssignment { span: Span },

    #[error("{}:{} Cannot assign twice to immutable variable `{}`", span.start.line, span.start.column, .name)]
    CannotAssignTwiceToImutable { name: String, span: Span },

    #[error("{}:{} Cannot assign to immutable reference", span.start.line, span.start.column)]
    CannotAssignToImutableRef { span: Span },

    #[error("{}:{} Has no fields", span.start.line, span.start.column)]
    HasNoFields { span: Span },

    #[error("{}:{} Type `{}` cannot be dereferenced", span.start.line, span.start.column, ty)]
    CannotDeref { ty: String, span: Span },

    #[error("{}:{} Cannot borrow `{}` as mutable, as it is not declared as mutable",
    span.start.line, span.start.column, immutable_var)]
    MutableBorrowingFromImmutable { immutable_var: String, span: Span },

    #[error("Failed to lookup target '{}': {}", triple, what)]
    FailedToLookupTarget { triple: String, what: String },

    #[error("Failed to create target machine")]
    FailedToCreateTargetMachine,

    /// Error issued by LLVM
    #[error("{}", .what)]
    LLVMError { what: String },
}

pub type CodegenResult<T> = Result<T, CodegenError>;
