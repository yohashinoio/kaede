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

    #[error("{}:{} Mismatched types: {} vs {}", span.start.line, span.start.column, types.0, types.1)]
    MismatchedTypes { types: (String, String), span: Span },

    #[error("{}:{} Invalid left-hand side of assignment", span.start.line, span.start.column)]
    InvalidLeftOfAssignment { span: Span },

    #[error("{}:{} Cannot assign twice to immutable variable", span.start.line, span.start.column)]
    CannotAssignTwiceToImutable { span: Span },

    #[error("{}:{} Has no fields", span.start.line, span.start.column)]
    HasNoFields { span: Span },

    #[error("Failed to lookup target '{}': {}", triple, what)]
    FailedToLookupTarget { triple: String, what: String },

    #[error("Failed to create target machine")]
    FailedToCreateTargetMachine,

    #[error("{}:{} File not found for module `{}`", span.start.line, span.start.column, mod_name)]
    FileNotFoundForModule { span: Span, mod_name: String },

    #[error("{}:{} Tuples require access by index", span.start.line, span.start.column)]
    TupleRequireAccessByIndex { span: Span },

    #[error("{}:{} Index '{}' is out of range", span.start.line, span.start.column, index)]
    IndexOutOfRange { index: u64, span: Span },

    #[error("{}:{} An initializer for tuple unpacking must be a tuple", span.start.line, span.start.column)]
    InitializerTupleUnpackingMustBeTuple { span: Span },

    #[error("{}:{} Number of tuple fields does not match ({} vs {})", span.start.line, span.start.column, lens.0, lens.1)]
    NumberOfTupleFieldsDoesNotMatch { lens: (usize, usize), span: Span },

    #[error("{}:{} 'if' must have both main and 'else' branches if used as an expression", span.start.line, span.start.column)]
    IfMustHaveElseUsedAsExpr { span: Span },

    #[error("{}:{} `if` and `else` have incompatible types: {} vs {}", span.start.line, span.start.column, types.0, types.1)]
    IfAndElseHaveIncompatibleTypes { types: (String, String), span: Span },

    #[error("{}:{} Cannot assign an immutable to a mutable", span.start.line, span.start.column)]
    CannotAssignImmutableToMutable { span: Span },

    /// Error issued by LLVM
    #[error("{}", .what)]
    LLVMError { what: String },
}

pub type CodegenResult<T> = Result<T, CodegenError>;
