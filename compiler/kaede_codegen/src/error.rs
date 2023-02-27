use kaede_location::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("{}:{} '{}' was not declared in this scope", span.start.line, span.start.column, .name)]
    Undeclared { name: String, span: Span },

    #[error("{}:{} Variable '{}' declared void", span.start.line, span.start.column, .name)]
    VoidVariable { name: String, span: Span },

    #[error("{}:{} `break` outside of a loop", span.start.line, span.start.column)]
    BreakOutsideOfLoop { span: Span },

    /// Error issued by LLVM.
    #[error("{}", .what)]
    LLVMError { what: String },
}

pub type CodegenResult<T> = Result<T, CodegenError>;
