#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    InvalidModuleName,
    Unimplemented { message: String },
    InternalError { message: String },
}
