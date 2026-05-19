use crate as ddl;

/// The outcome of calling a parser.
pub enum ParserResult<T> {
    Ok(T, ddl::Input),
    Failure,
    Exception,
}

/// The result of a pure function that may throw an exception.
pub enum PureResult<T> {
    Ok(T),
    Exception(&'static str, &'static str),
}
