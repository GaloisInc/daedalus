//! Lazy resolution and parsing of indirect PDF objects.

use crate::pdfcos::{ObjectStreamState, Pdf, PdfCos, PdfObject, ReferenceState, with_fresh_error};
use crate::pdfcos_parsers::{ObjStream, Ref, TopDecl, TopDeclDef};
use daedalus_rts_rust as ddl;
use ddl::Type;

const MAX_RESOLUTION_DEPTH: usize = 256;

/// Resolve and parse an indirect PDF object, caching the result.
///
/// Returns `None` if the reference is absent, has a mismatched generation,
/// or denotes a null object.
pub fn resolve_reference(pdf: &mut PdfCos, reference: Ref) -> Result<Option<TopDecl>, String> {
    let input = pdf.user_state.input.clone();
    let (resolved, parse_error) =
        with_fresh_error(pdf, |pdf| resolve_reference_parser(pdf, input, reference));
    match resolved {
        ddl::ParserResult::Ok(ddl::Maybe::Just(value), _) => Ok(Some(value)),
        ddl::ParserResult::Ok(ddl::Maybe::Nothing, _) => Ok(None),
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => Err(parse_error.to_string()),
    }
}

/// Parser-facing reference resolution used by the generated native primitive.
pub(crate) fn resolve_reference_parser(
    parser_state: &mut PdfCos,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    let object = match reference.obj.try_to_unsigned() {
        Some(object) => object,
        None => {
            return resolve_failure(
                parser_state,
                &input,
                "PDF reference has an invalid object number",
            );
        }
    };
    let requested_generation = match reference.r#gen.try_to_unsigned() {
        Some(generation) => generation,
        None => {
            return resolve_failure(
                parser_state,
                &input,
                "PDF reference has an invalid generation number",
            );
        }
    };

    let entry = match parser_state.user_state.entries.get(&object).cloned() {
        Some(entry) => entry,
        None => return ddl::ParserResult::Ok(ddl::Maybe::Nothing, input),
    };

    let PdfObject { generation, state } = entry;
    if generation != requested_generation {
        return ddl::ParserResult::Ok(ddl::Maybe::Nothing, input);
    }

    match state {
        ReferenceState::Parsed(value) => ddl::ParserResult::Ok(ddl::Maybe::Just(value), input),

        ReferenceState::Null => ddl::ParserResult::Ok(ddl::Maybe::Nothing, input),

        ReferenceState::Loading => {
            let error = format!("recursive resolution of PDF object {object} {generation}");
            cache_resolve_error(parser_state, &input, object, generation, error, false)
        }

        ReferenceState::Failed { error, exception } => {
            replay_resolve_error(parser_state, &input, &error, exception)
        }

        ReferenceState::InObjectStream { container, index } => {
            let depth = next_resolution_depth(&parser_state.user_state);
            if depth > MAX_RESOLUTION_DEPTH {
                let error = format!(
                    "PDF object resolution exceeds the maximum depth of {MAX_RESOLUTION_DEPTH}"
                );
                return cache_resolve_error(parser_state, &input, object, generation, error, false);
            }
            parser_state.user_state.entries.insert(
                object,
                PdfObject {
                    generation,
                    state: ReferenceState::Loading,
                },
            );

            let error_input = input.clone();
            let previous_object = parser_state
                .user_state
                .current_object
                .replace((object, generation, depth));
            let (parsed, parse_error) = with_fresh_error(parser_state, |parser_state| {
                resolve_compressed_object(parser_state, input, object, container, index)
            });
            parser_state.user_state.current_object = previous_object;

            match parsed {
                ddl::ParserResult::Ok(value, input) => {
                    parser_state.user_state.entries.insert(
                        object,
                        PdfObject {
                            generation,
                            state: ReferenceState::Parsed(value.clone()),
                        },
                    );
                    ddl::ParserResult::Ok(ddl::Maybe::Just(value), input)
                }
                ddl::ParserResult::Failure => {
                    let error = format!(
                        "failed to parse PDF object {object} {generation} from object stream {container}:\n{}",
                        parse_error
                    );
                    cache_resolve_error(
                        parser_state,
                        &error_input,
                        object,
                        generation,
                        error,
                        false,
                    )
                }
                ddl::ParserResult::Exception => {
                    let error = format!(
                        "exception while parsing PDF object {object} {generation} from object stream {container}:\n{}",
                        parse_error
                    );
                    cache_resolve_error(parser_state, &error_input, object, generation, error, true)
                }
            }
        }

        ReferenceState::AtOffset(offset) => {
            let depth = next_resolution_depth(&parser_state.user_state);
            if depth > MAX_RESOLUTION_DEPTH {
                let error = format!(
                    "PDF object resolution exceeds the maximum depth of {MAX_RESOLUTION_DEPTH}"
                );
                return cache_resolve_error(parser_state, &input, object, generation, error, false);
            }
            parser_state.user_state.entries.insert(
                object,
                PdfObject {
                    generation,
                    state: ReferenceState::Loading,
                },
            );

            let object_input = match parser_state.user_state.input.clone().advance_maybe(offset) {
                ddl::Maybe::Just(input) => input,
                ddl::Maybe::Nothing => {
                    let error =
                        format!("PDF object {object} {generation} has invalid offset {offset}");
                    return cache_resolve_error(
                        parser_state,
                        &input,
                        object,
                        generation,
                        error,
                        false,
                    );
                }
            };

            let error_input = object_input.clone();
            let previous_object = parser_state
                .user_state
                .current_object
                .replace((object, generation, depth));
            let (parsed, parse_error) = with_fresh_error(parser_state, |parser_state| {
                crate::pdfcos_parsers::top_decl(parser_state, object_input)
            });
            parser_state.user_state.current_object = previous_object;

            match parsed {
                ddl::ParserResult::Ok(value, _) => {
                    parser_state.user_state.entries.insert(
                        object,
                        PdfObject {
                            generation,
                            state: ReferenceState::Parsed(value.clone()),
                        },
                    );
                    ddl::ParserResult::Ok(ddl::Maybe::Just(value), input)
                }
                ddl::ParserResult::Failure => {
                    let error = format!(
                        "failed to parse PDF object {object} {generation}:\n{}",
                        parse_error
                    );
                    cache_resolve_error(
                        parser_state,
                        &error_input,
                        object,
                        generation,
                        error,
                        false,
                    )
                }
                ddl::ParserResult::Exception => {
                    let error = format!(
                        "exception while parsing PDF object {object} {generation}:\n{}",
                        parse_error
                    );
                    cache_resolve_error(parser_state, &error_input, object, generation, error, true)
                }
            }
        }
    }
}

fn next_resolution_depth(pdf: &Pdf) -> usize {
    let current_depth = pdf.current_object.map(|(_, _, depth)| depth).unwrap_or(0);
    current_depth.saturating_add(1)
}

/// Resolve and parse an object-stream container, caching its index and body.
fn resolve_object_stream(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    container: u64,
) -> ddl::ParserResult<ObjStream> {
    match parser_state
        .user_state
        .object_streams
        .get(&container)
        .cloned()
    {
        Some(ObjectStreamState::Parsed(object_stream)) => {
            ddl::ParserResult::Ok(object_stream, input)
        }

        Some(ObjectStreamState::Loading) => resolve_failure(
            parser_state,
            &input,
            &format!("recursive resolution of object stream {container}"),
        ),

        Some(ObjectStreamState::Failed { error, exception }) => {
            replay_resolve_error(parser_state, &input, &error, exception)
        }

        None => {
            parser_state
                .user_state
                .object_streams
                .insert(container, ObjectStreamState::Loading);

            let error_input = input.clone();
            let (parsed, parse_error) = with_fresh_error(parser_state, |parser_state| {
                parse_object_stream(parser_state, input, container)
            });

            match parsed {
                ddl::ParserResult::Ok(object_stream, input) => {
                    parser_state
                        .user_state
                        .object_streams
                        .insert(container, ObjectStreamState::Parsed(object_stream.clone()));
                    ddl::ParserResult::Ok(object_stream, input)
                }
                ddl::ParserResult::Failure => {
                    let error =
                        format!("failed to parse object stream {container}:\n{parse_error}");
                    cache_object_stream_error(parser_state, &error_input, container, error, false)
                }
                ddl::ParserResult::Exception => {
                    let error = format!(
                        "exception while parsing object stream {container}:\n{parse_error}"
                    );
                    cache_object_stream_error(parser_state, &error_input, container, error, true)
                }
            }
        }
    }
}

fn parse_object_stream(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    container: u64,
) -> ddl::ParserResult<ObjStream> {
    // Objects referenced by compressed cross-reference entries have generation 0.
    let container_reference = Ref {
        obj: ddl::Int::from(container),
        r#gen: ddl::Int::from(0_u64),
    };
    let (container_decl, input) =
        match resolve_reference_parser(parser_state, input, container_reference) {
            ddl::ParserResult::Ok(ddl::Maybe::Just(value), input) => (value, input),
            ddl::ParserResult::Ok(ddl::Maybe::Nothing, input) => {
                return resolve_failure(
                    parser_state,
                    &input,
                    &format!("object stream {container} was not found"),
                );
            }
            ddl::ParserResult::Failure => return ddl::ParserResult::Failure,
            ddl::ParserResult::Exception => return ddl::ParserResult::Exception,
        };

    let stream = match container_decl.obj {
        TopDeclDef::Stream(stream) => stream,
        TopDeclDef::Value(_) => {
            return resolve_failure(
                parser_state,
                &input,
                &format!("object stream container {container} is not a stream"),
            );
        }
    };

    crate::pdfcos_parsers::obj_stream(parser_state, input, stream)
}

/// Extract an object packed inside a cached PDF object stream.
fn resolve_compressed_object(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    _object: u64,
    container: u64,
    index: u64,
) -> ddl::ParserResult<TopDecl> {
    let (object_stream, input) = match resolve_object_stream(parser_state, input, container) {
        ddl::ParserResult::Ok(value, input) => (value, input),
        ddl::ParserResult::Failure => return ddl::ParserResult::Failure,
        ddl::ParserResult::Exception => return ddl::ParserResult::Exception,
    };

    crate::pdfcos_parsers::obj_stream_entry(
        parser_state,
        input,
        object_stream,
        ddl::U::<64>::from(index),
    )
}

fn cache_object_stream_error<T>(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    container: u64,
    error: String,
    exception: bool,
) -> ddl::ParserResult<T> {
    parser_state.user_state.object_streams.insert(
        container,
        ObjectStreamState::Failed {
            error: error.clone(),
            exception,
        },
    );
    replay_resolve_error(parser_state, input, &error, exception)
}

fn cache_resolve_error<T>(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    object: u64,
    generation: u64,
    error: String,
    exception: bool,
) -> ddl::ParserResult<T> {
    parser_state.user_state.entries.insert(
        object,
        PdfObject {
            generation,
            state: ReferenceState::Failed {
                error: error.clone(),
                exception,
            },
        },
    );
    replay_resolve_error(parser_state, input, &error, exception)
}

fn replay_resolve_error<T>(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    error: &str,
    exception: bool,
) -> ddl::ParserResult<T> {
    let result = resolve_failure(parser_state, input, error);
    if exception {
        ddl::ParserResult::Exception
    } else {
        result
    }
}

fn resolve_failure<T>(
    parser_state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    message: &str,
) -> ddl::ParserResult<T> {
    let message = ddl::new_byte_array(message.as_bytes());
    parser_state.note_fail(
        true,
        "pdf-cos-spec/PdfDecl.ddl:37:ResolveRef",
        input,
        message.bor(),
    );
    ddl::ParserResult::Failure
}
