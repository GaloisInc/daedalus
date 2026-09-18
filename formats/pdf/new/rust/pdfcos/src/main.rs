use daedalus_pdf_cos::{PdfCos, Ref, prepare_pdf_bytes, references, resolve_reference};
use daedalus_rts_rust as ddl;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.as_slice() {
        [_, command, file] if command == "list" => list(file),
        [_, command, file, object, generation] if command == "resolve" => {
            resolve(file, parse_number("object", object), parse_number("generation", generation))
        }
        _ => usage(&args[0]),
    }
}

fn list(file: &str) {
    let pdf = load(file);
    for reference in references(&pdf) {
        println!("{} {} R", reference.obj, reference.r#gen);
    }
}

fn resolve(file: &str, object: u64, generation: u64) {
    let mut pdf = load(file);
    let reference = Ref {
        obj: ddl::Int::from(object),
        r#gen: ddl::Int::from(generation),
    };

    match resolve_reference(&mut pdf, reference) {
        Ok(Some(value)) => match serde_json::to_string_pretty(&value) {
            Ok(json) => println!("{json}"),
            Err(error) => exit_with_error(3, &format!("failed to print object: {error}")),
        },
        Ok(None) => exit_with_error(
            3,
            &format!("PDF object {object} {generation} was not found"),
        ),
        Err(error) => exit_with_error(3, &error),
    }
}

fn load(file: &str) -> PdfCos {
    let bytes = fs::read(file)
        .unwrap_or_else(|error| exit_with_error(1, &format!("failed to read '{file}': {error}")));
    prepare_pdf_bytes(file, &bytes)
        .unwrap_or_else(|error| exit_with_error(2, &format!("failed to prepare '{file}': {error}")))
}

fn parse_number(name: &str, value: &str) -> u64 {
    value
        .parse()
        .unwrap_or_else(|error| exit_with_error(1, &format!("invalid {name} '{value}': {error}")))
}

fn usage(program: &str) -> ! {
    eprintln!("Usage:");
    eprintln!("  {program} list PDF_FILE");
    eprintln!("  {program} resolve PDF_FILE OBJECT GENERATION");
    process::exit(1);
}

fn exit_with_error(code: i32, message: &str) -> ! {
    eprintln!("Error: {message}");
    process::exit(code);
}
