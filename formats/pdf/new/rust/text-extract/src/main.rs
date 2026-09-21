use daedalus_pdf_text_extract::extract_text_bytes;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args = env::args_os();
    let program = args.next().unwrap_or_default();
    let Some(input_path) = args.next().map(PathBuf::from) else {
        return usage(&program);
    };
    if args.next().is_some() {
        return usage(&program);
    }

    let bytes = match fs::read(&input_path) {
        Ok(bytes) => bytes,
        Err(error) => {
            eprintln!("{}: {error}", input_path.display());
            return ExitCode::FAILURE;
        }
    };

    let text = match extract_text_bytes(&input_path.to_string_lossy(), &bytes) {
        Ok(text) => text,
        Err(error) => {
            eprintln!("{}: {error}", input_path.display());
            return ExitCode::FAILURE;
        }
    };

    if let Err(error) = io::stdout().lock().write_all(text.as_bytes()) {
        eprintln!("standard output: {error}");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn usage(program: &std::ffi::OsStr) -> ExitCode {
    eprintln!("Usage: {} INPUT.pdf", PathBuf::from(program).display());
    ExitCode::FAILURE
}
