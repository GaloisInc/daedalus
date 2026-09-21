use daedalus_pdf_text_extract::{extract_page_text_bytes, extract_text_bytes};
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args = env::args_os();
    let program = args.next().unwrap_or_default();
    let Some(first) = args.next() else {
        return usage(&program);
    };
    let (page, input_path) = if first == "--page" || first == "-p" {
        let Some(page) = args.next().as_deref().and_then(parse_page) else {
            return usage(&program);
        };
        let Some(input_path) = args.next().map(PathBuf::from) else {
            return usage(&program);
        };
        (Some(page), input_path)
    } else {
        (None, PathBuf::from(first))
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

    let name = input_path.to_string_lossy();
    let result = match page {
        Some(page) => extract_page_text_bytes(&name, &bytes, page),
        None => extract_text_bytes(&name, &bytes),
    };
    let text = match result {
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

fn parse_page(page: &OsStr) -> Option<u64> {
    page.to_str()?.parse().ok().filter(|page| *page > 0)
}

fn usage(program: &std::ffi::OsStr) -> ExitCode {
    eprintln!(
        "Usage: {} [--page PAGE] INPUT.pdf",
        PathBuf::from(program).display()
    );
    ExitCode::FAILURE
}
