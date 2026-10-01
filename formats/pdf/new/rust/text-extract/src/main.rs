use daedalus_pdf_text_extract::{extract_chunks_bytes, extract_page_chunks_bytes};
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
        Some(page) => extract_page_chunks_bytes(&name, &bytes, page),
        None => extract_chunks_bytes(&name, &bytes),
    };
    let chunks = match result {
        Ok(chunks) => chunks,
        Err(error) => {
            eprintln!("{}: {error}", input_path.display());
            return ExitCode::FAILURE;
        }
    };

    let mut output = io::stdout().lock();
    for chunk in chunks {
        let result = match chunk.bounding_box {
            Some(bounds) => writeln!(
                output,
                "page {} bbox ({}, {})-({}, {}) text {:?}",
                chunk.page_number,
                bounds.min.x,
                bounds.min.y,
                bounds.max.x,
                bounds.max.y,
                chunk.text
            ),
            None => writeln!(
                output,
                "page {} bbox unknown text {:?}",
                chunk.page_number, chunk.text
            ),
        };
        if let Err(error) = result {
            eprintln!("standard output: {error}");
            return ExitCode::FAILURE;
        }
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
