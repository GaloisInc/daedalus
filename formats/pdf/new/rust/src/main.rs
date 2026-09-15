use daedalus_rts_rust as ddl;
use daedalus_pdf_cos::prepare_pdf;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} PDF_FILE", args[0]);
        process::exit(1);
    }

    let file = &args[1];
    let bytes = fs::read(file).unwrap_or_else(|err| {
        eprintln!("Error reading '{}': {}", file, err);
        process::exit(1);
    });

    let input = ddl::new_input(
        ddl::new_byte_array(file.as_bytes()),
        ddl::new_byte_array(&bytes),
    );

    match prepare_pdf(input) {
        Ok(pdf) => {
            println!(
                "Prepared '{}' with {} cross-reference entries.",
                file,
                pdf.user_state.entries.len()
            );
        }
        Err(err) => {
            eprintln!("Error preparing '{}': {}", file, err);
            process::exit(2);
        }
    }
}
