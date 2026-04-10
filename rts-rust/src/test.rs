use crate as ddl;
use std::env;
use std::fs;
use std::process;
use serde::Serialize;


pub fn test_parser<T: Serialize, F: FnOnce(&mut ddl::ParserState, ddl::Input) -> Option<(T,ddl::Input)>>(f: F) {
    let args: Vec<String> = env::args().collect();

    let (nm_arr, byte_arr) = if args.len() == 1 {
        // No file provided, use empty input
        let empty = b"";
        (ddl::new_byte_array(empty), ddl::new_byte_array(empty))
    } else if args.len() == 2 {
        let filename = &args[1];
        let bytes = fs::read(filename).unwrap_or_else(|err| {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        });
        (ddl::new_byte_array(filename.as_bytes()), ddl::new_byte_array(&bytes))
    } else {
        eprintln!("Usage: {} [<filename>]", args[0]);
        process::exit(1);
    };

    let mut pstate = ddl::new_parser_state();

    match f(&mut pstate, ddl::new_input(nm_arr, byte_arr)) {
      None =>
        println!("parse error: {}", pstate.error),
      Some((a,_)) => {
          match serde_json::to_string_pretty(&a) {
              Ok(json) => println!("[{}]", json),
              Err(err) => eprintln!("Serialization error: {}", err)
          }
      }
    }
}

