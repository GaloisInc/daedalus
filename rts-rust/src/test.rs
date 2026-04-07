use crate as ddl;
use std::env;
use std::fs;
use std::process;
use serde::Serialize;

pub fn test_parser<T: Serialize, F: FnOnce(ddl::Input) -> Option<(T,ddl::Input)>>(f: F) {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];

    let bytes = fs::read(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        process::exit(1);
    });
    let nm_arr = ddl::new_byte_array(filename.as_bytes());
    let byte_arr = ddl::new_byte_array(&bytes);

    match f(ddl::new_input(nm_arr,byte_arr)) {
      None => println!("parse error"),
      Some((a,_)) => {
          match serde_json::to_string_pretty(&a) {
              Ok(json) => println!("{}", json),
              Err(err) => eprintln!("Serialization error: {}", err)
          }
      }
    }
}

