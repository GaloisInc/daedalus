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



pub enum L<T> {
  Nil,
  Cons(Node<T>)
}

pub struct Node<T> {
  pub head: T,
  pub tail: ddl::O<L<T>>
}

// Example using the serialize_enum macro
crate::serialize_enum!(<T>, L<T>,
  (L::Nil, "Nil", &()),
  (L::Cons(x), "Cons", x)
);

// Example using the serialize_struct macro
crate::serialize_struct!(<T>, Node<T>,
  (head, "head"),
  (tail, "tail")
);