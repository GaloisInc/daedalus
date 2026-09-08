use daedalus_rts_rust as ddl;
use std::env;
use std::fs;
use std::process;

mod format;
mod native;

use native::State;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} FILE", args[0]);
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
    let mut state = ddl::new_parser_state_with(State::new(b'A'));

    match format::packets(&mut state, input) {
        ddl::ParserResult::Ok(result, _) => {
            println!("{}", serde_json::to_string(&result).unwrap());
            println!(
                "observed by native code: {}",
                serde_json::to_string(state.user_state.observed_packets()).unwrap()
            );
            println!(
                "native totals: {} special, {} normal",
                state.user_state.special_packets(),
                state.user_state.normal_packets()
            );
        }
        ddl::ParserResult::Failure => {
            eprintln!("Parse error: {}", state.error);
            process::exit(2);
        }
        ddl::ParserResult::Exception => {
            eprintln!("Exception: {}", state.error);
            process::exit(3);
        }
    }
}
