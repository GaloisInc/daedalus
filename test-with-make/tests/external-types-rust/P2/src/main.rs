use daedalus_rts_rust as ddl;

mod generated;

fn main() {
    let input = ddl::new_input(
        ddl::new_byte_array(b""),
        ddl::new_byte_array(b""),
    );
    let mut state = ddl::new_parser_state();

    match generated::P2::P2(&mut state, input) {
        ddl::ParserResult::Ok(value, _) => match value {
            p1::P1::P1::A => println!("A"),
            p1::P1::P1::B => println!("B"),
        },
        ddl::ParserResult::Failure => println!("parse failure"),
        ddl::ParserResult::Exception => println!("parser exception"),
    }
}
