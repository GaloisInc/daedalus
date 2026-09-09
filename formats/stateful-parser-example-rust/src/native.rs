use crate::format::Packet;
use daedalus_rts_rust as ddl;

// This state contains values of a type defined by the generated parser.
pub struct State {
    special: u8,
    observed_packets: Vec<Packet>,
    special_packets: usize,
    normal_packets: usize,
}

impl State {
    pub fn new(special: u8) -> Self {
        State {
            special,
            observed_packets: Vec::new(),
            special_packets: 0,
            normal_packets: 0,
        }
    }

    pub fn observed_packets(&self) -> &[Packet] {
        &self.observed_packets
    }

    pub fn special_packets(&self) -> usize {
        self.special_packets
    }

    pub fn normal_packets(&self) -> usize {
        self.normal_packets
    }
}

// Implementation of the `SetSpecial` parser.
pub fn set_special(
    state: &mut ddl::ParserStateWith<State>,
    input: ddl::Input,
    value: ddl::U<8>,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.special = u8::from(value);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

// Implementation of the `GetSpecial` parser.
pub fn get_special(
    state: &mut ddl::ParserStateWith<State>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::U<8>> {
    let value = ddl::U::<8>::from(state.user_state.special);
    ddl::ParserResult::Ok(value, input)
}

// This function depends essentially on the generated Packet type: it examines
// its constructors and stores the packet in the application state.
pub fn observe_packet(
    state: &mut ddl::ParserStateWith<State>,
    input: ddl::Input,
    packet: Packet,
) -> ddl::ParserResult<Packet> {
    match &packet {
        Packet::Special(byte) => {
            state.user_state.special_packets += 1;
            println!("native: special packet {}", u8::from(*byte));
        }
        Packet::Normal(byte) => {
            state.user_state.normal_packets += 1;
            println!("native: normal packet {}", u8::from(*byte));
        }
    }

    state.user_state.observed_packets.push(packet.clone());
    ddl::ParserResult::Ok(packet, input)
}
