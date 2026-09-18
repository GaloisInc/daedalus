//! LZW decompression for PDF streams.

const CLEAR_TABLE: u16 = 256;
const END_OF_DATA: u16 = 257;
const FIRST_DYNAMIC_CODE: usize = 258;
const MAX_TABLE_SIZE: usize = 4096;

#[derive(Clone, Copy)]
struct Entry {
    prefix: u16,
    suffix: u8,
}

/// Decompress a PDF LZW stream.
///
/// Codes are packed most-significant bit first and grow from 9 to 12 bits.
/// `early_change` controls whether each width change happens one code early
/// (`1`, the PDF default) or is postponed until necessary (`0`).
pub(crate) fn decode(input: &[u8], early_change: u8) -> Result<Vec<u8>, String> {
    if early_change > 1 {
        return Err(format!("invalid LZW EarlyChange value {early_change}"));
    }

    let mut bits = BitReader::new(input);
    let mut dictionary = initial_dictionary();
    let mut code_width = 9;
    let mut previous = None;
    let mut output = Vec::new();
    let mut scratch = Vec::new();

    let first = bits
        .read(code_width)
        .ok_or_else(|| "missing initial LZW clear-table code".to_owned())?;
    if first != CLEAR_TABLE {
        return Err(format!(
            "LZW stream starts with code {first}, expected clear-table code"
        ));
    }

    loop {
        let code = bits
            .read(code_width)
            .ok_or_else(|| "LZW stream ended before end-of-data code".to_owned())?;

        match code {
            CLEAR_TABLE => {
                dictionary.truncate(FIRST_DYNAMIC_CODE);
                code_width = 9;
                previous = None;
            }
            END_OF_DATA => return Ok(output),
            _ => {
                if previous.is_none() {
                    if code >= 256 {
                        return Err(format!(
                            "invalid first LZW code {code} after clear-table code"
                        ));
                    }
                    output.push(code as u8);
                    previous = Some(code);
                    continue;
                }

                if dictionary.len() == MAX_TABLE_SIZE {
                    return Err("LZW table is full; expected clear-table code".to_owned());
                }

                scratch.clear();
                let first_byte;
                if usize::from(code) < dictionary.len() {
                    first_byte = expand_code(code, &dictionary, &mut scratch)?;
                    output.extend(scratch.iter().rev().copied());
                } else if usize::from(code) == dictionary.len() {
                    first_byte =
                        expand_code(previous.unwrap(), &dictionary, &mut scratch)?;
                    output.extend(scratch.iter().rev().copied());
                    output.push(first_byte);
                } else {
                    return Err(format!(
                        "LZW code {code} is beyond the next table entry {}",
                        dictionary.len()
                    ));
                }

                dictionary.push(Entry {
                    prefix: previous.unwrap(),
                    suffix: first_byte,
                });
                previous = Some(code);

                if code_width < 12
                    && dictionary.len() + usize::from(early_change)
                        == 1_usize << code_width
                {
                    code_width += 1;
                }
            }
        }
    }
}

fn initial_dictionary() -> Vec<Entry> {
    let mut dictionary = Vec::with_capacity(MAX_TABLE_SIZE);
    for byte in 0..=u8::MAX {
        dictionary.push(Entry {
            prefix: 0,
            suffix: byte,
        });
    }

    // Reserve the clear-table and end-of-data code positions.
    dictionary.push(Entry {
        prefix: 0,
        suffix: 0,
    });
    dictionary.push(Entry {
        prefix: 0,
        suffix: 0,
    });
    dictionary
}

/// Expand a dictionary code into `scratch` in reverse byte order.
///
/// The first byte of the expanded sequence is returned for constructing the
/// next dictionary entry.
fn expand_code(
    mut code: u16,
    dictionary: &[Entry],
    scratch: &mut Vec<u8>,
) -> Result<u8, String> {
    while code >= FIRST_DYNAMIC_CODE as u16 {
        let entry = dictionary
            .get(usize::from(code))
            .ok_or_else(|| format!("invalid LZW dictionary code {code}"))?;
        scratch.push(entry.suffix);
        code = entry.prefix;
    }

    if code >= 256 {
        return Err(format!("invalid LZW dictionary prefix {code}"));
    }

    let first = code as u8;
    scratch.push(first);
    Ok(first)
}

/// Reads bits high-order first, as required by ISO 32000-2:2017 section
/// 7.4.4.2.
struct BitReader<'a> {
    /// Complete bytes not yet loaded into `current`.
    data: &'a [u8],
    /// Current byte, with unread bits aligned to the high end.
    current: u8,
    /// Number of unread bits remaining in `current`.
    available: u8,
}

impl<'a> BitReader<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self {
            data: input,
            current: 0,
            available: 0,
        }
    }

    fn read(&mut self, mut width: u8) -> Option<u16> {
        let mut value = 0;
        while width > 0 {
            if self.available == 0 {
                let (&current, data) = self.data.split_first()?;
                self.current = current;
                self.data = data;
                self.available = 8;
            }

            let amount = width.min(self.available);
            width -= amount;
            value <<= amount;

            if amount == 8 {
                value |= u16::from(self.current);
                self.available = 0;
            } else {
                value |= u16::from(self.current >> (8 - amount));
                self.current <<= amount;
                self.available -= amount;
            }
        }

        Some(value)
    }
}
