//! Predictor reversal for PDF LZW and Flate stream filters.

pub(crate) fn apply_predictor(
    input: Vec<u8>,
    predictor: u64,
    colors: usize,
    bits_per_component: usize,
    columns: usize,
) -> Result<Vec<u8>, String> {
    match predictor {
        1 => Ok(input),
        10..=15 => png_predictor(input, colors, bits_per_component, columns),
        _ => Err(format!(
            "unsupported predictor parameters: Predictor={predictor}, Colors={colors}, \
             BitsPerComponent={bits_per_component}"
        )),
    }
}

/// Reverse the PNG row predictors used by PDF Predictor values 10 through 15.
/// Each encoded row begins with a tag selecting None, Sub, Up, Average, or
/// Paeth. The tag is removed while the reconstructed rows are compacted in
/// place.
///
/// See ISO 32000-2:2017 section 7.4.4.4 and
/// https://en.wikipedia.org/wiki/PNG#Filtering.
fn png_predictor(
    mut input: Vec<u8>,
    colors: usize,
    bits_per_component: usize,
    columns: usize,
) -> Result<Vec<u8>, String> {
    if colors == 0 {
        return Err("invalid PNG predictor parameter Colors".to_owned());
    }
    if !matches!(bits_per_component, 1 | 2 | 4 | 8 | 16) {
        return Err("invalid PNG predictor parameter BitsPerComponent".to_owned());
    }
    if columns == 0 {
        return Err("invalid PNG predictor parameter Columns".to_owned());
    }

    let bits_per_pixel = colors
        .checked_mul(bits_per_component)
        .ok_or_else(|| "PNG predictor pixel size does not fit in usize".to_owned())?;
    let bytes_per_pixel = bits_per_pixel
        .checked_add(7)
        .ok_or_else(|| "PNG predictor pixel size does not fit in usize".to_owned())?
        / 8;
    let row_bytes = bits_per_pixel
        .checked_mul(columns)
        .and_then(|bits| bits.checked_add(7))
        .ok_or_else(|| "PNG predictor row size does not fit in usize".to_owned())?
        / 8;
    let row_size = row_bytes
        .checked_add(1)
        .ok_or_else(|| "PNG predictor row size does not fit in usize".to_owned())?;
    if input.len() % row_size != 0 {
        return Err("truncated PNG predictor row".to_owned());
    }
    if input.is_empty() {
        return Ok(input);
    }

    let first_tag = input[0];
    validate_png_predictor_tag(first_tag)?;
    for column in 0..row_bytes {
        let byte = input[column + 1];
        let left = if column < bytes_per_pixel {
            0
        } else {
            input[column - bytes_per_pixel]
        };
        input[column] = byte.wrapping_add(png_prediction(first_tag, left, 0, 0));
    }

    let mut read_row = row_size;
    let mut write = row_bytes;
    while read_row < input.len() {
        let tag = input[read_row];
        validate_png_predictor_tag(tag)?;

        let row_end = read_row + row_size;
        for (column, read) in (read_row + 1..row_end).enumerate() {
            let byte = input[read];
            let left = if column < bytes_per_pixel {
                0
            } else {
                input[write + column - bytes_per_pixel]
            };
            let above = input[write - row_bytes + column];
            let upper_left = if column < bytes_per_pixel {
                0
            } else {
                input[write - row_bytes + column - bytes_per_pixel]
            };
            input[write + column] =
                byte.wrapping_add(png_prediction(tag, left, above, upper_left));
        }
        write += row_bytes;
        read_row = row_end;
    }

    input.truncate(write);
    Ok(input)
}

fn validate_png_predictor_tag(tag: u8) -> Result<(), String> {
    if tag <= 4 {
        Ok(())
    } else {
        Err(format!("unsupported PNG predictor tag {tag}"))
    }
}

fn png_prediction(tag: u8, left: u8, above: u8, upper_left: u8) -> u8 {
    match tag {
        0 => 0,
        1 => left,
        2 => above,
        3 => ((u16::from(left) + u16::from(above)) / 2) as u8,
        4 => paeth_predictor(left, above, upper_left),
        _ => unreachable!("PNG predictor tag was validated"),
    }
}

fn paeth_predictor(left: u8, above: u8, upper_left: u8) -> u8 {
    let left = i32::from(left);
    let above = i32::from(above);
    let upper_left = i32::from(upper_left);
    let estimate = left + above - upper_left;
    let left_distance = (estimate - left).abs();
    let above_distance = (estimate - above).abs();
    let upper_left_distance = (estimate - upper_left).abs();

    if left_distance <= above_distance && left_distance <= upper_left_distance {
        left as u8
    } else if above_distance <= upper_left_distance {
        above as u8
    } else {
        upper_left as u8
    }
}
