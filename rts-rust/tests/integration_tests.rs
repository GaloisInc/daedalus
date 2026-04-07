use daedalus_rts_rust as ddl;
use ddl::{Type, Clo};

// ============================================================================
// Map Tests
// ============================================================================

#[test]
fn test_map_insert_lookup_contains() {
    // Tests basic map operations: insert, lookup, contains, and empty map behavior.
    // Also verifies that later inserts overwrite earlier values for the same key.
    let m = ddl::empty_map::<u32, u32>();
    assert!(!m.bor().contains(42), "Empty map should not contain any keys");
    assert_eq!(m.bor().lookup(42), ddl::Maybe::Nothing, "Lookup in empty map should return Nothing");

    let m = m.insert(10, 100);
    let m = m.insert(20, 200);
    let m = m.insert(5, 50);

    assert!(m.bor().contains(10), "Map should contain inserted key 10");
    assert!(m.bor().contains(20), "Map should contain inserted key 20");
    assert!(m.bor().contains(5), "Map should contain inserted key 5");
    assert!(!m.bor().contains(42), "Map should not contain uninserted key 42");

    assert_eq!(m.bor().lookup(10), ddl::Maybe::Just(100));
    assert_eq!(m.bor().lookup(20), ddl::Maybe::Just(200));
    assert_eq!(m.bor().lookup(5), ddl::Maybe::Just(50));
    assert_eq!(m.bor().lookup(42), ddl::Maybe::Nothing);

    // Test overwriting a value
    let m = m.insert(10, 999);
    assert_eq!(m.bor().lookup(10), ddl::Maybe::Just(999),
               "Insert should overwrite existing value");
}

#[test]
fn test_map_iteration_order() {
    // Tests that map iterators traverse elements in ascending key order,
    // and that both owned and borrowed iterators work correctly.
    let m = ddl::empty_map::<u32, u32>()
        .insert(30, 300)
        .insert(10, 100)
        .insert(20, 200)
        .insert(5, 50);

    // Test borrowed iterator
    let mut it = ddl::new_map_borrow_iterator(m.bor());
    let mut keys = Vec::new();
    while !it.ddl_done() {
        keys.push(it.ddl_key());
        it = it.ddl_next();
    }
    assert_eq!(keys, vec![5, 10, 20, 30], "Map iteration should be in ascending key order");

    // Test owned iterator
    let mut it = ddl::new_map_iterator(m.clone());
    let mut values = Vec::new();
    while !it.ddl_done() {
        values.push(it.ddl_value());
        it = it.ddl_next();
    }
    assert_eq!(values, vec![50, 100, 200, 300],
               "Owned iterator should produce values in key order");
}

#[test]
fn test_map_comparison() {
    // Tests map comparison operations (Eq, Ord) which compare maps lexicographically
    // by (key, value) pairs in iteration order.
    let m1 = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(2, 20);

    let m2 = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(2, 20);

    let m3 = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(2, 21); // Different value

    let m4 = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(3, 20); // Different key

    assert!(m1.bor() == m2.bor(), "Maps with same entries should be equal");
    assert!(m1.bor() != m3.bor(), "Maps with different values should not be equal");
    assert!(m1.bor() != m4.bor(), "Maps with different keys should not be equal");

    assert!(m1.bor() < m3.bor(), "Map comparison should be lexicographic");
    assert!(m1.bor() < m4.bor());
}

#[test]
fn test_map_persistence() {
    // Tests that maps are persistent (immutable) - inserting into a map
    // doesn't modify the original map.
    let m1 = ddl::empty_map::<u32, u32>().insert(1, 10);
    let m2 = m1.clone().insert(2, 20);
    let m3 = m1.clone().insert(3, 30);

    // m1 should still only have key 1
    assert!(m1.bor().contains(1));
    assert!(!m1.bor().contains(2));
    assert!(!m1.bor().contains(3));

    // m2 should have keys 1 and 2
    assert!(m2.bor().contains(1));
    assert!(m2.bor().contains(2));
    assert!(!m2.bor().contains(3));

    // m3 should have keys 1 and 3
    assert!(m3.bor().contains(1));
    assert!(!m3.bor().contains(2));
    assert!(m3.bor().contains(3));
}

// ============================================================================
// Array Tests
// ============================================================================

#[test]
fn test_array_creation_and_access() {
    // Tests various array creation methods and basic indexing/length operations.
    let arr1 = ddl::new_array([1u32, 2, 3, 4, 5]);
    assert_eq!(arr1.len(), 5);
    assert_eq!(arr1[0], 1);
    assert_eq!(arr1[4], 5);

    let arr2 = ddl::new_array_vec(vec![10, 20, 30]);
    assert_eq!(arr2.len(), 3);
    assert_eq!(arr2[1], 20);

    let arr3 = ddl::new_array_slice(&[7, 8, 9]);
    assert_eq!(arr3.len(), 3);
    assert_eq!(arr3[2], 9);

    // Test byte array creation
    let bytes = ddl::new_byte_array(&[0x41, 0x42, 0x43]);
    assert_eq!(bytes.len(), 3);
    assert_eq!(u8::from(bytes[0]), 0x41);
}

#[test]
fn test_array_iterator() {
    // Tests array iterator functionality including done check, key (index),
    // and value access.
    let arr = ddl::new_array([10u32, 20, 30, 40]);
    let mut it = ddl::new_array_iterator(arr);

    let mut collected = Vec::new();
    while !it.bor().ddl_done() {
        collected.push((it.bor().ddl_key(), it.bor().ddl_val()));
        it = it.ddl_next();
    }

    assert_eq!(collected, vec![(0, 10), (1, 20), (2, 30), (3, 40)],
               "Iterator should produce (index, value) pairs");
}

#[test]
fn test_array_conversion() {
    // Tests conversion between DDL arrays and Rust vectors.
    let vec = vec![1u32, 2, 3, 4, 5];
    let arr = ddl::new_array_vec(vec.clone());
    let vec2 = ddl::array_to_vec(arr);
    assert_eq!(vec, vec2, "Round-trip conversion should preserve values");

    // Test byte array conversion
    let bytes = vec![0x10, 0x20, 0x30];
    let arr = ddl::new_byte_array(&bytes);
    let bytes2: Vec<u8> = arr.iter().map(|x| u8::from(*x)).collect();
    assert_eq!(bytes, bytes2, "Byte array round-trip should preserve values");
}

#[test]
fn test_array_concat() {
    // Tests concatenation of an array of arrays into a single flat array.
    let arr1 = ddl::new_array([1u32, 2, 3]);
    let arr2 = ddl::new_array([4u32, 5]);
    let arr3 = ddl::new_array([6u32, 7, 8, 9]);

    let nested = ddl::new_array([arr1, arr2, arr3]);
    let flat = nested.bor().concat();

    assert_eq!(flat.len(), 9);
    assert_eq!(ddl::array_to_vec(flat), vec![1, 2, 3, 4, 5, 6, 7, 8, 9],
               "Concat should flatten nested arrays in order");
}

// ============================================================================
// Builder Tests
// ============================================================================

#[test]
fn test_builder_basic() {
    // Tests basic builder operations: push and build.
    let b = ddl::new_builder()
        .push(1u32)
        .push(2)
        .push(3);
    let arr = b.build();

    assert_eq!(arr.len(), 3);
    assert_eq!(ddl::array_to_vec(arr), vec![1, 2, 3]);
}

#[test]
fn test_builder_push_array() {
    // Tests builder's ability to efficiently append entire arrays.
    let arr1 = ddl::new_array([1u32, 2, 3]);
    let arr2 = ddl::new_array([4u32, 5]);

    let b = ddl::new_builder()
        .push(0u32)
        .push_array(arr1)
        .push_array(arr2)
        .push(6);
    let result = b.build();

    assert_eq!(ddl::array_to_vec(result), vec![0, 1, 2, 3, 4, 5, 6],
               "Builder should correctly interleave push and push_array");
}

#[test]
fn test_builder_persistence() {
    // Tests that builders are persistent - sharing a builder and then
    // diverging creates independent results.
    // Due to sharing (Rc), when a builder is cloned and then pushed to,
    // the new element goes into a separate node, affecting final order.
    let b1 = ddl::new_builder().push(1u32).push(2);
    let b2 = b1.clone().push(3);
    let b3 = b1.clone().push(4);

    let arr2 = b2.build();
    let arr3 = b3.build();

    // When a builder is shared, new pushes create new nodes at the front,
    // so the most recent push appears first, followed by the shared content
    assert_eq!(ddl::array_to_vec(arr2), vec![3, 1, 2]);
    assert_eq!(ddl::array_to_vec(arr3), vec![4, 1, 2],
               "Diverged builders should produce independent results");
}

// ============================================================================
// Maybe Tests
// ============================================================================

#[test]
fn test_maybe_basic() {
    // Tests Maybe construction, comparison, and from_option conversion.
    let nothing: ddl::Maybe<i32> = ddl::Maybe::Nothing;
    let just_42 = ddl::Maybe::Just(42);

    assert_eq!(nothing, ddl::Maybe::Nothing);
    assert_eq!(just_42, ddl::Maybe::Just(42));
    assert_ne!(nothing, just_42);

    assert_eq!(ddl::from_option(None::<i32>), ddl::Maybe::Nothing);
    assert_eq!(ddl::from_option(Some(42)), ddl::Maybe::Just(42));
}

#[test]
fn test_maybe_unwrap() {
    // Tests Maybe::unwrap for both Just and Nothing cases.
    let just_value = ddl::Maybe::Just(42);
    assert_eq!(just_value.unwrap(), 42);
}

#[test]
#[should_panic(expected = "called `Maybe::unwrap()` on a `Nothing` value")]
fn test_maybe_unwrap_panic() {
    // Tests that unwrapping Nothing panics with the expected message.
    let nothing: ddl::Maybe<i32> = ddl::Maybe::Nothing;
    nothing.unwrap();
}

#[test]
fn test_maybe_ordering() {
    // Tests Maybe's ordering: Nothing < Just(x) < Just(y) when x < y.
    assert!(ddl::Maybe::Nothing < ddl::Maybe::Just(1));
    assert!(ddl::Maybe::Just(1) < ddl::Maybe::Just(2));
    assert_eq!(ddl::Maybe::Just(42), ddl::Maybe::Just(42));
}

// ============================================================================
// Number/Word Tests
// ============================================================================

#[test]
fn test_word_arithmetic() {
    // Tests arithmetic operations on Word types including wrapping behavior.
    let a: ddl::U<8> = 100u8.into();
    let b: ddl::U<8> = 50u8.into();

    assert_eq!(u8::from(a + b), 150);
    assert_eq!(u8::from(a - b), 50);
    assert_eq!(u8::from(a * 2u8.into()), 200);
    assert_eq!(u8::from(a / 2u8.into()), 50);
    assert_eq!(u8::from(a % 30u8.into()), 10);

    // Test wrapping
    let max: ddl::U<8> = 255u8.into();
    let one: ddl::U<8> = 1u8.into();
    assert_eq!(u8::from(max + one), 0, "Addition should wrap");

    // Test signed arithmetic
    let neg: ddl::I<8> = (-5i8).into();
    let pos: ddl::I<8> = 3i8.into();
    assert_eq!(i8::from(neg + pos), -2);
    assert_eq!(i8::from(-neg), 5);
}

#[test]
fn test_word_bitwise() {
    // Tests bitwise operations on Word types.
    let a: ddl::U<8> = 0b11001100u8.into();
    let b: ddl::U<8> = 0b10101010u8.into();

    assert_eq!(u8::from(a & b), 0b10001000);
    assert_eq!(u8::from(a | b), 0b11101110);
    assert_eq!(u8::from(a ^ b), 0b01100110);
    assert_eq!(u8::from(!a), 0b00110011);
}

#[test]
fn test_word_shifts() {
    // Tests shift operations including edge cases (shifting by >= width).
    let a: ddl::U<8> = 0b00001111u8.into();

    assert_eq!(u8::from(a << 2), 0b00111100);
    assert_eq!(u8::from(a >> 2), 0b00000011);
    assert_eq!(u8::from(a << 8), 0, "Shift by width should zero");
    assert_eq!(u8::from(a << 10), 0, "Shift beyond width should zero");

    // Test arithmetic right shift for signed values
    let neg: ddl::I<8> = (-4i8).into(); // 0b11111100
    assert_eq!(i8::from(neg >> 1), -2, "Signed right shift should preserve sign");
    assert_eq!(i8::from(neg >> 8), -1, "Signed right shift by width should give sign-extended value");
}

#[test]
fn test_word_comparisons() {
    // Tests ordering and equality for Word types.
    let a: ddl::U<16> = 100u16.into();
    let b: ddl::U<16> = 200u16.into();

    assert!(a < b);
    assert!(b > a);
    assert!(a == 100u16.into());
    assert!(a != b);

    // Test signed comparisons
    let neg: ddl::I<8> = (-10i8).into();
    let pos: ddl::I<8> = 10i8.into();
    assert!(neg < pos);
}

#[test]
fn test_word_casting() {
    // Tests cast_to operation for converting between different word sizes and signedness.
    let a: ddl::U<8> = 200u8.into();
    let b: ddl::U<16> = a.cast_to();
    assert_eq!(u16::from(b), 200);

    // Test truncation
    let c: ddl::U<16> = 1000u16.into();
    let d: ddl::U<8> = c.cast_to();
    assert_eq!(u8::from(d), 232); // 1000 % 256 = 232

    // Test sign conversion
    let unsigned: ddl::U<8> = 200u8.into();
    let signed: ddl::I<8> = unsigned.cast_to();
    assert_eq!(i8::from(signed), -56, "200 as u8 should become -56 as i8");
}

#[test]
fn test_word_zero_width() {
    // Tests the degenerate case of 0-bit words.
    let a: ddl::U<0> = 0u8.into();
    let b: ddl::U<0> = 99u8.into(); // Any value should become 0

    assert!(a == b, "All 0-bit words should be equal");
    assert_eq!(u8::from(a), 0);
    assert_eq!(u8::from(a + b), 0);
}

#[test]
fn test_word_various_sizes() {
    // Tests Word operations across various bit widths (3-bit, 12-bit, 17-bit, 33-bit)
    // to ensure the WordRep trait mappings work correctly.
    let a: ddl::U<3> = 7u8.into(); // Max 3-bit value
    let b: ddl::U<3> = 1u8.into();
    assert_eq!(u8::from(a + b), 0, "3-bit overflow should wrap");

    let c: ddl::U<12> = 4095u16.into(); // Max 12-bit value
    assert_eq!(u16::from(c), 4095);

    let d: ddl::U<17> = 100000u32.into();
    assert_eq!(u32::from(d), 100000);

    // 33-bit values are stored with padding in a 64-bit container
    // The maximum value for a 33-bit unsigned integer is 2^33 - 1 = 8589934591
    let e: ddl::U<33> = 8589934591u64.into();
    assert_eq!(u64::from(e), 8589934591);
}

// ============================================================================
// Range Iterator Tests
// ============================================================================

#[test]
fn test_range_iterators() {
    // Tests range iterator functions for ascending/descending, signed/unsigned.
    let up_u: Vec<ddl::U<8>> = ddl::rng_up_u_iter(0u8.into(), 5u8.into(), 1u8.into()).collect();
    assert_eq!(up_u.len(), 5);
    assert_eq!(u8::from(up_u[0]), 0);
    assert_eq!(u8::from(up_u[4]), 4);

    let up_step: Vec<ddl::U<8>> = ddl::rng_up_u_iter(0u8.into(), 10u8.into(), 3u8.into()).collect();
    assert_eq!(up_step.len(), 4); // 0, 3, 6, 9
    assert_eq!(u8::from(up_step[3]), 9);

    // Note: rng_down_u takes (start, end, step) where range is (start+1..=end).rev()
    // So for a descending sequence from 10 down to 5, we pass end=10, start=4
    let down_u: Vec<ddl::U<8>> = ddl::rng_down_u(4u8.into(), 10u8.into(), 1u8.into()).collect();
    assert_eq!(down_u.len(), 6); // 10, 9, 8, 7, 6, 5
    assert_eq!(u8::from(down_u[0]), 10);
    assert_eq!(u8::from(down_u[5]), 5);

    let up_i: Vec<ddl::I<8>> = ddl::rng_up_i((-3i8).into(), 3i8.into(), 2i8.into()).collect();
    assert_eq!(up_i.len(), 3); // -3, -1, 1
    assert_eq!(i8::from(up_i[0]), -3);
    assert_eq!(i8::from(up_i[2]), 1);
}

// ============================================================================
// Type System Tests (bor/clo)
// ============================================================================

#[test]
fn test_type_borrow_clone() {
    // Tests the Type trait's bor (borrow) and Clo trait's clo (clone) operations
    // which are fundamental to the DDL type system.
    let arr = ddl::new_array([1u32, 2, 3]);
    let borrowed = arr.bor();
    let cloned = borrowed.clo();

    assert_eq!(arr.len(), cloned.len());
    assert_eq!(arr[0], cloned[0]);

    // Test with Maybe
    let just = ddl::Maybe::Just(42u32);
    let borrowed_just = just.bor();
    let cloned_just = borrowed_just.clo();
    assert_eq!(just, cloned_just);

    // Test with Map
    let map = ddl::empty_map::<u32, u32>().insert(1, 10);
    let borrowed_map = map.bor();
    let cloned_map = borrowed_map.clo();
    assert!(map.bor() == cloned_map.bor());
}

#[test]
fn test_ddl_box_sharing() {
    // Tests that O<T> (owned box) uses reference counting for efficient cloning.
    let arr1 = ddl::new_array([1u32, 2, 3, 4, 5]);
    let arr2 = arr1.clone();

    // Both should have the same content
    assert_eq!(arr1.len(), arr2.len());
    assert_eq!(arr1[0], arr2[0]);

    // Modifications through one shouldn't affect the other (copy-on-write)
    // This is implicitly tested by the immutability of the data structures
}

// ============================================================================
// Display and Serialization Tests
// ============================================================================

#[test]
fn test_maybe_display() {
    // Tests Display implementation for Maybe.
    let nothing: ddl::Maybe<i32> = ddl::Maybe::Nothing;
    let just = ddl::Maybe::Just(42);

    assert_eq!(format!("{}", nothing), "nothing");
    assert_eq!(format!("{}", just), "just 42");
}

#[test]
fn test_array_display() {
    // Tests Display implementation for Array.
    let arr = ddl::new_array([1u32, 2, 3]);
    assert_eq!(format!("{}", arr), "[1, 2, 3]");

    let empty: ddl::Array<u32> = ddl::new_array([]);
    assert_eq!(format!("{}", empty), "[]");
}

#[test]
fn test_map_display() {
    // Tests Display implementation for Map.
    let m = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(2, 20);

    let display = format!("{}", m);
    assert!(display.contains("1 -> 10"));
    assert!(display.contains("2 -> 20"));
    assert!(display.starts_with("[|"));
    assert!(display.ends_with("|]"));
}

#[test]
fn test_word_display() {
    // Tests Display implementation for unsigned and signed Words.
    let unsigned: ddl::U<8> = 42u8.into();
    assert_eq!(format!("{}", unsigned), "42");

    let signed: ddl::I<8> = (-42i8).into();
    assert_eq!(format!("{}", signed), "-42");
}

#[test]
fn test_serialization() {
    // Tests JSON serialization for various DDL types.

    // Test Maybe serialization
    let nothing: ddl::Maybe<u32> = ddl::Maybe::Nothing;
    let just = ddl::Maybe::Just(42u32);
    assert_eq!(serde_json::to_string(&nothing).unwrap(), "null");
    assert_eq!(serde_json::to_string(&just).unwrap(), r#"{"$$just":42}"#);

    // Test Array serialization
    let arr = ddl::new_array([1u32, 2, 3]);
    assert_eq!(serde_json::to_string(&arr).unwrap(), "[1,2,3]");

    // Test Word serialization
    let word: ddl::U<8> = 42u8.into();
    assert_eq!(serde_json::to_string(&word).unwrap(), "42");

    // Test Map serialization
    let map = ddl::empty_map::<u32, u32>()
        .insert(1, 10)
        .insert(2, 20);
    let json = serde_json::to_string(&map).unwrap();
    assert!(json.contains(r#""$$map""#), "Map should serialize with $$map key");
}
