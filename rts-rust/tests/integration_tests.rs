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

#[test]
fn test_array_comparison() {
    // Tests array comparison operations (Eq, Ord) which compare arrays lexicographically.
    // Using byte arrays (ArrayB<U<8>>) to test the cmp operator.
    let arr1 = ddl::new_byte_array(&[1, 2, 3]);
    let arr2 = ddl::new_byte_array(&[1, 2, 3]);
    let arr3 = ddl::new_byte_array(&[1, 2, 4]); // Different last element
    let arr4 = ddl::new_byte_array(&[1, 2]);    // Shorter array
    let arr5 = ddl::new_byte_array(&[1, 2, 3, 4]); // Longer array
    let arr6 = ddl::new_byte_array(&[2, 1, 0]); // Different first element

    // Test equality
    assert!(arr1.bor() == arr2.bor(), "Arrays with same elements should be equal");
    assert!(arr1.bor() != arr3.bor(), "Arrays with different elements should not be equal");
    assert!(arr1.bor() != arr4.bor(), "Arrays with different lengths should not be equal");

    // Test ordering - lexicographic comparison
    assert!(arr1.bor() < arr3.bor(), "arr1 < arr3 (differs at last element: 3 < 4)");
    assert!(arr3.bor() > arr1.bor(), "arr3 > arr1 (differs at last element: 4 > 3)");

    assert!(arr4.bor() < arr1.bor(), "arr4 < arr1 (prefix is less than longer array)");
    assert!(arr1.bor() > arr4.bor(), "arr1 > arr4 (longer array with same prefix)");

    assert!(arr1.bor() < arr5.bor(), "arr1 < arr5 (prefix is less than longer array)");

    assert!(arr1.bor() < arr6.bor(), "arr1 < arr6 (differs at first element: 1 < 2)");
    assert!(arr6.bor() > arr1.bor(), "arr6 > arr1 (differs at first element: 2 > 1)");

    // Test with empty arrays
    let empty = ddl::new_byte_array(&[]);
    assert!(empty.bor() < arr1.bor(), "Empty array should be less than non-empty array");
    assert!(empty.bor() == ddl::new_byte_array(&[]).bor(), "Empty arrays should be equal");

    // Test <= and >= operators
    assert!(arr1.bor() <= arr2.bor(), "Equal arrays should satisfy <=");
    assert!(arr1.bor() >= arr2.bor(), "Equal arrays should satisfy >=");
    assert!(arr4.bor() <= arr1.bor(), "arr4 <= arr1");
    assert!(arr1.bor() >= arr4.bor(), "arr1 >= arr4");
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
    // pushing to divergent copies produces correctly ordered results.
    let b0 = ddl::new_builder().push(1u32).push(2);
    let b1 = b0.clone().push(3);
    let b2 = b0.clone().push(4);

    let arr1 = b1.build();
    let arr2 = b2.build();

    assert_eq!(ddl::array_to_vec(arr1), vec![1, 2, 3]);
    assert_eq!(ddl::array_to_vec(arr2), vec![1, 2, 4]);
}

#[test]
fn test_builder_shared_then_extended() {
    // Regression test: a shared builder extended with multiple elements
    // must preserve the original order (shared prefix followed by new elements).
    let base = ddl::new_builder().push(10u32).push(20).push(30);
    let extended = base.clone().push(40).push(50);

    let base_arr = base.build();
    let ext_arr = extended.build();

    assert_eq!(ddl::array_to_vec(base_arr), vec![10, 20, 30]);
    assert_eq!(ddl::array_to_vec(ext_arr), vec![10, 20, 30, 40, 50]);
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
    let up_u: Vec<ddl::U<8>> = ddl::rng_up_u(0u8.into(), 5u8.into(), 1u8.into()).collect();
    assert_eq!(up_u.len(), 5);
    assert_eq!(u8::from(up_u[0]), 0);
    assert_eq!(u8::from(up_u[4]), 4);

    let up_step: Vec<ddl::U<8>> = ddl::rng_up_u(0u8.into(), 10u8.into(), 3u8.into()).collect();
    assert_eq!(up_step.len(), 4); // 0, 3, 6, 9
    assert_eq!(u8::from(up_step[3]), 9);

    let down_u: Vec<ddl::U<8>> = ddl::rng_down_u(10u8.into(), 5u8.into(), 1u8.into()).collect();
    assert_eq!(down_u.len(), 5); // 10, 9, 8, 7, 6
    assert_eq!(u8::from(down_u[0]), 10);
    assert_eq!(u8::from(down_u[4]), 6);

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

// ============================================================================
// Input Tests
// ============================================================================

#[test]
fn test_input_creation() {
    // Tests creating Input from byte arrays and strings.
    let name = ddl::new_byte_array(b"test.dat");
    let bytes = ddl::new_byte_array(b"hello world");
    let input = ddl::new_input(name, bytes);

    assert_eq!(input.len(), 11);
    assert_eq!(input.offset(), 0);
    assert!(!input.is_empty());

    // Test string convenience constructor
    let input2 = ddl::new_input_str("file.txt", "test data");
    assert_eq!(input2.len(), 9);
    assert_eq!(input2.offset(), 0);
}

#[test]
fn test_input_empty() {
    // Tests behavior with empty input.
    let input = ddl::new_input_str("empty", "");
    assert_eq!(input.len(), 0);
    assert!(input.is_empty());
    assert_eq!(input.offset(), 0);
}

#[test]
fn test_input_advance() {
    // Tests advancing through input by various amounts.
    let input = ddl::new_input_str("test", "abcdefgh");
    assert_eq!(input.len(), 8);
    assert_eq!(input.offset(), 0);

    // Advance by 3 bytes
    let input2 = input.advance(3);
    assert_eq!(input2.len(), 5);
    assert_eq!(input2.offset(), 3);

    // Advance by more bytes
    let input3 = input2.advance(2);
    assert_eq!(input3.len(), 3);
    assert_eq!(input3.offset(), 5);

    // Advance beyond end (should clamp to end)
    let input4 = input3.advance(100);
    assert_eq!(input4.len(), 0);
    assert!(input4.is_empty());
    assert_eq!(input4.offset(), 8);
}

#[test]
fn test_input_advance_maybe() {
    // Tests conditional advancing that fails if not enough bytes available.
    let input = ddl::new_input_str("test", "abcdefgh");

    // Should succeed with enough bytes
    let result = input.clone().advance_maybe(3);
    assert!(result.is_just());
    let input2 = result.unwrap();
    assert_eq!(input2.len(), 5);
    assert_eq!(input2.offset(), 3);

    // Should succeed advancing to exactly the end
    let result = input.clone().advance_maybe(8);
    assert!(result.is_just());
    let input3 = result.unwrap();
    assert_eq!(input3.len(), 0);
    assert!(input3.is_empty());

    // Should fail when not enough bytes
    let result = input.clone().advance_maybe(9);
    assert!(result.is_nothing());

    // Should fail advancing from middle when not enough remaining
    let input4 = input.advance(5);
    let result = input4.advance_maybe(4);
    assert!(result.is_nothing());
}

#[test]
fn test_input_restrict() {
    // Tests restricting input to a limited number of bytes.
    let input = ddl::new_input_str("test", "0123456789");
    assert_eq!(input.len(), 10);

    // Restrict to 5 bytes
    let restricted = input.clone().restrict(5);
    assert_eq!(restricted.len(), 5);
    assert_eq!(restricted.offset(), 0);

    // Advance and then restrict
    let input2 = input.clone().advance(3);
    let restricted2 = input2.restrict(4);
    assert_eq!(restricted2.len(), 4);
    assert_eq!(restricted2.offset(), 3);

    // Restrict beyond available bytes (should clamp)
    let restricted3 = input.clone().restrict(100);
    assert_eq!(restricted3.len(), 10);

    // After restriction, advancing beyond restriction should stop at restriction
    let restricted4 = input.clone().restrict(5);
    let advanced = restricted4.advance(10);
    assert_eq!(advanced.len(), 0);
    assert_eq!(advanced.offset(), 5);
}

#[test]
fn test_input_bytes() {
    // Tests getting the bytes from input at various offsets.
    let input = ddl::new_input_str("test", "hello world");

    // Get all bytes from start
    let bytes = input.bytes();
    assert_eq!(bytes.len(), 11);

    // Advance and get remaining bytes
    let input2 = input.advance(6);
    let bytes2 = input2.bytes();
    assert_eq!(bytes2.len(), 5);

    // Convert to vec to check actual content
    let vec: Vec<u8> = bytes2.iter().map(|x| u8::from(*x)).collect();
    assert_eq!(vec, b"world");
}

#[test]
fn test_input_head() {
    // Tests getting the first byte of input.
    let input = ddl::new_input_str("test", "abc");

    // Head of initial input
    let h = input.head();
    assert_eq!(u8::from(h), b'a');

    // Head after advancing should be the byte at current position
    let input2 = input.advance(1);
    let h2 = input2.head();
    assert_eq!(u8::from(h2), b'b');

    let input3 = input2.advance(1);
    let h3 = input3.head();
    assert_eq!(u8::from(h3), b'c');
}

#[test]
fn test_input_is_prefix() {
    // Tests checking if input starts with a given byte sequence.
    let input = ddl::new_input_str("test", "hello world");

    // Check for matching prefix
    let prefix1_arr = ddl::new_byte_array(b"hello");
    let prefix1 = prefix1_arr.bor();
    assert!(input.is_prefix(prefix1), "Should match 'hello' prefix");

    // Check for non-matching prefix
    let prefix2_arr = ddl::new_byte_array(b"world");
    let prefix2 = prefix2_arr.bor();
    assert!(!input.is_prefix(prefix2), "Should not match 'world' at start");

    // Check after advancing
    let input2 = input.clone().advance(6);
    let prefix3_arr = ddl::new_byte_array(b"world");
    let prefix3 = prefix3_arr.bor();
    assert!(input2.is_prefix(prefix3), "Should match 'world' after advancing");

    // Check prefix longer than remaining input
    let input3 = input.clone().advance(9);
    let prefix4_arr = ddl::new_byte_array(b"longer");
    let prefix4 = prefix4_arr.bor();
    assert!(!input3.is_prefix(prefix4), "Should not match prefix longer than input");

    // Check empty prefix
    let empty_prefix_arr = ddl::new_byte_array(b"");
    let empty_prefix = empty_prefix_arr.bor();
    assert!(input.is_prefix(empty_prefix), "Empty prefix should always match");
}

#[test]
fn test_input_name() {
    // Tests getting the name of the input.
    let input = ddl::new_input_str("myfile.txt", "data");
    let name = input.name();

    let name_bytes: Vec<u8> = name.iter().map(|x| u8::from(*x)).collect();
    assert_eq!(name_bytes, b"myfile.txt");

    // Name should remain unchanged after operations
    let input2 = input.advance(2);
    let name2 = input2.name();
    let name2_bytes: Vec<u8> = name2.iter().map(|x| u8::from(*x)).collect();
    assert_eq!(name2_bytes, b"myfile.txt");
}

#[test]
fn test_input_combined_operations() {
    // Tests combining multiple input operations.
    let input = ddl::new_input_str("combined", "0123456789ABCDEF");

    // Restrict to first 10 bytes, then advance
    let restricted = input.clone().restrict(10);
    assert_eq!(restricted.len(), 10);

    let advanced = restricted.advance(5);
    assert_eq!(advanced.len(), 5);
    assert_eq!(advanced.offset(), 5);

    // Check we can't advance beyond restriction
    let at_end = advanced.advance(100);
    assert_eq!(at_end.len(), 0);
    assert_eq!(at_end.offset(), 10);

    // Original input should be unaffected (immutability)
    assert_eq!(input.len(), 16);
    assert_eq!(input.offset(), 0);
}

#[test]
fn test_input_display_debug() {
    // Test Display and Debug formatting
    let input = ddl::new_input_str("test.txt", "hello world");
    let display_str = format!("{}", input);
    let debug_str = format!("{:?}", input);
    
    // Both should match: Input("test.txt:0x0--0xb")
    assert!(display_str.starts_with("Input(\"test.txt:0x0--0x"));
    assert!(debug_str.starts_with("Input(\"test.txt:0x0--0x"));
    
    // Test after advancing
    let input2 = input.clone().advance(5);
    let display_str2 = format!("{}", input2);
    assert!(display_str2.contains("0x5--0x"));
    
    // Test after restricting
    let input3 = input.restrict(5);
    let display_str3 = format!("{}", input3);
    assert!(display_str3.contains("0x0--0x5"));
}

#[test]
fn test_input_serialize() {
    // Test serialization format matches C++ toJS
    let input = ddl::new_input_str("test.txt", "hello world");
    let json = serde_json::to_string(&input).unwrap();
    
    // Should be: {"$$input":"test.txt:0x0--0xb"}
    assert!(json.contains("$$input"));
    assert!(json.contains("test.txt:0x0--0x"));
}

#[test]
fn test_input_format_matches_cpp() {
    // Verify exact format matches C++ implementation
    let input = ddl::new_input_str("file.txt", "abcdefgh");
    
    // Display/Debug should show: Input("file.txt:0x0--0x8")
    let display = format!("{}", input);
    let debug = format!("{:?}", input);
    
    println!("Display: {}", display);
    println!("Debug:   {}", debug);
    
    assert!(display.contains("Input(\"file.txt:0x0--0x8\")"));
    assert!(debug.contains("Input(\"file.txt:0x0--0x8\")"));
    
    // Serialize should produce: {"$$input":"file.txt:0x0--0x8"}
    let json = serde_json::to_string(&input).unwrap();
    println!("JSON:    {}", json);
    
    assert!(json.contains("$$input"));
    assert!(json.contains("file.txt:0x0--0x8"));
}
