# Dex Regression Test Suite

This directory contains regression tests for the Dex code generator.

## Running Tests

### Run all tests
```bash
./run-tests.sh
```

### Run specific tests
Pass test directory names as arguments:
```bash
./run-tests.sh test_name1 test_name2
```

### Build tests without running them
```bash
make                  # Build all tests
make test_name        # Build a specific test
```

### Clean build artifacts
```bash
make clean
```

## Test Structure

Each test is a subdirectory containing:
- `spec.ddl` - Daedalus parser specification
- `spec.dex` - Dex exporter specification
- `driver.cpp` - C++ test driver that uses the generated parser and exporter
- `expected.txt` - Expected output from running the driver

Tests are auto-discovered: any directory with a `spec.ddl` file will be recognized as a test.

## Adding a New Test

1. Create a new directory with your test name:
   ```bash
   mkdir my-new-test
   cd my-new-test
   ```

2. Create the required files:
   - `spec.ddl` - Define your parser grammar with a `Main` entry point
   - `spec.dex` - Define exporter functions (imports from `spec` and defines `exportMain`)
   - `driver.cpp` - Write a test driver (see existing tests for examples)
   - `expected.txt` - Run your test and capture the expected output

3. Test it:
   ```bash
   cd ..
   make my-new-test
   ./run-tests.sh my-new-test
   ```

## Build System

The build process involves three steps:
1. **Daedalus** compiles `spec.ddl` → `main_parser.cpp/h`
2. **Dex** compiles `spec.dex` → `exporter.cpp/h`
3. **C++** compiles the generated code + `driver.cpp` → executable

Tests are built with address sanitizer (`-fsanitize=address,undefined`) to catch memory errors. Memory leak detection is automatically disabled on ARM Macs where it's not currently supported.

## Dependencies

- Daedalus (parser generator)
- Dex (exporter generator)
- C++ compiler with C++20 support
- GMP library (automatically located via pkg-config)
