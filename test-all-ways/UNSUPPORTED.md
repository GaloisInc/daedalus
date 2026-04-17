# Marking Tests as Unsupported for Specific Backends

## Overview

Some tests exercise functionality that is not supported by all backends. Instead of having these tests fail, you can now mark specific backends as unsupported for a given test.

## Usage

To mark backends as unsupported for a test, create a `.unsupported` file with the same base name as your test file.

### Example

If you have a test file `tests/MyTest.ddl`, create a file `tests/MyTest.unsupported` containing the names of backends that don't support this test:

```
CompileRust
CompileCPP
```

Each backend name should be on its own line. Valid backend names are:
- `InterpDaedalus`
- `InterpCore`
- `InterpVM`
- `CompileHaskell`
- `CompileCPP`
- `CompileRust`

## Behavior

When a test has unsupported backends:
1. The test will only be compiled and run on the supported backends
2. Validation will only compare outputs from supported backends
3. The test output will show which backends are being skipped:
   ```
   --- MyTest.ddl ------------------------------------------
       Skipping backends: CompileRust CompileCPP
   ```
4. The test will pass as long as all supported backends agree on the output

## Notes

- Lines with only whitespace are ignored
- Backend names are case-sensitive
- Invalid backend names are silently ignored
- If all backends are marked as unsupported, the test will still run but have nothing to validate
