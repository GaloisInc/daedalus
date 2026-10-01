# PDF text-layout extraction plan

## Goal

Extend text extraction so that it produces text chunks with practical,
font-metric-based geometry. The geometry should describe the area occupied by
the text advance in page user space; it is not intended to be an exact bound
of the painted glyph outlines.

The first implementation will produce axis-aligned page-space bounding boxes,
including for rotated or skewed text by transforming all four corners of the
text-space rectangle.

Keep extracted chunks in content-stream order. Reconstructing visual reading
order may use the geometry later, but is not part of the initial extractor.

## Implementation architecture

Daedalus should recognize operators, project the operands needed for
extraction, and decode PDF-specific values. Rust should own the mutable
graphics, text, font-cache, and output state. Daedalus will apply completed
operator actions through native calls.

Native state changes should occur only after all Daedalus operations that may
fail for that operator have completed. This avoids needing to undo Rust state
if operand processing backtracks or fails.

- [x] Define the initial Rust-owned matrix, graphics, and extraction state.
- [x] Add native operations for the extraction effects currently implemented.
- [x] Move existing operator effects from the Daedalus-threaded state to
      native calls.
- [x] Move UTF-16 output accumulation into the Rust extraction state.
- [x] Remove the temporary Daedalus layout-state representation.
- [ ] Add native operations for new graphics and text-layout operators as
      they are implemented.

### Current implementation baseline

- Rust owns the selected font, reference-keyed font cache, graphics state,
  graphics-state stack, text matrices, and UTF-16 output.
- Daedalus owns only traversal-local values such as the current instruction
  index, operand count, and text-object flag used to gate operators.
- Native operations currently implement page reset, `q`, `Q`, `cm`, `BT`,
  `Tf`, `Tc`, `Tw`, `Tz`, `TL`, `Tr`, `Ts`, `Tm`, font loading and selection,
  numeric `TJ` positioning adjustments, and UTF-16 output.
- `Tj`, string elements of `TJ`, `'`, and `"` currently decode into the
  plain-text output, without positioning or chunk geometry.
- `Td`, `TD`, `T*`, `'`, and `"` update the text matrices. `TD` also updates
  leading, and `"` applies its word- and character-spacing operands.
- Text-positioning operators no longer emit synthetic newline characters.
- No font metrics, glyph advances, bounding boxes, or structured chunks are
  implemented yet.

## Proposed output

Each extracted chunk should contain:

- Decoded Unicode text, accumulated internally as UTF-16 code units.
- The page number.
- An optional axis-aligned bounding box in page user space.
- Optionally, enough baseline information to support later line and word
  reconstruction.

Initially, emit one chunk for each string operand of a text-showing operator:

- One chunk for `Tj`.
- One chunk for each string element in a `TJ` array.
- One chunk for the string shown by `'` or `"`.

Adjacent chunks may be merged in a later layout pass.

## 1. Make content operators operand-aware

- [x] Track the number of operands since the preceding
      operator. Reuse the values already stored in the flat content-stream
      entry array rather than building another operand list.
- [x] Check the complete operand count before projecting individual values
      with `GetOperand`, so missing operands cannot cause index underflow.
- [x] Check operand counts for the currently interpreted `Tf`, `Td`, `TD`,
      `T*`, `Tj`, `TJ`, `'`, and `"` operators.
- [x] Support the operands needed for `q`, `Q`, `cm`, `Tc`, `Tw`, `Tz`,
      `TL`, `Tr`, `Ts`, and `Tm`.
- [x] Skip malformed supported operator invocations without losing later
      valid text, and record in Rust state that the result may be unreliable.
      For `TJ` arrays, process valid string and numeric elements individually
      and count invalid elements rather than discarding the entire array.

The operand count makes the remaining `GetOperand (i - n)` projections safe
from subtraction underflow.

As each operator is implemented, project and type-check the operands needed
for extraction or layout at the point where they are used. Operands that the
extractor otherwise ignores do not need separate validation.

## 2. Define geometry and matrix types

- [ ] Define points, rectangles, and axis-aligned bounding boxes.
- [x] Define affine matrix composition.
- [ ] Define point transformation operations.
- [x] Establish the matrix representation and multiplication order used by
      the implementation.
- [ ] Add operations for:
  - Transforming a point.
  - Transforming all four corners of an axis-aligned rectangle and enclosing
    the results in a page-space axis-aligned bounding box.
  - Computing the union of axis-aligned bounding boxes.
- [ ] Decide whether output coordinates use raw page user space or are
      normalized for page rotation and page boxes.

## 3. Track graphics state

- [x] Add the current transformation matrix, `CTM`, to extraction state.
- [x] Implement `cm` by concatenating its matrix with the current `CTM`.
- [x] Implement the `q` and `Q` graphics-state stack.
- [x] Reset page-local graphics state at each page boundary.
- [ ] Preserve the appropriate graphics state when processing Form XObjects
      in the future.

Only the state needed for text geometry has to be represented initially.

## 4. Track text-object matrices

- [x] Track whether extraction is currently inside a `BT`/`ET` text object.
- [x] Interpret the currently supported text-positioning and text-showing
      operators only inside a text object.
- [x] Add the text matrix, `Tm`, and text-line matrix, `Tlm`, to extraction
      state.
- [x] On `BT`, initialize `Tm` and `Tlm` to the identity matrix.
- [x] On `ET`, make the text-object-local matrices inactive. Their stored
      values need not be cleared because they are reinitialized by `BT`.
- [x] Implement `Tm`.
- [x] Implement `Td` and update both `Tm` and `Tlm`.
- [x] Implement `TD`, including its effect on text leading.
- [x] Implement `T*` using the current text leading.
- [x] Implement the positioning behavior implied by `'` and `"`.
- [x] Track whether `Tm` and `Tlm` remain reliable, and avoid reporting
      geometry after an unknown advance until positioning is re-established.

Synthetic newline emission for text-positioning operators has already been
removed. Logical lines will be inferred later from chunk geometry.

## 5. Track text state

- [x] Track the selected font from `Tf`.
- [x] Track and validate the font size from `Tf`.
- [x] Track character spacing from `Tc`.
- [x] Track word spacing from `Tw`.
- [x] Track horizontal scaling from `Tz`.
- [x] Track text leading from `TL`.
- [x] Track text rise from `Ts`.
- [x] Track text rendering mode from `Tr`, even if the first implementation
      does not use it to filter invisible text.
- [x] Preserve text-state parameters across text objects as required.
- [x] Separate page-local layout state from reference-keyed font and CMap
      caches.

## 6. Parse simple-font metrics

- [x] Extend parsed simple fonts with `FirstChar` and `Widths`.
- [x] Parse `MissingWidth` from the font descriptor.
- [x] Define deliberate fallback behavior when a width is absent: use
      `MissingWidth`, and report dimensions as unavailable when neither width
      source exists.
- [ ] Handle the standard 14 fonts when their metrics are not embedded in
      the PDF.
- [ ] Parse the Type3 `FontMatrix`.
- [x] Parse `Ascent` and `Descent` from the font descriptor.
- [x] Record `FontBBox` where useful, while keeping advance-based geometry as
      the initial model.

Geometry must be calculated from the original encoded character codes.
Unicode decoding may map one source code to zero, one, or multiple Unicode
code points, but that source code still has one positioning advance.

## 7. Calculate glyph advances

- [ ] For each source character code, obtain its width from the selected
      font.
- [ ] Convert glyph-space width using the font matrix or the conventional
      simple-font scale.
- [ ] Apply font size.
- [ ] Apply character spacing.
- [ ] Apply word spacing where the encoded character code denotes a space.
- [ ] Apply horizontal scaling.
- [ ] Advance `Tm` after each shown character.
- [ ] Keep decoding and positioning synchronized when a variable-width CMap
      consumes source codes.

## 8. Interpret text-showing operators

- [ ] Implement `Tj` using the shared decode-and-position operation.
- [ ] Implement string elements of `TJ`.
- [x] Apply numeric `TJ` adjustments to `Tm`.
- [ ] Use sufficiently large `TJ` positioning gaps to infer likely word
      boundaries in the plain-text output.
- [x] Implement `'` as `T*` followed by text showing.
- [x] Implement `"` with its word-spacing and character-spacing updates,
      followed by line movement and text showing.
- [ ] Ensure positioning still occurs when a source code has no Unicode
      mapping.
- [ ] Decide whether text rendering modes that do not paint glyphs should
      produce chunks.

## 9. Build chunk geometry

- [ ] Choose the practical vertical extent for simple-font chunks. Candidate
      sources include font ascent/descent, `FontBBox`, and conservative
      defaults.
- [ ] Construct each glyph or run rectangle in text space.
- [ ] Apply text rise.
- [ ] Transform axis-aligned geometry through the text rendering matrix and
      `CTM`.
- [ ] Union glyph geometry into the chunk bounding box.
- [ ] Define how spaces and empty decoded mappings contribute to chunk
      geometry.
- [ ] Retain baseline endpoints if useful for later grouping.

The first implementation should use advance-based horizontal extents and
font-level vertical metrics. It should not parse glyph outlines.

## 10. Add structured extraction results

- [x] Supplement the plain UTF-16 output builder with a chunk
      builder.
- [x] Add Rust state for completed text chunks and an in-progress chunk,
      explicitly distinguishing known, empty, and unknown geometry.
- [x] Convert completed chunk text from UTF-16 to Rust strings.
- [x] Expose a Rust result type containing the page and optional bounding-box
      information.

## 11. Add Type0 and vertical-font support

- [ ] Parse Type0 descendant-font metrics.
- [ ] Implement `DW` and `W` for horizontal CID widths.
- [ ] Implement writing-mode selection from the encoding CMap.
- [ ] Implement `DW2` and `W2` for vertical metrics when vertical writing is
      supported.
- [ ] Account for multi-byte source character codes while measuring text.

This can follow the first useful milestone for simple horizontal fonts.

## 12. Testing

- [ ] Add focused examples for matrix composition and transformed rectangles.
- [ ] Test horizontal, translated, and scaled text.
- [ ] Test four-corner axis-aligned boxes for rotated and skewed text.
- [ ] Test character spacing, word spacing, text rise, and leading.
- [ ] Test positive and negative numeric adjustments in `TJ`.
- [ ] Test multiple text objects with retained text state and reset text
      matrices.
- [ ] Test `q`, `Q`, and `cm` around text objects.
- [ ] Test missing widths and missing Unicode mappings.
- [ ] Compare selected results with a known PDF renderer or extraction tool.
- [ ] Include examples where decoded Unicode length differs from the number
      of positioned source codes.

## Suggested implementation milestones

1. Operand-aware operators and matrix primitives.
2. Graphics state, text matrices, and text-state tracking.
3. Simple-font widths and horizontal advances.
4. Metric-based chunks for `Tj`, `TJ`, `'`, and `"`.
5. Structured Rust output while preserving the plain-text API.
6. Page-coordinate normalization and layout-based grouping.
7. Type0, CID, and vertical-writing metrics.
