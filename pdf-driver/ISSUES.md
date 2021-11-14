# pdf-hs-driver

TODO

# pdf-dom
## Known Issues

- PDF conformance:
  - pdf-dom and pdf-hs-driver are based on the PDF specification in
    pdf-cos/spec/*.ddl, which has issues
    - the parser is very strict (much more than most tools) in PDF validation,
      rejecting many files that most/all tools allow.
    - handling of encryption issues, e.g., govdocs/000029.pdf is rejected.
    - etc.
    
## Unsupported Parts of PDF

1. pdf-dom currently ignores linearization data/hints, so all the linearization
   data will be treated as a cavity.

2. Dom-Dependent objects (PDF object definitions that require a lookup in the
   DOM to be able to parse them).  E.g. this output line
   ```
   object ( R {refObj = 5, refGen = 0} ) ... is a Dom-Dependent parser [unsupported]
   ```
   indicates that object 5 0 is an object stream which uses an indirect object
   to indicate how to parse/validate itself.  Handling such cases properly while
   being cognizant of incremental updates has not been implemented yet.
   (Dom-dependent objects **are** handled in pdf-hs-driver where incremental 
   updates are ignored.)

3. Hybrid files: we have less than full support for.

## Feature Requests / To Do Items

1. Could you also report object number ranges for each incremental update summary?
   (PW)

2. Can you report the "%%EOF" and "startxref" positions, startxref value and
   overall syntactic validity of the overall file structure? (e.g. are there
   bytes after %%EOF? Before %PDF-x.y? since this affects the byte
   offsets). This also allows checking that you got the correct startxref in
   case there were multiple near the end of the file. (PW)

3. Can you report the "xref subsection lines" (i.e. the line with 2 integers
   separated by a single space)? Note that a single xref can have multiple such
   subsections ... and that "0 0" is invalid/useless. (PW)

4. In the "combined xref", are you able to also state from which incremental
   update (or original file) this object definition is coming from? (PW)

5. Check consistency of the traditional and stream xrefs in hybrid files.

6. Add support for linearization and check consistency of linearized data/hints.

7. Improve display and reporting of cavities
    - show content of cavities
    - filter cavities in various forms
        - ignore whitespace
        - ignore small cavities (below a threshold?)

8. Expand & categorize notion of cavities 
    - Cavities can be
        - at end of lines 1 and 2
        - between body objects
        - between updates
    - We might categorize and report these differently.
   
9. Replace the 'heuristic' that incremental updates occur sequentially in the
   file (which allows us to guess as to the "body start") with something more
   general.
   
10. Inspect DOM and check xref/DOM consistency: LOTS HERE, TODO.


