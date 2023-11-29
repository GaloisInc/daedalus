# Conditional Cavity-based Polyglots

Conditional cavity polyglots embed a location-independent document into a
cavity within another file, often with some restrictions (conditions) on the
content.  A common example is embedding content into a multi-line comment, so
long as the content does not contain the comment-termination sequence.

As an example, a Java file can contain PDF data in a multi-line comment, so
long as the PDF data can be interpreted as UTF-8 and doesn't contain the
comment terminator `*/`.

```
/*
< PDF content >
*/

class Example {
  public static void main(String[] args) {
      System.out.println("Hello, world!");
  }    
}
```

[conditional-cavity-shape.ddl](conditional-cavity-shape.ddl) is a Daedalus
specification describing a conditional cavity-based polyglot skeleton.

## Structure of this Directory

This directory contains Daedalus files in two forms:

1. `FMT1-FMT2.ddl` is a polyglot skeleton combining two specific formats.  It references `FMT1.ddl` and `FMT2.ddl`.
2. `FMT.ddl` defines characteristics of a format required for Talos to generate conditional cavity-based polyglots.

## How to Generate a Polyglot

Simply run
```bash
talos -n 1 FMT1-FMT2.ddl > output.fmt
```
