#!/bin/sh

# the following assumes graphtool is in PATH
# graphtool is a tool of MarkT's
#  git url:
#    url = ssh://ssh.galois.com/~tullsen/share/tools.git
#
#  to look at the doc/manpage:
#   man tools/graphtools/man/man1/graphtool.1

# create the two dependency DAGs:

grep "^import" *.ddl | \
  sed 's/\.ddl:import / /' | \
  graphtool "edges -source auto-add -destination auto-add  < - | display" > doc-all-import.dag
  
grep "^import" *.ddl | \
  sed 's/\.ddl:import / /' | \
  graphtool "edges -source auto-add -destination auto-add  < - | roots CMap | display" > doc-cmap-import.dag

# create a topological sort at column 37 (or so), with the dependencies of PdfExtractText:

grep "^import" *.ddl | \
  sed 's/\.ddl:import / /' | \
  graphtool "edges -source auto-add -destination auto-add  < - | roots PdfExtractText | display ascii right" > doc-pdfextracttext-import.dag

grep "^import" *.ddl | \
  sed 's/\.ddl:import / /' | \
  graphtool "edges -source auto-add -destination auto-add  < - | roots GlyphList | display ascii right" > doc-glyphlist-import.dag


