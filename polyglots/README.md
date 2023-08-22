# Polyglot Detection and Generation

Talos is a tool for generating well-formed documents from a Daedalus data
format specification.  Polyglots are documents that are well-formed with
respect to more than one data format specification; that is, they can be parsed
as more than one format.

Given a pair of Daedalus formats, we have developed a two-step methodology for
detecting whether their intersection admits polyglots and, if so, generating
examples.

## Detection

[These
notes](https://docs.google.com/document/d/1Zcrn7_AH1EGbziyQcdNAaYIVljaw5EchNcI1Gp8iUHo/edit#heading=h.an7ziriw2ubc)
define an operational semantics for Daedalus parsers, identify characteristics
of data format cavities, and show how to use cavity characteristics to compose
polyglots.

TODO: Fill details of the detection algorithm here and how to run it.

## Generation

Once we have cavity characteristics for our data formats, the next step is to
create a Daedalus specification that represents the polyglot.  We call such a
file the polyglot skeleton, as it typically has holes to fill in with arbitrary
content.  For example, the `stack` polyglot skeleton is a Daedalus file that
concatenates two data formats, such as a bitmap followed by a PDF.

Skeleton holes can be filled with specific content, e.g. a PDF one would like
to insert into a polyglot, by writing a Daedalus specification that matches
exactly the bytes of the desired document.  Alternatively, one can specify part
or all of the data format parser.

The next step is to use Talos to generate the polyglot.  If the polyglot
skeleton has been filled in with specific content, Talos will generate a
polyglot file that stitches together that content.  Otherwise, Talos will
generate a polyglot containing arbitrary but well-formed examples of each
format.

## Evaluation

We have used this methodology to partially reproduce the results of
[Mitra](https://github.com/corkami/mitra); see below for progress and
limitations.  We have also applied this methodology to data formats beyond
those in Mitra; those results are also presented below.

### Reproducing Mitra Results
```
                          Delayed                 Magic at offset zero,                  No appended
      Any offset  Cavities start                 tolerated appended data                     data    Footer

        Z 7 A R   P I D T   P M   A B B C C E E F F G G I I I I J J N O P L P P R R T W   B J P P W   I X
        i Z r A   D S C A   S P   R M Z A P B L L l I Z C C D L P P E G S N E N I T I A   P a C C A   D Z
        p   j R   F O M R     4     P 2 B I M F V a F   C O 3 D 2 G S G D K   G F F F D   G v A A S   3
                                          O L     c         v A                 F   F       a P P M   v
                                                            2                                   N     1

Zip     . X X X   X X X X   X X   X X X X X   X X X X X   X     X X   X X   X X   X X X     X             30
7Z      
Arj     
RAR     

PDF     X X X X   . X X X   X X   X X X X X   X X X X X   X     X X   X X   X X   X X X     X             30
ISO     
DCM    
TAR   

PS      
MP4     

AR      
BMP     
BZ2     
CAB     
CPIO    
EBML    
ELF     
FLV     
Flac    
GIF     
GZ      
ICC     
ICO     
ID3v2   
ILDA    
JP2     
JPG     
NES     
OGG     
PSD     
LNK     
PE      
PNG     
RIFF    
RTF     
TIFF    
WAD     

BPG     
Java    
PCAP    
PCAPN   
WASM    

ID3v1
XZ
```

### Formats Beyond Mitra

TODO: Add cavity characteristics/polyglots for other data formats we've defined.
