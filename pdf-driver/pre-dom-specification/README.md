The top level literate Haskell file is in pre-dom.tex.
Other utilities and such are in etc/*.hs

Is quite portable.
Don't think any extra packages are needed.
This should work to explore things:

```
$ ghci -x lhs -ietc pre-dom.tex
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/tullsen/.ghc/x86_64-darwin-8.8.4/environments/default
[1 of 5] Compiling Utils            ( etc/Utils.hs, interpreted )
[2 of 5] Compiling Types            ( etc/Types.hs, interpreted )
[3 of 5] Compiling Primitives       ( etc/Primitives.hs, interpreted )
[4 of 5] Compiling Streams          ( etc/Streams.hs, interpreted )
[5 of 5] Compiling Spec             ( pre-dom.tex, interpreted )
Ok, five modules loaded.
*Spec>
```
