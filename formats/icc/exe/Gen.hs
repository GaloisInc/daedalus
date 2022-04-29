module Main(main) where

import Bin

main :: IO ()
main = writeExample "out.icc" (calcExp 64)


