
module Main where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Codec.Picture
import System.Environment (getArgs)

type ProvMap = Map Int Int

getProv :: FilePath -> IO ProvMap
getProv f = read <$> readFile f


nProvs :: ProvMap -> Int
nProvs = length . Set.toList . Set.fromList . Map.elems

maxAddr :: ProvMap -> Int
maxAddr = fst . Map.findMax


palette :: [PixelRGB8]
-- palette = [PixelRGB8 105 239 123, PixelRGB8 128 25 103, PixelRGB8 174 227 154, PixelRGB8 209 72 211, PixelRGB8 86 162 33, PixelRGB8 99 20 175, PixelRGB8 92 232 239, PixelRGB8 243 59 93, PixelRGB8 17 94 65, PixelRGB8 239 161 204, PixelRGB8 76 62 118, PixelRGB8 233 215 55, PixelRGB8 82 150 206, PixelRGB8 110 57 13, PixelRGB8 202 211 250, PixelRGB8 142 16 35, PixelRGB8 51 162 126] -- , PixelRGB8 244 125 13, PixelRGB8 130 112 246, PixelRGB8 140 149 109]  

palette = [PixelRGB8 250 169 13 , PixelRGB8 1 129 253 , PixelRGB8 160 195 9 , PixelRGB8 132 85 220 , PixelRGB8 101 212 75 , PixelRGB8 167 64 198 , PixelRGB8 55 192 65 , PixelRGB8 204 94 228 , PixelRGB8 118 187 23 , PixelRGB8 142 118 255 , PixelRGB8 215 193 0 , PixelRGB8 108 51 172 , PixelRGB8 188 184 0 , PixelRGB8 177 127 255 , PixelRGB8 1 144 42 , PixelRGB8 248 94 218 , PixelRGB8 1 178 88 , PixelRGB8 199 35 169 , PixelRGB8 66 130 0 , PixelRGB8 240 73 195 , PixelRGB8 60 223 168 , PixelRGB8 233 44 164 , PixelRGB8 123 218 139 , PixelRGB8 143 23 147 , PixelRGB8 183 209 83 , PixelRGB8 2 103 212 , PixelRGB8 255 166 50 , PixelRGB8 1 82 173 , PixelRGB8 224 197 72 , PixelRGB8 79 68 165 , PixelRGB8 155 214 119 , PixelRGB8 200 0 132 , PixelRGB8 11 223 192 , PixelRGB8 255 46 105 , PixelRGB8 0 149 85 , PixelRGB8 255 120 233 , PixelRGB8 86 112 0 , PixelRGB8 211 133 255 , PixelRGB8 170 132 0 , PixelRGB8 0 163 253 , PixelRGB8 255 105 50 , PixelRGB8 1 181 249 , PixelRGB8 213 55 24 , PixelRGB8 0 123 205 , PixelRGB8 251 77 62 , PixelRGB8 2 173 150 , PixelRGB8 218 0 110 , PixelRGB8 0 125 70 , PixelRGB8 255 118 205 , PixelRGB8 66 90 21 , PixelRGB8 198 158 255 , PixelRGB8 107 106 0 , PixelRGB8 186 166 255 , PixelRGB8 175 107 0 , PixelRGB8 1 92 165 , PixelRGB8 228 45 52 , PixelRGB8 0 147 188 , PixelRGB8 166 23 0 , PixelRGB8 143 198 255 , PixelRGB8 166 77 0 , PixelRGB8 168 191 255 , PixelRGB8 145 53 4 , PixelRGB8 219 178 255 , PixelRGB8 116 94 0 , PixelRGB8 117 57 142 , PixelRGB8 177 209 133 , PixelRGB8 144 38 115 , PixelRGB8 198 203 140 , PixelRGB8 171 0 83 , PixelRGB8 136 155 102 , PixelRGB8 255 103 176 , PixelRGB8 97 119 69 , PixelRGB8 255 144 217 , PixelRGB8 119 99 51 , PixelRGB8 255 109 156 , PixelRGB8 247 187 120 , PixelRGB8 101 69 126 , PixelRGB8 255 138 76 , PixelRGB8 145 114 162 , PixelRGB8 255 166 102 , PixelRGB8 140 51 88 , PixelRGB8 255 162 119 , PixelRGB8 163 105 141 , PixelRGB8 255 99 102 , PixelRGB8 250 176 222 , PixelRGB8 134 63 19 , PixelRGB8 255 150 185 , PixelRGB8 119 71 30 , PixelRGB8 255 100 125 , PixelRGB8 171 124 88 , PixelRGB8 173 0 52 , PixelRGB8 208 146 181 , PixelRGB8 148 49 25 , PixelRGB8 248 173 168 , PixelRGB8 162 21 59 , PixelRGB8 255 158 138 , PixelRGB8 127 65 60 , PixelRGB8 215 141 156 , PixelRGB8 139 56 48 , PixelRGB8 189 117 132]

colour :: Int -> PixelRGB8
colour p = palette !! (p `mod` length palette)

baseColour :: PixelRGB8
baseColour = PixelRGB8 255 255 255

mkImage :: ProvMap -> Int -> Int -> Image PixelRGB8
mkImage pmap width height = generateImage mk width height
  where
    mk x y
      | Just p <- Map.lookup (x + y * width) pmap = colour p
      | otherwise                                 = baseColour
          
main :: IO ()
main = do
  [fin, widthS, fout] <- getArgs
  pmap <- getProv fin
  
  let width = read widthS 
      height = (maxAddr pmap `div` width) + 1
  writePng fout (mkImage pmap width height)
