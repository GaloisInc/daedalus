{-# Language OverloadedStrings #-}
module Daedalus.PP (module Daedalus.PP, module PP) where

import Data.List(intersperse)
import Data.Text (Text)
import Data.ByteString(ByteString)
import Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding ((<>))

import Daedalus.SourceRange

bullet :: Doc
bullet = if useUni then "•" else "*"
  where useUni = True

backticks :: Doc -> Doc
backticks p = "`" <.> p <.> "`"

buletItem :: Doc -> Doc
buletItem d = bullet <+> d

bullets :: [Doc] -> Doc
bullets ds = vcat (map buletItem ds)

vcat' :: [Doc] -> Doc
vcat' xs = vcat (intersperse " " xs)

block :: Doc -> Doc -> Doc -> [Doc] -> Doc
block open sp close xs =
  case xs of
    [] -> open <+> close
    x : ys -> vcat (start : things ++ [end])
        where start  = open <+> x
              things = [ sp <+> y | y <- ys ]
              end    = close

commaSep :: [Doc] -> Doc
commaSep ds = hsep (punctuate comma ds)

showPP :: PP a => a -> String
showPP = show . pp

infixl 6 <.>

(<.>) :: Doc -> Doc -> Doc
(<.>) = (PP.<>)

class PP t where
  ppPrec :: Int -> t -> Doc
  ppPrec _ = pp

  pp     :: t -> Doc
  pp = ppPrec 0


instance PP Text where
  pp = text . Text.unpack

instance PP ByteString where
  pp = text . show . BS.unpack

instance PP Bool where
  pp = text . show

instance PP Int where
  pp = int

instance PP Integer where
  pp = integer

instance PP Word32 where
  pp = text . show

instance PP SourcePos where
  pp = text . prettySourcePos

instance PP SourceRange where
  pp = text . prettySourceRangeLong

wrapIf :: Bool -> Doc -> Doc
wrapIf p d = if p then parens d else d



