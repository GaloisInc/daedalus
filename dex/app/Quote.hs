module Quote where

import Data.Text(Text)
import Data.Text qualified as Text
import Daedalus.PP
import Data.Char

newtype Q a       = Q [QuoteWord a]
  deriving (Functor,Foldable,Traversable)

data QuoteWord a  = Meta a | Object Text
  deriving (Functor,Foldable,Traversable)

renderQuote :: Q Doc -> Doc
renderQuote (Q xs) = foldMap renderQuoteWord (hack xs)
  where
  -- As an artifact of parsing we emit newlines Object words at the end
  -- of blocks.  This is a quick-and-dirty function to remove them.
  hack = reverse . dropWhile onlyWS . reverse
  onlyWS w =
    case w of
      Object txt -> Text.all isSpace txt
      _ -> False

renderQuoteWord :: QuoteWord Doc -> Doc
renderQuoteWord w =
  case w of
    Meta a -> a
    Object a -> text (Text.unpack a)

instance PP a => PP (Q a) where
  pp (Q xs) = hcat (map pp xs)

instance PP a => PP (QuoteWord a) where
  pp w =
    case w of
      Meta a -> "$" <.> parens (pp a)
      Object a -> text (Text.unpack a)