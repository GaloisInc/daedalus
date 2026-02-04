module Quote where

import Data.Text(Text)
import Data.Text qualified as Text
import Daedalus.PP

newtype Q a       = Q [QuoteWord a]
  deriving (Functor,Foldable,Traversable)

data QuoteWord a  = Meta a | Object Text
  deriving (Functor,Foldable,Traversable)

renderQuote :: Q Doc -> Doc
renderQuote (Q xs) = foldMap renderQuoteWord xs

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