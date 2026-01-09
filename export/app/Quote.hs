module Quote where

import Data.Text.Lazy(Text)
import Data.Text.Lazy qualified as Lazy
import Daedalus.PP

newtype Q a       = Q [QuoteWord a]
  deriving (Functor,Foldable,Traversable)

data QuoteWord a  = Meta a | Object Text
  deriving (Functor,Foldable,Traversable)

renderQuote :: Q Text -> Text
renderQuote (Q xs) = foldMap renderQuoteWord xs

renderQuoteWord :: QuoteWord Text -> Text
renderQuoteWord w =
  case w of
    Meta a -> a
    Object a -> a

instance PP a => PP (Q a) where
  pp (Q xs) = fcat (map pp xs)

instance PP a => PP (QuoteWord a) where
  pp w =
    case w of
      Meta a -> "$" <.> parens (pp a)
      Object a -> text (Lazy.unpack a)