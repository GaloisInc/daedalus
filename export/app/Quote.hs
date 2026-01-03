module Quote where

import Data.Text.Lazy(Text)

newtype Q a       = Q [QuoteWord a]
  deriving (Functor,Foldable,Traversable)

data QuoteWord a  = Meta a | Object Text
  deriving (Functor,Foldable,Traversable)

render :: Q Text -> Text
render (Q xs) = foldMap renderQuoteWord xs

renderQuoteWord :: QuoteWord Text -> Text
renderQuoteWord w =
  case w of
    Meta a -> a
    Object a -> a