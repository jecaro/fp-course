module Element (Element, render) where

import qualified Player
import Player (Player)

type Element = Maybe Player

render :: Element -> String
render Nothing = "-"
render (Just p) = Player.render p
