module Player (Player (..), other, render) where

data Player = Cross | Plus
    deriving (Eq, Show)

other :: Player -> Player
other Cross = Plus
other Plus = Cross

render :: Player -> String
render Cross = "x"
render Plus = "+"
