module Board (Board, empty, render, diag1, diag2, row, col, winnerOnRow) where

import Coordinate (Coordinate, lens)
import Lens.Micro ((^.), _1, _2, _3)
import Player (Player)
import ThreeElements (ThreeElements)
import qualified ThreeElements

type Board = (ThreeElements, ThreeElements, ThreeElements)

empty :: Board
empty =
    let emptyLine = (Nothing, Nothing, Nothing)
     in (emptyLine, emptyLine, emptyLine)

render :: Board -> String
render board =
    ThreeElements.render (board ^. _1)
        <> "\n"
        <> ThreeElements.render (board ^. _2)
        <> "\n"
        <> ThreeElements.render (board ^. _3)
        <> "\n"

row :: Coordinate -> Board -> ThreeElements
row coordinate board = board ^. lens coordinate

col :: Coordinate -> Board -> ThreeElements
col coordinates board =
    let x1 = board ^. _1 . lens coordinates
        x2 = board ^. _2 . lens coordinates
        x3 = board ^. _3 . lens coordinates
     in (x1, x2, x3)

diag1 :: Board -> ThreeElements
diag1 board = (board ^. _1 . _1, board ^. _2 . _2, board ^. _3 . _3)

diag2 :: Board -> ThreeElements
diag2 board = (board ^. _1 . _3, board ^. _2 . _2, board ^. _3 . _1)

winnerOnRow :: ThreeElements -> Maybe Player
winnerOnRow (Just p1, Just p2, Just p3) | p1 == p2 && p1 == p3 = Just p1
winnerOnRow _ = Nothing
