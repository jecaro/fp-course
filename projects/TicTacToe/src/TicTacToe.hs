{-# LANGUAGE RankNTypes #-}

module TicTacToe where

import Control.Monad (join)
import Data.List (elemIndex, find)
import Data.Maybe (isJust, isNothing)
import Lens.Micro ((&), (?~), Lens, (^.), _1, _2, _3)
import Lens.Micro.Internal (Field1, Field2, Field3)

data Player = Cross | Plus
    deriving (Eq, Show)

data Coordinates = One | Two | Three
    deriving (Eq, Enum)

data Position = MkPosition !Coordinates !Coordinates
    deriving (Eq)

data Game = MkGame
    { gaFirstPlayer :: !Player,
      gaPositions :: ![Position]
    }

data MoveError = GameIsFinished | PositionOccupied

type Element = Maybe Player

type ThreeElements = (Element, Element, Element)

type Board = (ThreeElements, ThreeElements, ThreeElements)

empty :: Board
empty =
    let emptyLine = (Nothing, Nothing, Nothing)
     in (emptyLine, emptyLine, emptyLine)

fill :: Game -> Board
fill (MkGame player positions) =
    let f :: Position -> (Board, Player) -> (Board, Player)
        f (MkPosition x y) (b, p) =
            let b' = b & coordLens x . coordLens y ?~ p
                p' = other p
             in (b', p')
     in fst $ foldr f (empty, player) positions

other :: Player -> Player
other Cross = Plus
other Plus = Cross

coordLens ::
    (Field1 s t a b, Field2 s t a b, Field3 s t a b) =>
    Coordinates ->
    Lens s t a b
coordLens One = _1
coordLens Two = _2
coordLens Three = _3

row :: Coordinates -> Board -> ThreeElements
row coordinate board = board ^. coordLens coordinate

col :: Coordinates -> Board -> ThreeElements
col coordinates board =
    let x1 = board ^. _1 . coordLens coordinates
        x2 = board ^. _2 . coordLens coordinates
        x3 = board ^. _3 . coordLens coordinates
     in (x1, x2, x3)

diag1 :: Board -> ThreeElements
diag1 board = (board ^. _1 . _1, board ^. _2 . _2, board ^. _3 . _3)

diag2 :: Board -> ThreeElements
diag2 board = (board ^. _1 . _3, board ^. _2 . _2, board ^. _3 . _1)

winnerOnRow :: ThreeElements -> Maybe Player
winnerOnRow (Just p1, Just p2, Just p3) | p1 == p2 && p1 == p3 = Just p1
winnerOnRow _ = Nothing

whoWon :: Game -> Maybe Player
whoWon game =
    let b = fill game
        rows = ($ b) <$> [diag1, diag2] ++ (row <$> [One .. Three]) ++ (col <$> [One .. Three])
        winners = winnerOnRow <$> rows
     in join $ find isJust winners

isDraw :: Game -> Bool
isDraw game@(MkGame _ positions) = isNothing (whoWon game) && length positions == 9

playerAt :: Game -> Position -> Maybe Player
playerAt (MkGame player positions) position = do
    idx <- elemIndex position positions
    if even idx
        then pure player
        else pure (other player)

takeBack :: Game -> Maybe Game
takeBack game@(MkGame _ (_ : ps)) = Just $ game {gaPositions = ps}
takeBack (MkGame _ []) = Nothing

move :: Game -> Position -> Either MoveError Game
move game@(MkGame _ positions) position
    | isJust (whoWon game) || isDraw game = Left GameIsFinished
    | position `elem` positions = Left PositionOccupied
    | otherwise = Right $ game {gaPositions = position : positions}
