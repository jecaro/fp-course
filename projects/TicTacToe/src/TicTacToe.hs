module TicTacToe
    ( Game,
      Error (..),
      Player (..),
      State (..),
      gaPlayer,
      isDraw,
      move,
      mkGame,
      nextPlayer,
      playerAt,
      state,
      render,
      takeBack,
      whoWon,
    )
where

import Board (Board, col, diag1, diag2, empty, row, winnerOnRow)
import qualified Board
import Control.Monad (join)
import Coordinate (Coordinate (..), lens)
import Data.List (elemIndex, find)
import Data.Maybe (isJust, isNothing)
import Lens.Micro ((&), (?~))
import Player (Player (..), other)
import Position (Position (..))

data Game = MkGame
    { gaPlayer :: !Player,
      gaPositions :: ![Position]
    }
    deriving (Show)

data State = Draw | Playing | Finish Player

data Error = GameIsFinished | PositionOccupied Position
    deriving (Show)

fill :: Game -> Board
fill (MkGame player positions) =
    let f :: Position -> (Board, Player) -> (Board, Player)
        f (MkPosition x y) (b, p) =
            let b' = b & lens y . lens x ?~ p
                p' = other p
             in (b', p')
     in fst $ foldr f (empty, player) positions

whoWon :: Game -> Maybe Player
whoWon game =
    let b = fill game
        rows = ($ b) <$> [diag1, diag2] ++ (row <$> [One .. Three]) ++ (col <$> [One .. Three])
        winners = winnerOnRow <$> rows
     in join $ find isJust winners

isDraw :: Game -> Bool
isDraw game@(MkGame _ positions) = isNothing (whoWon game) && length positions == 9

state :: Game -> State
state game
    | isDraw game = Draw
    | otherwise =
        maybe Playing Finish (whoWon game)

playerAt :: Game -> Position -> Maybe Player
playerAt (MkGame player positions) position = do
    idx <- elemIndex position positions
    if even idx
        then pure player
        else pure (other player)

takeBack :: Game -> Maybe Game
takeBack game@(MkGame _ (_ : ps)) = Just game {gaPositions = ps}
takeBack (MkGame _ []) = Nothing

mkGame :: Player -> Game
mkGame player = MkGame player []

render :: Game -> String
render = Board.render . fill

move :: Game -> Position -> Either Error Game
move game@(MkGame _ positions) position
    | isJust (whoWon game) || isDraw game = Left GameIsFinished
    | position `elem` positions = Left $ PositionOccupied position
    | otherwise = Right game {gaPositions = position : positions}

nextPlayer :: Game -> Player
nextPlayer (MkGame player positions) =
    if even (length positions)
        then player
        else other player
