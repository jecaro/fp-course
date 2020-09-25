{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Except (liftEither, runExcept, withExcept)
import Options.Applicative
    ( (<**>),
      (<|>),
      Parser,
      ParserInfo,
      execParser,
      flag',
      fullDesc,
      helper,
      info,
      long,
      progDesc,
      short,
    )
import qualified Position
import Text.Parsec (parse)
import qualified Text.Parsec as Parsec (ParseError)
import TicTacToe
    ( Game,
      Player (..),
      State (..),
      mkGame,
      move,
      nextPlayer,
      state,
    )
import qualified TicTacToe (Error (..), render)

playerParser :: Parser Player
playerParser =
    flag' Cross (long "cross" <> short 'c')
        <|> flag' Plus (long "plus" <> short 'p')

opts :: ParserInfo Player
opts =
    info
        (playerParser <**> helper)
        (fullDesc <> progDesc "Start the TicTacToe game")

data Error = EParsec Parsec.ParseError | ETicTacToe TicTacToe.Error

render :: Error -> String
render (EParsec err) = show err
render (ETicTacToe TicTacToe.GameIsFinished) = "Game is finished"
render (ETicTacToe (TicTacToe.PositionOccupied position)) =
    "Position is occupied " <> Position.render position

play :: Game -> IO ()
play game = do
    putStrLn $ TicTacToe.render game
    case state game of
        Draw -> putStrLn "Draw !"
        Finish p -> putStrLn $ "Winner is " <> show p
        Playing -> do
            putStrLn $ "Player " <> show (nextPlayer game)
            line <- getLine
            let gameOrError :: Either Error Game
                gameOrError = runExcept $ do
                    position <-
                        withExcept EParsec
                            $ liftEither
                            $ parse Position.parser "" line
                    withExcept ETicTacToe $ liftEither $ move game position
            case gameOrError of
                Left err -> do
                    putStrLn $ "Error: " <> render err
                    putStrLn "Try again"
                    play game
                Right game' -> play game'

main :: IO ()
main = do
    player <- execParser opts
    play $ mkGame player
