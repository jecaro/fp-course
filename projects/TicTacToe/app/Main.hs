module Main where

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
import TicTacToe
    ( Game,
      Player (..),
      State (..),
      mkGame,
      move,
      nextPlayer,
      render,
      state,
    )

playerParser :: Parser Player
playerParser =
    flag' Cross (long "cross" <> short 'c')
        <|> flag' Plus (long "plus" <> short 'p')

opts :: ParserInfo Player
opts =
    info
        (playerParser <**> helper)
        (fullDesc <> progDesc "Start the TicTacToe game")

play :: Game -> IO ()
play game = do
    putStrLn $ render game
    case state game of
        Draw -> putStrLn "Draw !"
        Finish p -> putStrLn $ "Winner is " <> show p
        Playing -> do
            putStrLn $ "Player " <> show (nextPlayer game)
            line <- getLine
            let game' =
                    case parse Position.parser "" line of
                        Left _ -> game
                        Right position ->
                            case move game position of
                                Left _ -> game
                                Right g -> g
            play game'

main :: IO ()
main = do
    player <- execParser opts
    play $ mkGame player
