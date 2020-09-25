{-# LANGUAGE RankNTypes #-}

module Coordinate (Coordinate (..), lens, render, parser) where

import Lens.Micro (Lens, _1, _2, _3)
import Lens.Micro.Internal (Field1, Field2, Field3)
import Text.Parsec ((<|>))
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser)

data Coordinate = One | Two | Three
    deriving (Eq, Enum, Show)

parser :: Parser Coordinate
parser = One <$ string "1" <|> Two <$ string "2" <|> Three <$ string "3"

lens ::
    (Field1 s t a b, Field2 s t a b, Field3 s t a b) =>
    Coordinate ->
    Lens s t a b
lens One = _1
lens Two = _2
lens Three = _3

render :: Coordinate -> String
render One = "1"
render Two = "2"
render Three = "3"
