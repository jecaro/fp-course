module Position where

import Coordinate (Coordinate)
import qualified Coordinate (parser, render)
import Text.Parsec (spaces)
import Text.Parsec.String (Parser)

data Position = MkPosition !Coordinate !Coordinate
    deriving (Eq, Show)

parser :: Parser Position
parser = MkPosition <$> Coordinate.parser <* spaces <*> Coordinate.parser

render :: Position -> String
render (MkPosition p1 p2) = Coordinate.render p1 <> " " <> Coordinate.render p2
