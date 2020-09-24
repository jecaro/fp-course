module Position where

import Coordinate (Coordinate)
import qualified Coordinate
import Text.Parsec (spaces)
import Text.Parsec.String (Parser)

data Position = MkPosition !Coordinate !Coordinate
    deriving (Eq, Show)

parser :: Parser Position
parser = MkPosition <$> Coordinate.parser <* spaces <*> Coordinate.parser
