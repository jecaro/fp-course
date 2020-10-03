{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (foldM, guard, void)
import Coordinate (Coordinate (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Position (Position (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import TicTacToe
    ( Error (..),
      Game,
      Player (..),
      State (..),
      isDraw,
      mkGame,
      move,
      nextPlayer,
      playerAt,
      state,
      takeBack,
      whoWon,
    )

draw :: Player -> Either Error Game
draw p =
    foldM
        move
        (mkGame p)
        [ MkPosition One One,
          MkPosition Two Two,
          MkPosition Two One,
          MkPosition Three One,
          MkPosition One Three,
          MkPosition One Two,
          MkPosition Three Two,
          MkPosition Two Three,
          MkPosition Three Three
        ]

line1 :: Player -> Either Error Game
line1 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One One,
          MkPosition Two One,
          MkPosition One Two,
          MkPosition Two Two,
          MkPosition One Three
        ]

line2 :: Player -> Either Error Game
line2 p =
    foldM
        move
        (mkGame p)
        [ MkPosition Two One,
          MkPosition Three One,
          MkPosition Two Two,
          MkPosition Three Two,
          MkPosition Two Three
        ]

line3 :: Player -> Either Error Game
line3 p =
    foldM
        move
        (mkGame p)
        [ MkPosition Three One,
          MkPosition One One,
          MkPosition Three Two,
          MkPosition One Two,
          MkPosition Three Three
        ]

col1 :: Player -> Either Error Game
col1 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One One,
          MkPosition One Two,
          MkPosition Two One,
          MkPosition Two Two,
          MkPosition Three One
        ]

col2 :: Player -> Either Error Game
col2 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One Two,
          MkPosition One Three,
          MkPosition Two Two,
          MkPosition Two Three,
          MkPosition Three Two
        ]

col3 :: Player -> Either Error Game
col3 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One Three,
          MkPosition One One,
          MkPosition Two Three,
          MkPosition Two One,
          MkPosition Three Three
        ]

diag1 :: Player -> Either Error Game
diag1 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One One,
          MkPosition One Two,
          MkPosition Two Two,
          MkPosition One Three,
          MkPosition Three Three
        ]

diag2 :: Player -> Either Error Game
diag2 p =
    foldM
        move
        (mkGame p)
        [ MkPosition One Three,
          MkPosition One Two,
          MkPosition Two Two,
          MkPosition One Three,
          MkPosition Three One
        ]

spec :: Spec
spec =
    describe "Test the game" $ do
        describe "Test the state function" $ do
            it "on the empty game" $
                state (mkGame Cross) `shouldBe` Playing
            it "on the first line" $
                state <$> line1 Cross `shouldBe` Right (Finish Cross)
            it "on the second line" $
                state <$> line2 Cross `shouldBe` Right (Finish Cross)
            it "on the third line" $
                state <$> line3 Cross `shouldBe` Right (Finish Cross)
            it "on the first column" $
                state <$> col1 Plus `shouldBe` Right (Finish Plus)
            it "on the second column" $
                state <$> col2 Plus `shouldBe` Right (Finish Plus)
            it "on the third column" $
                state <$> col3 Plus `shouldBe` Right (Finish Plus)
            it "on the first diagonal" $
                state <$> diag1 Cross `shouldBe` Right (Finish Cross)
            it "on the second diagonal" $
                state <$> diag1 Plus `shouldBe` Right (Finish Plus)
            it "on draw" $
                state <$> draw Plus `shouldBe` Right Draw
        describe "Test the isDraw function" $ do
            it "on the empty game" $
                isDraw (mkGame Cross) `shouldBe` False
            it "on the first line" $
                isDraw <$> line1 Cross `shouldBe` Right False
            it "on the second line" $
                isDraw <$> line2 Cross `shouldBe` Right False
            it "on the third line" $
                isDraw <$> line3 Cross `shouldBe` Right False
            it "on the first column" $
                isDraw <$> col1 Plus `shouldBe` Right False
            it "on the second column" $
                isDraw <$> col2 Plus `shouldBe` Right False
            it "on the third column" $
                isDraw <$> col3 Plus `shouldBe` Right False
            it "on the first diagonal" $
                isDraw <$> diag1 Cross `shouldBe` Right False
            it "on the second diagonal" $
                isDraw <$> diag1 Plus `shouldBe` Right False
            it "on draw" $
                isDraw <$> draw Plus `shouldBe` Right True
        describe "Test the whoWon function" $ do
            it "on the empty game" $
                whoWon (mkGame Cross) `shouldBe` Nothing
            it "on the first line" $
                whoWon <$> line1 Cross `shouldBe` Right (Just Cross)
            it "on the second line" $
                whoWon <$> line2 Cross `shouldBe` Right (Just Cross)
            it "on the third line" $
                whoWon <$> line3 Cross `shouldBe` Right (Just Cross)
            it "on the first column" $
                whoWon <$> col1 Plus `shouldBe` Right (Just Plus)
            it "on the second column" $
                whoWon <$> col2 Plus `shouldBe` Right (Just Plus)
            it "on the third column" $
                whoWon <$> col3 Plus `shouldBe` Right (Just Plus)
            it "on the first diagonal" $
                whoWon <$> diag1 Cross `shouldBe` Right (Just Cross)
            it "on the second diagonal" $
                whoWon <$> diag1 Plus `shouldBe` Right (Just Plus)
            it "on draw" $
                whoWon <$> draw Plus `shouldBe` Right Nothing
        describe "Test the nextPlayer function" $ do
            it "on Cross" $
                nextPlayer (mkGame Cross) `shouldBe` Cross
            it "on Plus" $
                nextPlayer (mkGame Plus) `shouldBe` Plus
        describe "Test the playerAt function" $ do
            it "on empty game" $
                playerAt (mkGame Cross) (MkPosition One One) `shouldBe` Nothing
            it "on 1 1" $
                flip playerAt (MkPosition One One)
                    <$> draw Plus `shouldBe` Right (Just Plus)
            it "on 1 2" $
                flip playerAt (MkPosition One Two)
                    <$> draw Plus `shouldBe` Right (Just Cross)
            it "on 3 2" $
                flip playerAt (MkPosition Three Two)
                    <$> draw Plus `shouldBe` Right (Just Plus)
        describe "Test the takeBack function" $ do
            it "on empty game" $
                takeBack (mkGame Cross) `shouldBe` Nothing
            it "with one move" $
                let empty = mkGame Cross
                    oneMove = move empty (MkPosition One One)
                 in takeBack <$> oneMove `shouldBe` Right (Just empty)
            it "with two moves" $
                let empty = mkGame Cross
                    firstMove = move empty (MkPosition One One)
                    secondMove = flip move (MkPosition Two One) =<< firstMove
                 in takeBack <$> secondMove `shouldBe` Just <$> firstMove

allCoordinates :: [Coordinate]
allCoordinates = [minBound ..]

allPositions :: [Position]
allPositions = MkPosition <$> allCoordinates <*> allCoordinates

genPosition :: Gen Position
genPosition =
    MkPosition <$> Gen.enumBounded <*> Gen.enumBounded

genEmptyGame :: Gen Game
genEmptyGame = mkGame <$> Gen.enumBounded

-- | Return a random game along available positions
genNonEmptyNonFinishedGame :: Gen (Game, [Position])
genNonEmptyNonFinishedGame = do
    empty <- genEmptyGame
    allPositionsShuffled <- Gen.shuffle allPositions
    n <- Gen.integral (Range.constantFrom 1 1 8)
    let (played, remaining) = splitAt n allPositionsShuffled
        gameOrError = foldM move empty played
    case gameOrError of
        Left _ -> Gen.discard
        Right game -> do
            guard (state game == Playing)
            pure (game, remaining)

prop_empty :: Property
prop_empty =
    property $ do
        game <- forAll genEmptyGame
        position <- forAll genPosition
        let game' = move game position
        void $ evalEither game'

prop_fourMoves :: Property
prop_fourMoves =
    property $ do
        game <- forAll genEmptyGame
        positions <- forAll $ take 4 <$> Gen.shuffle allPositions
        let gameOrError = foldM move game positions
        void $ evalEither gameOrError

prop_twice :: Property
prop_twice =
    property $ do
        game <- forAll genEmptyGame
        position <- forAll genPosition
        let gameOrError = move game position
        game' <- evalEither gameOrError
        move game' position === Left (PositionOccupied position)

prop_takeBackEmpty :: Property
prop_takeBackEmpty =
    property $ do
        game <- forAll genEmptyGame
        takeBack game === Nothing

prop_takeBack :: Property
prop_takeBack =
    property $ do
        (game, positions) <- forAll genNonEmptyNonFinishedGame
        case positions of
            [] -> discard
            position : _ -> do
                let gameOrError = move game position
                (takeBack <$> gameOrError) === Right (Just game)

prop_playerAt :: Property
prop_playerAt =
    property $ do
        (game, positions) <- forAll genNonEmptyNonFinishedGame
        case positions of
            [] -> discard
            position : _ -> do
                (`playerAt` position) game === Nothing
                let player = nextPlayer game
                    gameOrError = move game position
                ((`playerAt` position) <$> gameOrError) === Right (Just player)

properties :: TestTree
properties =
    testGroup
        "Properties"
        [ testProperty
              "Play on empty board"
              prop_empty,
          testProperty
              "Play four moves"
              prop_fourMoves,
          testProperty
              "Play twice the same position"
              prop_twice,
          testProperty
              "Take back on empty board"
              prop_takeBackEmpty,
          testProperty
              "Take back"
              prop_takeBack,
          testProperty
              "Player at"
              prop_playerAt
        ]

main :: IO ()
main = do
    hspecTests <- testSpec "Unit tests" spec
    defaultMain $ testGroup "Test suite" [hspecTests, properties]
