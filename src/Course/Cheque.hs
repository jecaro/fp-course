{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Show)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"

ten :: Digit -> Chars
ten Zero = Nil
ten One = "ten"
ten Two = "twenty"
ten Three = "thirty"
ten Four = "forty"
ten Five = "fifty"
ten Six = "sixty"
ten Seven = "seventy"
ten Eight = "eighty"
ten Nine = "ninety"

showDigit3 :: Digit3 -> Chars
showDigit3 (D1 d) = showDigit d
showDigit3 (D2 One Zero) = "ten"
showDigit3 (D2 One One) = "eleven"
showDigit3 (D2 One Two) = "twelve"
showDigit3 (D2 One Three) = "thirteen"
showDigit3 (D2 One Four) = "fourteen"
showDigit3 (D2 One Five) = "fifteen"
showDigit3 (D2 One Six) = "sixteen"
showDigit3 (D2 One Seven) = "seventeen"
showDigit3 (D2 One Eight) = "eightteen"
showDigit3 (D2 One Nine) = "nineteen"
showDigit3 (D2 d1 Zero) = ten d1
showDigit3 (D2 d1 d2) = ten d1 ++ '-' :. showDigit d2
showDigit3 (D3 d1 Zero Zero) = showDigit d1 ++ " hundred"
showDigit3 (D3 d1 Zero d3) = showDigit d1 ++ " hundred and " ++ showDigit d3
showDigit3 (D3 d1 d2 Zero) = showDigit d1 ++ " hundred and " ++ ten d2
showDigit3 (D3 d1 d2 d3) = showDigit d1 ++ " hundred and " ++ ten d2 ++ '-' :. showDigit d3

dollars ::
  Chars
  -> Chars
dollars string =
      -- Break the string in two
  let (integerString, decimalString) = break (== '.') string

      -- Convert a string to a list of digits
      stringToDigits :: Chars -> List Digit
      stringToDigits = listOptional fromChar

      -- Remove trailing zeros from a digit
      normalize :: Digit3 -> Digit3
      normalize (D3 Zero c1 c2) = normalize (D2 c1 c2)
      normalize (D2 Zero c) = D1 c
      normalize d = d

      -- Group a list of digit in 'Digit3'
      digitsToDigit3 :: List Digit -> List Digit3
      digitsToDigit3 digits =
        let f :: Digit -> List Digit3 -> List Digit3
            f d ((D1 d1) :. ds) = D2 d d1 :. ds
            f d ((D2 d1 d2) :. ds) = D3 d d1 d2 :. ds
            f d ds = D1 d :. ds
         in normalize <$> foldRight f Nil digits

      -- The decimal part in 'Digit3' form
      decimals :: Digit3
      decimals =
        case stringToDigits decimalString of
          Zero :. Nil -> D1 Zero
          c1 :. Nil -> D2 c1 Zero
          Zero :. c :. Nil -> D1 c
          Zero :. Zero :. _ -> D1 Zero
          c1 :. c2 :. _ -> D2 c1 c2
          _ -> D1 Zero

      -- The integer part as a list of 'Digit'
      integers :: List Digit3
      integers =
        case digitsToDigit3 $ stringToDigits integerString of
          Nil -> D1 Zero :. Nil
          xs -> xs

      -- The group of digits along its power of three
      integersAndPowerOf3 :: List (Digit3, Chars)
      integersAndPowerOf3 = zip integers (reverse $ take (length integers) illion)

      -- Convert it to a string
      showDigitWithPowerOf3 :: (Digit3, List Char) -> List Char
      showDigitWithPowerOf3 (d3, "") = showDigit3 d3 ++ " "
      showDigitWithPowerOf3 (D1 Zero, _) = ""
      showDigitWithPowerOf3 (d3, powerOf3) = showDigit3 d3 ++ " " ++ powerOf3 ++ " "

      -- Eventual terminating 's' for one 'Digit3'
      addSForOneDigit3 :: Digit3 -> Chars
      addSForOneDigit3 (D1 One) = ""
      addSForOneDigit3 _ = "s"

      -- Same for a list
      addSForListOfDigits3 :: List Digit3 -> Chars
      addSForListOfDigits3 l =
        case l of
          d :. Nil -> addSForOneDigit3 d
          Nil -> ""
          _ -> "s"

      -- Output the result
   in flatMap showDigitWithPowerOf3 integersAndPowerOf3
      ++ "dollar" ++ addSForListOfDigits3 integers
      ++ " and "
      ++ showDigit3 decimals ++ " cent" ++ addSForOneDigit3 decimals
