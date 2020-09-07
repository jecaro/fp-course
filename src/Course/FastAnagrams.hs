{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams string name =
   let listToNCSet :: List Chars -> S.Set NoCaseString
       listToNCSet lc = (S.fromList . hlist) (NoCaseString <$> lc)
       ncPermutations :: List NoCaseString
       ncPermutations = NoCaseString <$> permutations string
       findAnagrams :: List Chars -> List Chars
       findAnagrams lc = ncString <$> filter (flip S.member $ listToNCSet lc) ncPermutations
    in findAnagrams . lines <$> readFile name

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Ord NoCaseString where
  compare (NoCaseString str1) (NoCaseString str2) = compare str1 str2

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
