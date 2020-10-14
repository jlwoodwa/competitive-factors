{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Competitive.Factors
import Control.Arrow ((&&&))
import Control.Monad (forever, join)
import Data.Function ((&))
import Data.List (group, minimumBy, sort)
import Data.List.Extra (allSame, chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Debug.Trace

divisions :: [a] -> [[[a]]]
divisions xs =
  let divs =
        map fromIntegral
          . divisors
          . fromIntegral
          . length
          $ xs
   in flip map divs $
        flip chunksOf xs

chkMultigram :: Ord a => [[a]] -> Bool
chkMultigram =
  allSame
    . map
      ( M.fromList
          . map (head &&& length)
          . group
          . sort
      )

smallestMultigram :: Ord a => [a] -> Maybe [a]
smallestMultigram xs =
  join 
  . fmap (grd (/= xs) . head . minimumBy (comparing (length . head)))
    . grd (/= [])
    . filter chkMultigram
    . divisions
    $ xs
  where
    grd :: (a -> Bool) -> a -> Maybe a
    grd f x
      | f x = Just x
      | otherwise = Nothing

main = forever $ getLine >>= print . smallestMultigram
