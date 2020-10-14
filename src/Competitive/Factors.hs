module Competitive.Factors (primes, factor, divisors, totient) where

import Control.Arrow
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as M

primes :: [Integer]
primes = 2 : sieve primes [3 ..]
  where
    sieve :: [Integer] -> [Integer] -> [Integer]
    sieve (p : ps) xs =
      let (h, t) = span (< p * p) xs
       in h ++ sieve ps (filter ((/= 0) . (`mod` p)) t)

listFactors :: Integer -> [Integer]
listFactors = go primes
  where
    go :: [Integer] -> Integer -> [Integer]
    go _ 1 = []
    go (p : ps) n
      | p * p > n = [n]
      | n `mod` p == 0 = p : go (p : ps) (n `div` p)
      | otherwise = go ps n

factor :: Integer -> Map Integer Int
factor =
  listFactors
    >>> group
    >>> map (head &&& length)
    >>> M.fromList

divisors :: Integer -> [Integer]
divisors =
  factor
    >>> M.assocs
    >>> map (\(p, k) -> take (k + 1) (iterate (* p) 1))
    >>> sequence
    >>> map product

totient :: Integer -> Integer
totient =
  factor
    >>> M.assocs
    >>> map (\(p, k) -> p ^ (k -1) * (p -1))
    >>> product
