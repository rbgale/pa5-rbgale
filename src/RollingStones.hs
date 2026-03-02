module RollingStones where

import Data.Array (Array, (!), array)
import Probability (Probability (..), normalize, roll)

rollingStones :: Array Int (Probability Int)
rollingStones = arr
  where
    arr = array (0, 50) [(n, go n) | n <- [0..50]]

    go 0 = Probability { getProbabilities = [(0, 1.0)] }
    go n = normalize $ do
        r <- roll
        remove r n

    remove r n
        | r >= n = pure 1
        | otherwise = do
            k <- arr ! (n - r)
            pure (k + 1)

firstPlayerWins :: Int -> Double
firstPlayerWins n = sum [p | (k, p) <- getProbabilities (rollingStones ! n), odd k]
