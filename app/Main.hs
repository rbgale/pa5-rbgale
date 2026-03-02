module Main (main) where

import Control.Monad (forM_)
import RollingStones (firstPlayerWins)

showPercent :: Double -> String
showPercent p = show (fromIntegral (round (p * 10000) :: Integer) / 100 :: Double) ++ "%"

main :: IO ()
main = forM_ [2..50] $ \n ->
    putStrLn $ show n ++ ": " ++ showPercent (firstPlayerWins n)
