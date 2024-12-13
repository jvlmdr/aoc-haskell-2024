import Data.Maybe (catMaybes)
import qualified Data.List as List

main :: IO ()
main = do
    input <- getLine
    print $ parseLine input
    print $ length $ (!! 25) $ iterate blink $ parseLine input

parseLine :: String -> [Integer]
parseLine = map read . words

blink1 :: Integer -> [Integer]
blink1 x
    | x == 0 = [1]
    | n == 2 * m = let (a, b) = splitAt m s in [read a, read b]
    | otherwise = [2024 * x]
    where
        s = show x
        n = length s
        m = n `div` 2

blink :: [Integer] -> [Integer]
blink = concatMap blink1
