import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
    input <- getLine
    print $ sum $ blinks 75 $ fromList $ parseLine input

type Stone = Integer

parseLine :: String -> [Stone]
parseLine = map read . words

blink1 :: Stone -> [Stone]
blink1 x
    | x == 0 = [1]
    | n == 2 * m = let (a, b) = splitAt m s in [read a, read b]
    | otherwise = [2024 * x]
    where
        s = show x
        n = length s
        m = n `div` 2

fromList :: [Stone] -> Map Stone Integer
fromList = Map.fromListWith (+) . map (, 1)

blinks :: Int -> Map Stone Integer -> Map Stone Integer
blinks 0 = id
blinks n = Map.unionsWith (+) . Map.mapWithKey (\x k -> rep k $ blink1' x) . blinks (n - 1)

rep :: Integer -> Map Stone Integer -> Map Stone Integer
rep n = Map.map (* n)

blink1' :: Stone -> Map Stone Integer
blink1' = fromList . blink1
