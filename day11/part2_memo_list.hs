import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

main :: IO ()
main = do
    input <- getLine
    print $ parseLine input
    print $ memoCount 75 $ parseLine input

type Stone = Integer

parseLine :: String -> [Stone]
parseLine = map read . words

memoCount :: Int -> [Stone] -> Integer
memoCount n xs = sum $ map (countList !! n !) xs where
    supportAtList :: [Set Stone]
    supportAtList = map supportAt [0 ..]
    -- The set of unique stones obtained from `xs` after `n` blinks.
    supportAt :: Int -> Set Stone
    supportAt 0 = Set.fromList xs
    supportAt n = Set.unions $ map (Set.fromList . blink1) $ Set.toList $ supportAtList !! (n - 1)

    -- The set of all stones seen in the course of `n` blinks.
    support :: Set Stone
    support = Set.unions $ take (n + 1) supportAtList

    countList :: [Map Stone Integer]
    countList = map count [0 ..]
    -- The number of stones that each `x` will expand into after `n` iterations.
    count :: Int -> Map Stone Integer
    count 0 = Map.fromList $ map (, 1) $ Set.toList support
    count n = Map.fromList $ map (\x -> (x, sum $ map (countList !! (n - 1) !) $ blink1 x)) $ Set.toList support

blink1 :: Stone -> [Stone]
blink1 x
    | x == 0 = [1]
    | n == 2 * m = let (a, b) = splitAt m s in [read a, read b]
    | otherwise = [2024 * x]
    where
        s = show x
        n = length s
        m = n `div` 2
