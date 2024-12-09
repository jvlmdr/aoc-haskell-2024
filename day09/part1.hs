import Data.Char
import Data.List

main :: IO ()
main = do
    input <- getLine
    print $ checksum $ compact $ fill $ map digitToInt input

fill :: [Int] -> [Maybe Int]
fill = concat . go 0 True where
    go :: Int -> Bool -> [Int] -> [[Maybe Int]]
    go _ _ [] = []
    go n isFile (size : xs) = y : ys where
        y = replicate size $ if isFile then Just n else Nothing
        ys = go (if isFile then n + 1 else n) (not isFile) xs

compact :: [Maybe Int] -> [Int]
compact disk = go (zip [0..] disk) (reverse $ zip [0..] disk) where
    go :: [(Int, Maybe Int)] -> [(Int, Maybe Int)] -> [Int]
    go [] _ = []
    go _ [] = []
    go ((a, Nothing) : xs) ((b, Just y) : ys)
        | a <= b = y : go xs ys
        | otherwise = []
    go ((a, Just x) : xs) ys@((b, _) : _)
        | a <= b = x : go xs ys
        | otherwise = []
    go xs ((_, Nothing) : ys) = go xs ys

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]