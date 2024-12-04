import Data.List

main :: IO ()
main = do
    input <- getContents
    print $ countXmas $ lines input

-- Count the number of times that 'XMAS' occurs on each line.
countXmas :: [String] -> Int
countXmas = sum . map (countSubstr "XMAS") . allDirections

-- Does this not exist in standard lib?
countSubstr :: Eq a => [a] -> [a] -> Int
countSubstr sub = length . filter (isPrefixOf sub) . tails

allDirections :: [String] -> [String]
allDirections xs = ys ++ map reverse ys where
    ys = xs ++ transpose xs ++ diags xs ++ diags (reverse xs)

diags :: [[a]] -> [[a]]
diags xs = map snd (diagsIndexed xs)

diagsIndexed :: [[a]] -> [(Integer, [a])]
diagsIndexed [] = []
diagsIndexed (xs : xss) = merge (zip [0..] xs) yss where
    -- Subtract 1 from index in next row.
    yss = [(i - 1, d) | (i, d) <- diagsIndexed xss]
    -- Merge sorted heads from this row and sorted tails from next row.
    merge :: [(Integer, a)] -> [(Integer, [a])] -> [(Integer, [a])]
    merge [] ys = ys
    merge ((i, x) : xs) [] = (i, [x]) : merge xs []
    merge ((i, x) : xs) ((j, y) : ys)
        | i == j = (i, x : y) : merge xs ys
        | i < j = (i, [x]) : merge xs ((j, y) : ys)
        | i > j = (j, y) : merge ((i, x) : xs) ys
