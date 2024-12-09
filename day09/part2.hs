import Data.Char
import Data.List

main :: IO ()
main = do
    input <- getLine
    print $ checksum $ pack $ initSegments $ map (toInteger . digitToInt) input

-- File position size id
data Segment = File Integer Integer Integer deriving (Show, Eq)

initSegments :: [Integer] -> [Segment]
initSegments = go 0 0 where
    go :: Integer -> Integer -> [Integer] -> [Segment]
    go _ _ [] = []
    go pos n [a] = [File pos a n]
    go pos n (a : b : xs) = File pos a n : go (pos + a + b) (n + 1) xs

pack :: [Segment] -> [Segment]
pack segs = foldr insert segs segs where
    insert :: Segment -> [Segment] -> [Segment]
    insert x [a]
        | x == a = [a]
    insert x@(File ix sx nx) (a@(File ia sa na) : b@(File ib sb nb) : bs)
        | x == a = a : b : bs
        | ia + sa + sx <= ib = a : File (ia + sa) sx nx : delete x (b : bs)
        | otherwise = a : insert x (b : bs)

checksum :: [Segment] -> Integer
checksum = sum . map (\(File i s n) -> n * sum [i..i+s-1])
