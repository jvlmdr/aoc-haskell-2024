import Data.List

main :: IO ()
main = do
    input <- getContents
    print $ count $ lines input

count :: [String] -> Int
count (r1 : rs@(r2 : r3 : _)) = countRow [r1, r2, r3] + count rs
count _ = 0

countRow :: [String] -> Int
countRow ((a11:xs@(_:a13:_)) : (_:ys@(a22:_:_)) : (a31:zs@(_:a33:_)) : _) =
    (if b1 && b2 then 1 else 0) + countRow [xs, ys, zs] where
        d1 = [a11, a22, a33]
        d2 = [a13, a22, a31]
        b1 = (d1 == "MAS") || (d1 == "SAM")
        b2 = (d2 == "MAS") || (d2 == "SAM")
countRow _ = 0