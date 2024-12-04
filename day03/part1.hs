import Control.Monad (void)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

-- Parser for the pattern "mul(\d+,\d+)"
mul :: Parser (Integer, Integer)
mul = do
    void $ string "mul("
    num1 <- many1 digit
    void $ char ','
    num2 <- many1 digit
    void $ char ')'
    return (read num1, read num2)

-- Try to read a "mul" expression.
-- If it fails, consume a single char.
mulOrChar :: Parser (Maybe (Integer, Integer))
mulOrChar = (Just <$> try mul) <|> (anyChar >> return Nothing)

parseInput :: String -> Maybe [(Integer, Integer)]
parseInput input = case parse (many mulOrChar) "" input of
    Left _ -> Nothing
    Right xs -> Just $ catMaybes xs

part1 :: String -> Maybe Integer
part1 input = sum . map (uncurry (*)) <$> parseInput input

main :: IO ()
main = do
    input <- getContents
    print $ part1 input
