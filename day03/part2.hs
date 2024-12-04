import Control.Monad (void)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import System.IO (getContents)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

data Instruction = Mul Integer Integer | Do | Dont deriving Show

mul :: Parser Instruction
mul = do
    void $ string "mul("
    num1 <- many1 digit
    void $ char ','
    num2 <- many1 digit
    void $ char ')'
    return $ Mul (read num1) (read num2)

instruction :: Parser Instruction
instruction = try mul <|> try (Do <$ string "do()") <|> try (Dont <$ string "don't()")

-- Attempt to read instruction, otherwise consume a single char.
instructionOrConsume :: Parser (Maybe Instruction)
instructionOrConsume = try (Just <$> instruction) <|> (Nothing <$ anyChar)

parseInput :: String -> Maybe [Instruction]
parseInput input = case parse (many instructionOrConsume) "" input of
    Left _ -> Nothing  -- ParseError... Should not occur?
    Right xs -> Just $ catMaybes xs

update :: (Bool, Integer) -> Instruction -> (Bool, Integer)
update (b, acc) (Mul x y) = (b, if b then acc + x * y else acc)
update (b, acc) Do = (True, acc)
update (b, acc) Dont = (False, acc)

part2 :: String -> Maybe Integer
part2 input = snd . foldl' update (True, 0) <$> parseInput input

main :: IO ()
main = do
    input <- getContents
    print $ part2 input