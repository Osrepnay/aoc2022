module Day21.Part1 (thisMain) where

import Data.Char
import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as M
import System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day21" ReadMode
    contents <- hGetContents handle
    let monkeys = M.fromList $ parseMonkey <$> lines contents
    print $ runMonkey "root" monkeys
    hClose handle

data MonkeyOp
    = ImmediateMonkey Int
    | DelayMonkey String String (Int -> Int -> Int)

runMonkey :: String -> Map String MonkeyOp -> Int
runMonkey monkey monkeys = case thisOp of
    ImmediateMonkey x  -> x
    DelayMonkey a b op -> runMonkey a monkeys `op` runMonkey b monkeys
  where
    thisOp = monkeys ! monkey

parseMonkey :: String -> (String, MonkeyOp)
parseMonkey s = (name, monkeyOp)
  where
    splot = splitOn ": " s
    name = head splot
    op = last splot
    opWords = words op
    monkeyOp
        | isDigit (head op) = ImmediateMonkey $ read op
        | otherwise = DelayMonkey (head opWords) (last opWords)
            $ case opWords !! 1 of
                "+" -> (+)
                "-" -> (-)
                "*" -> (*)
                "/" -> div
                _ -> error "unknown monkey operation"
