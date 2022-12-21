module Day21.Part2 (thisMain) where

import Data.Char
import Data.List.Split
import Data.Map (Map, (!))
import Data.Maybe
import qualified Data.Map as M
import System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day21" ReadMode
    contents <- hGetContents handle
    let monkeys = M.fromList $ parseMonkey <$> lines contents
    let (leftRoot, rightRoot) = extractLR $ monkeys ! "root"
    let leftMath = abstractify leftRoot monkeys
    let rightMath = abstractify rightRoot monkeys
    let leftPath = humnPath leftMath
    let rightPath = humnPath rightMath
    let (path, maths) =
            case leftPath of
                Just x -> (x, (leftMath, rightMath))
                Nothing -> (fromJust rightPath, (rightMath, leftMath))
    print $ simplify $ isolateHumn maths path
    hClose handle

data Op = Add | Sub | Mul | Div deriving Show

invOp :: Op -> Op
invOp Add = Sub
invOp Sub = Add
invOp Mul = Div
invOp Div = Mul

data Math
    = Bin Op Math Math
    | Number Int
    | Variable String
    deriving Show

simplify :: Math -> Int
simplify (Variable _) = error "can't simplify variable"
simplify (Bin Add l r) = simplify l + simplify r
simplify (Bin Sub l r) = simplify l - simplify r
simplify (Bin Mul l r) = simplify l * simplify r
simplify (Bin Div l r) = simplify l `div` simplify r
simplify (Number x) = x

isolateHumn :: (Math, Math) -> [Bool] -> Math
isolateHumn (Variable "humn", other) _ = other
isolateHumn (Variable _, _) _ = error "no humn found"
isolateHumn (Number _, _) _ = error "no humn found"
isolateHumn (Bin {}, _) [] = error "no humn found"

isolateHumn (Bin Sub l r, other) (True : ps) =
    isolateHumn (r, Bin Sub l other) ps

isolateHumn (Bin Div l r, other) (True : ps) =
    isolateHumn (r, Bin Div l other) ps

isolateHumn (Bin op l r, other) (False : ps) =
    isolateHumn (l, Bin (invOp op) other r) ps
isolateHumn (Bin op l r, other) (True : ps) =
    isolateHumn (r, Bin (invOp op) other l) ps

humnPath :: Math -> Maybe [Bool]
humnPath (Variable "humn") = Just []
humnPath (Variable _) = Nothing
humnPath (Number _) = Nothing
humnPath (Bin _ l r) = (isNothing lh :) <$> joinMaybes lh (humnPath r)
  where
    lh = humnPath l
    joinMaybes (Just x) _ = Just x
    joinMaybes Nothing (Just x) = Just x
    joinMaybes Nothing Nothing = Nothing

data MonkeyOp
    = ImmediateMonkey Int
    | DelayMonkey String String Char
extractLR :: MonkeyOp -> (String, String)
extractLR (ImmediateMonkey _) = error "can't extract immediate monkey"
extractLR (DelayMonkey l r _) = (l, r)

abstractify :: String -> Map String MonkeyOp -> Math
abstractify "humn" _ = Variable "humn"
abstractify monkey ms = case thisOp of
    ImmediateMonkey x -> Number x
    DelayMonkey a b '+' -> Bin Add (abstractify a ms) (abstractify b ms)
    DelayMonkey a b '-' -> Bin Sub (abstractify a ms) (abstractify b ms)
    DelayMonkey a b '*' -> Bin Mul (abstractify a ms) (abstractify b ms)
    DelayMonkey a b '/' -> Bin Div (abstractify a ms) (abstractify b ms)
    DelayMonkey {} -> error "unknown monkey operation"
  where
    thisOp = ms ! monkey

parseMonkey :: String -> (String, MonkeyOp)
parseMonkey s = (name, monkeyOp)
  where
    splot = splitOn ": " s
    name = head splot
    op = last splot
    opWords = words op
    monkeyOp
        | isDigit (head op) = ImmediateMonkey $ read op
        | otherwise = DelayMonkey
            (head opWords)
            (last opWords)
            (head $ opWords !! 1)
