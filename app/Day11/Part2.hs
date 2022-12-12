module Day11.Part2 (thisMain) where

import           Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as I
import qualified Data.List        as L
import           Data.List.Split
import           Data.Sequence    (Seq (..), (|>))
import qualified Data.Sequence    as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day11" ReadMode
    contents <- hGetContents handle
    let monkeys =
            I.fromList
            $ zip [0..]
            $ parseMonkey . lines
            <$> splitOn "\n\n" contents
    let lickem = L.foldl' lcm 1 (monkeyTest <$> monkeys)
    let afterRounds = iterate (runMonkeys lickem) monkeys !! 10000
    print
        $ product
        $ take 2
        $ reverse
        $ L.sort
        $ monkeyThrows
        . snd
        <$> (I.toList afterRounds)
    hClose handle

data Monkey = Monkey
    { monkeyNumber  :: Int
    , monkeyItems   :: Seq Int
    , monkeyOp      :: Int -> Int
    , monkeyTest    :: Int
    , monkeyTTarget :: Int
    , monkeyFTarget :: Int
    , monkeyThrows  :: Int
    }

runMonkeys :: Int -> IntMap Monkey -> IntMap Monkey
runMonkeys modless monkeys = L.foldl'
    (\b a -> runMonkey modless (b ! a) b)
    monkeys
    [0..I.size monkeys - 1]

runMonkey :: Int -> Monkey -> IntMap Monkey -> IntMap Monkey
runMonkey modless (Monkey num (item :<| is) op test tt ft t) monkeys = runMonkey
    modless
    newMonkey
    (I.insert num newMonkey (addItemMonkey newItem target monkeys))
  where
    newMonkey = Monkey num is op test tt ft (t + 1)
    newItem = op item `rem` modless
    target = if newItem `rem` test == 0 then tt else ft
runMonkey _ (Monkey _ Empty _ _ _ _ _) monkeys = monkeys

addItemMonkey :: Int -> Int -> IntMap Monkey -> IntMap Monkey
addItemMonkey item monkey monkeys = I.insert
    monkey
    (oldMonkey { monkeyItems = monkeyItems oldMonkey |> item })
    monkeys
  where
    oldMonkey = monkeys ! monkey

parseMonkey :: [String] -> Monkey
parseMonkey ss = Monkey
    (read $ (: []) $ (!! 7) $ head ss)
    (S.fromList $ read <$> splitOn ", " (splitOn ": " (ss !! 1) !! 1))
    (\x ->
        let val  = if last opWords == "old" then x else read (last opWords)
         in
            case opWords !! 4 of
                "+" -> x + val
                "*" -> x * val
                _   -> error "unknown operation")
    (read $ last $ words $ ss !! 3)
    (read $ last $ words $ ss !! 4)
    (read $ last $ words $ ss !! 5)
    0
  where
    opWords = words $ ss !! 2
