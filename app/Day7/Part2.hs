module Day7.Part2 (thisMain) where

import           Data.Function
import qualified Data.List       as L
import           Data.List.Split
import           System.IO

type Name = String
data File = Dir Name [File] | Single Name Int deriving (Show)
data Elfcrumb = Elfcrumb Name [File] [File] deriving (Show)
type FileZipper = (File, [Elfcrumb])
data Command = Cd Name | Ls [File] deriving (Show)

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day7" ReadMode
    contents <- hGetContents handle
    let parsed = parseCommands contents
    let ran = cdRoot $ runCommands parsed
    let sizes = fst $ sizesAll (fst ran)
    let needToFree = 30000000 - (70000000 - maximum sizes)
    print $ head $ L.sortBy (compare `on` (subtract needToFree)) $ filter (> needToFree) $ sizes
    hClose handle

sizesAll :: File -> ([Int], Int)
sizesAll (Dir _ files) = (totalSize : (dirsSizes >>= fst), totalSize)
  where
    totalSize = sum singlesSizes + sum (snd <$> dirsSizes)
    dirsSizes = sizesAll <$> dirs
    singlesSizes = (\(Single _ s) -> s) <$> singles
    (dirs, singles) = L.partition
        (\f -> case f of
            Dir _ _ -> True
            _       -> False) files

runCommands :: [Command] -> FileZipper
runCommands cs = runCommandsZip cs (Dir "/" [], [])
  where
    runCommandsZip (Cd n : commands) fz = runCommandsZip commands $ case n of
        "/"  -> cdRoot fz
        ".." -> cdUp fz
        _    -> cdInto n fz
    runCommandsZip (Ls fs : commands) fz = runCommandsZip commands $ addLsInfo fs fz
    runCommandsZip [] fz = fz

addLsInfo :: [File] -> FileZipper -> FileZipper
addLsInfo newFiles ((Dir name []), crumbs) = (Dir name newFiles, crumbs)
addLsInfo _ fz                             = fz

cdUp :: FileZipper -> FileZipper
cdUp (file, (Elfcrumb crumbName start end) : crumbs) =
    (Dir crumbName (start ++ (file : end)), crumbs)

cdRoot :: FileZipper -> FileZipper
cdRoot fz@(_, []) = fz
cdRoot fz         = cdRoot $ cdUp fz

cdInto :: Name -> FileZipper -> FileZipper
cdInto n (Dir dirName files, crumbs) = (into, Elfcrumb dirName start end : crumbs)
  where
    start = take intoIdx files
    end = drop (intoIdx + 1) files
    into = files !! intoIdx
    (Just intoIdx) = L.findIndex ((== n) . filename) files

filename :: File -> String
filename (Dir n _)    = n
filename (Single n _) = n

parseCommands :: String -> [Command]
parseCommands s = parseCommandSingle <$> commandsStr
  where
    commandsStr = tail $ splitOn "$ " s
    parseCommandSingle ('c' : 'd' : ' ' : f) = Cd (init f)
    parseCommandSingle ('l' : 's' : '\n' : f) = Ls filesDone
      where
        files = lines f
        filesDone = groupFileLine . words <$> files
        groupFileLine (t : lf : [])
            | t == "dir" = Dir lf []
            | otherwise  = Single lf (read t)
