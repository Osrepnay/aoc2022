module Day19.Part1 (thisMain) where

import System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day19" ReadMode
    contents <- hGetContents handle
    let blueprints = parseBlueprint <$> lines contents
    print
        $ sum
        $ (\bp -> blueprintId bp
            * blueprintGeodes
                (Resources 0 0 0 0)
                (Resources 1 0 0 0)
                24
                bp)
        <$> blueprints
    hClose handle

data Resources = Resources
    { resOre   :: Int
    , resClay  :: Int
    , resObby  :: Int
    , resGeode :: Int
    } deriving (Show)

resAdd :: Resources -> Resources -> Resources
resAdd (Resources aa ab ac ad) (Resources ba bb bc bd) = Resources
    (aa + ba)
    (ab + bb)
    (ac + bc)
    (ad + bd)

resSub :: Resources -> Resources -> Resources
resSub (Resources aa ab ac ad) (Resources ba bb bc bd) = Resources
    (aa - ba)
    (ab - bb)
    (ac - bc)
    (ad - bd)

resValid :: Resources -> Bool
resValid (Resources a b c d) = a >= 0 && b >= 0 && c >= 0 && d >= 0

data Blueprint = Blueprint
    { blueprintId :: Int
    , oreCost     :: Resources
    , clayCost    :: Resources
    , obbyCost    :: Resources
    , geodeCost   :: Resources
    , maxCosts    :: Resources
    } deriving (Show)

blueprintGeodes :: Resources -> Resources -> Int -> Blueprint -> Int
blueprintGeodes res robots mins bp
    | mins == 0 = resGeode res
    | resValid madeGeodeRes = blueprintGeodes
        (robotWork madeGeodeRes)
        madeGeodeRob
        (mins - 1)
        bp
    | otherwise = maximum
        $ (\(nres, nrob) -> blueprintGeodes (robotWork nres) nrob (mins - 1) bp)
        <$> (res, robots)
            :
                [ (madeOreRes, madeOreRob)
                | resValid madeOreRes
                , resOre madeOreRob <= resOre (maxCosts bp)
                ]
            ++
                [ (madeClayRes, madeClayRob)
                | resValid madeClayRes
                , resClay madeClayRob <= resClay (maxCosts bp)
                ]
            ++
                [ (madeObbyRes, madeObbyRob)
                | resValid madeObbyRes
                , resObby madeObbyRob <= resObby (maxCosts bp)
                ]
  where
    robotWork = resAdd robots
    madeOreRes = res `resSub` oreCost bp
    madeOreRob = robots `resAdd` Resources 1 0 0 0
    madeClayRes = res `resSub` clayCost bp
    madeClayRob = robots `resAdd` Resources 0 1 0 0
    madeObbyRes = res `resSub` obbyCost bp
    madeObbyRob = robots `resAdd` Resources 0 0 1 0
    madeGeodeRes = res `resSub` geodeCost bp
    madeGeodeRob = robots `resAdd` Resources 0 0 0 1

parseBlueprint :: String -> Blueprint
parseBlueprint s = Blueprint
    (read $ init $ worded !! 1)
    ore
    clay
    obby
    geode
    (Resources
        (maximum [resOre ore, resOre clay, resOre obby, resOre geode])
        (resClay obby)
        (resObby geode)
        0)
  where
    worded = words s
    ore = Resources (read (worded !! 6)) 0 0 0
    clay = Resources (read (worded !! 12)) 0 0 0
    obby = Resources (read (worded !! 18)) (read (worded !! 21)) 0 0
    geode = Resources (read (worded !! 27)) 0 (read (worded !! 30)) 0
