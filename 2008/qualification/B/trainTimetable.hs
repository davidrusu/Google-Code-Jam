import System.IO
import Data.List
import Debug.Trace

data Location = A | B

data Event = Event { time :: Time
                   , location :: Location }

data Case = Case { turnAroundTime :: Int
                 , tripsAB :: [Trip]
                 , tripsBA :: [Trip] } deriving (Eq)

data Trip = Trip { depart :: Time
                 , arrive :: Time } deriving (Eq)

data Time = Time { hour :: Int
                 , minute :: Int } deriving (Eq)

instance Ord Time where
  compare a b | hour a == hour b = compare (minute a) (minute b)
              | otherwise        = compare (hour a) (hour b)

instance Show Case where
  show c = unlines $ [ show $ turnAroundTime c
                     , show (length tAB) ++ " " ++ show (length tBA)
                     ] ++ (map show tAB) ++ (map show tBA)
    where tAB = tripsAB c
          tBA = tripsBA c

instance Show Trip where
  show (Trip {depart=d, arrive=a}) = show d ++ " " ++ show a

instance Show Time where
  show (Time {hour=h, minute=m}) = hString ++ ":" ++ mString
    where hString = (padding h) ++ (show h)
          mString = (padding m) ++ (show m)
          padding n | n < 10 = "0"
                    | otherwise = ""

instance Read Time where
  readsPrec _ (h1:h2:':':m1:m2:[]) = [(result, "")]
    where h = (read [h1, h2]) :: Int
          m = (read [m1, m2]) :: Int
          result = Time { hour = h, minute = m }

instance Read Trip where
  readsPrec _ s = [(trip, "")]
    where times = (map read $ words s) :: [Time]
          [d, a] = times
          trip = Trip { depart = d
                      , arrive = a }

getCase :: ([Case], [String]) -> ([Case], [String])
getCase (cases, input) = (cases ++ [result], drop (2+numA+numB) input)
  where [numA, numB] = map read $ words (input !! 1) :: [Int]
        tripAInput = take numA $ drop 2 input 
        tripBInput = take numB $ drop (2 + numA) input
        result = Case { turnAroundTime = read $ head input
                      , tripsAB = map read tripAInput
                      , tripsBA = map read tripBInput }

getCases :: [String] -> [Case]
getCases input = cases
  where numCases = (read $ head input) :: Int
        casesInput = tail input
        (cases, _) = until (null . snd) getCase ([], tail input)
        

parseEvents :: ([Event], String) -> ([Event], String)
parseEvents (events, input) =
  where

parseCase :: [String] -> (Case, [String])
parseCase input =
  where turnTime = read $ head input
        [nA, nB] = map read $ words (input !! 1) :: [Int]
        tripsA = map read $ take nA $ drop 2 input :: [Trip]
        tripsB = map read $ take nB $ drop (2 + nA) input :: [Trip]
        
    
parseCases :: ([Case], [String]) -> ([Case], [String])
parseCases (cases, input) = undefined

---- Ok we've parsed the input ----

solveCase :: Case -> (Int, Int)
solveCase c = undefined
  where sortedA = sort $ map depart (tripsAB c)

main = do
  input <- openFile "test.in" ReadMode >>= hGetContents >>= return . lines
  let cases = getCases input
  mapM_ putStr $ map show cases
