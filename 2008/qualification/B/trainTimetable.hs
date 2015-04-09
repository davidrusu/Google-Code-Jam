import System.IO
import Data.List
import Debug.Trace

data Case = Case { turnAroundTime :: Int
                 , tripsAB :: [Trip]
                 , tripsBA :: [Trip] } deriving (Eq)

data Trip = Trip { depart :: Time
                 , arrive :: Time } deriving (Eq)

data Time = Time { hour :: Int
                 , minute :: Int } deriving (Eq)

instance Show Case where
  show (Case {turnAroundTime=t, tripsAB=tab, tripsBA=tba}) = unlines $ [ show t
                                                                       , show (length tab) ++ " " ++ show (length tba)
                                                                       ] ++ (map show tab) ++ (map show tba)

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
        
        result = Case { turnAroundTime = (read $ head input)
                      , tripsAB = (map read $ tripAInput)
                      , tripsBA = (map read $ tripBInput) }

getCases :: [String] -> [Case]
getCases input = cases
  where numCases = (read $ head input) :: Int
        casesInput = tail input
        (cases, _) = until (null . snd) getCase ([], tail input)
        

main = do
  input <- openFile "test.in" ReadMode >>= hGetContents >>= return . lines
  let cases = getCases input
  mapM_ putStr $ map show cases
