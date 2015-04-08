import Data.List
import System.IO

type Query = String
type Engine = String

runTillOutOfEngines :: ([Engine], [Query]) -> [Query]
runTillOutOfEngines ([], qs) = qs
runTillOutOfEngines (_, []) = []
runTillOutOfEngines (es, (q:qs)) = runTillOutOfEngines (newEs, newQs)
  where newEs = delete q es
        newQs = if null newEs
                then q:qs
                else qs

accumSwaps :: ([Engine], [Query], Int) -> ([Engine], [Query], Int)
accumSwaps (es, qs, n) = (es, qsLeft, newN)
  where qsLeft = runTillOutOfEngines (es, qs)
        newN = if null qsLeft then n else n+1 

countSwaps :: ([Engine], [Query]) -> Int
countSwaps (es, qs) = swaps
  where
    (_, _, swaps) = until queriesEmpty accumSwaps (es, qs, 0)
      where queriesEmpty (_, queries, _) = null queries

genCase :: ([([Engine], [Query])], [String]) -> ([([Engine], [Query])], [String])
genCase (accum, input) = (accum ++ [(engines, queries)], inputLeft)
  where
    numEngines = (read $ head input) :: Int
    numQueries = (read $ head (drop numEngines (tail input))) :: Int
    engines = take numEngines (tail input)
    queries = take numQueries (tail (drop numEngines (tail input)))
    inputLeft = drop numQueries (tail (drop numEngines (tail input)))
                               

genCases :: String -> [([Engine], [Query])]
genCases input = cases
  where inputLines = lines input
        numCases = (read $ head inputLines ) :: Int
        (cases, _) = until (\(_, input) -> null input) genCase ([], tail inputLines)

format :: (Int, Int) -> String
format (i, n) = "case #" ++ (show i) ++ ": " ++ (show n)

enumerate = zip [1..]

main = do
  inputString <- openFile "A-large-practice.in" ReadMode >>= hGetContents
  let cases = genCases inputString
  let swaps = map countSwaps cases
  mapM_ putStrLn $ map format (enumerate swaps)
  -- let numCases = 
  -- let swaps = countSwaps engines queries
  -- print swaps
