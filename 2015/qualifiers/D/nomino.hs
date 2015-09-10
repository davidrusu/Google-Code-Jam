import System.IO
import Data.List
import Debug.Trace

data Cell = Node Cell Cell Cell Cell | Leaf


appendCell :: Cell -> Cell -> Loc
appendCell new cell 

enumerate = zip [0..]

discatenate :: [a] -> [[a]]
discatenate xs = map (\a -> [a]) xs

format :: (Int, Int) -> String
format (i, n) = "Case #" ++ (show (i + 1)) ++ ": " ++ (show n)

rsort = reverse . sort

nomino :: Int -> 
        
main = do
  input <- openFile "c.in" ReadMode >>= hGetContents >>= return . lines
  let numCases = read $ head input :: Int
  let solved = solveCases $ tail input
  mapM_ putStrLn $ map format (enumerate solved)
