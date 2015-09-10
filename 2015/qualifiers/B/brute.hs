import System.IO
import Data.List
import Debug.Trace

enumerate = zip [0..]

discatenate :: [a] -> [[a]]
discatenate xs = map (\a -> [a]) xs

format :: (Int, Int) -> String
format (i, n) = "Case #" ++ (show (i + 1)) ++ ": " ++ (show n)

rsort :: (Ord a) => [a] -> [a]
rsort = reverse . sort

split :: [Int] -> Int -> [Int]
split [] _ = []
split (x:xs) 0 = let small = x `div` 2
                     big = x - small
                 in big : small : xs
split (x:xs) n = x : (split xs (n - 1))

advance :: [Int] -> [Int]
advance xs = map (\x -> x - 1) xs

removeFinished :: [Int] -> [Int]
removeFinished = filter (>0)

step :: (Int, [[Int]]) -> (Int, [[Int]])
step (count, xss) = (count + 1, newXss)
  where
    len = length xss
    indices = [0..(len-1)]
    splits = nub $ concatMap (\xs -> map (rsort . (split xs)) [0..(length xs)]) xss
    advances = map advance xss
    together = map removeFinished $ splits ++ advances
    newXss = if any null together
             then []
             else together

    
solveCase :: ([Int], [String]) -> ([Int], [String])
solveCase (results, input) = (results ++ [result], drop 2 input)
  where num = read $ head input :: Int
        plates = rsort $ map read $ words (input !! 1) :: [Int]
        (result, _) = until (\(_, xs) -> null xs) step (0, [plates])
        
solveCases :: [String] -> [Int]
solveCases input = result
  where (result, _) = until (\(_, is) -> null is) solveCase ([], input)
    
main = do
  input <- openFile "d.in" ReadMode >>= hGetContents >>= return . lines

  let numCases = read $ head input :: Int
  let solved = solveCases $ tail input
  mapM_ putStrLn $ map format (enumerate solved)  
  
