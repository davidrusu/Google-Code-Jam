import System.IO
import Data.List
import Debug.Trace

enumerate = zip [0..]

discatenate :: [a] -> [[a]]
discatenate xs = map (\a -> [a]) xs

format :: (Int, Int) -> String
format (i, n) = "Case #" ++ (show (i + 1)) ++ ": " ++ (show n)

rsort = reverse . sort

split :: [Int] -> Int -> [Int]
split [] n = []
split (x:xs) n = rsort $ big : small : xs
  where small = x `div` n
        big = x - small

costOfSplit :: [Int] -> Int
costOfSplit plates = go plates 0
  where go :: [Int] -> Int -> Int
        go ps currUtility = if maximum ps <= 3  || utility > 0
                            then utility
                            else go splitted utility
          where splitted = map (split ps) [2..(head ps `div` 2)]
                utility = (maximum ps) - (maximum splitted) - 1 + currUtility

minutes :: [Int] -> Int
minutes plates@(x:xs) =
  where splits = map (split ps) [2..(x `div` 2)]
        

solveCase :: ([Int], [String]) -> ([Int], [String])
solveCase (results, input) = (results ++ [result], drop 2 input)
  where num = read $ head input :: Int
        plates = rsort $ map read $ words (input !! 1) :: [Int]

        splitted = until (\ps -> costOfSplit ps <= 0) split plates
        result = (length splitted) - (length plates) + (maximum splitted)

solveCases :: [String] -> [Int]
solveCases input = result
  where (result, _) = until (\(_, is) -> null is) solveCase ([], input)
    
main = do
  input <- openFile "d.in" ReadMode >>= hGetContents >>= return . lines

  let numCases = read $ head input :: Int
  let solved = solveCases $ tail input
  mapM_ putStrLn $ map format (enumerate solved)
