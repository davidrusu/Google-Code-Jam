import System.IO
import Data.List

enumerate = zip [0..]

discatenate :: [a] -> [[a]]
discatenate xs = map (\a -> [a]) xs

parseAud :: String -> [(Int, Int)]
parseAud aud = enumerate $ (map read (discatenate aud) :: [Int])

run :: ([(Int, Int)],  Int, Int) -> ([(Int, Int)], Int, Int)
run ([], up, extras) = ([], up, extras)
run (((thresh, num):xs), up, extras)
  | up >= thresh = run (xs, (up + num), extras)
  | otherwise    = ((thresh, num):xs, up + newExtras, extras + newExtras)
  where newExtras = thresh - up

solveCase :: String -> Int
solveCase caseString = extras
  where [maxS, aud] = words caseString
        startAud = parseAud aud
        (_, _, extras) = until (\(notStanding, _, _) -> null notStanding ) run (startAud, 0, 0)

format :: (Int, Int) -> String
format (i, n) = "Case #" ++ (show (i + 1)) ++ ": " ++ (show n)

main = do
  input <- openFile "a.in" ReadMode >>= hGetContents >>= return . lines

  let numCases = read $ head input :: Int
  let solved = map solveCase $ tail input
  mapM_ putStrLn $ map format (enumerate solved)
