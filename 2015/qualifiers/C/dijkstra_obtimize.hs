import System.IO
import Data.List
import Debug.Trace

enumerate = zip [0..]

discatenate :: [a] -> [[a]]
discatenate xs = map (\a -> [a]) xs

format :: (Int, Bool) -> String
format (i, True) = "Case #" ++ (show (i + 1)) ++ ": Yes"
format (i, False) = "Case #" ++ (show (i + 1)) ++ ": No"

data Var = One | I | J | K | Neg Var deriving (Eq, Show)

-- data Expr = One Expr Expr | I Expr Expr | J Expr Expr | K Expr Expr | Neg Expr Expr Expr

data Quad = Node Var Var Quad Quad Quad Quad deriving (Show)

tree :: Var -> Var -> Quad
tree root product = Node root product (tree One prod1) (tree I prodi) (tree J prodj) (tree K prodk)
  where [prod1, prodi, prodj, prodk] = map (mult product) [One, I, J, K]

--tree root product  = ((prod1, tree One prod1), (prodi, tree I prodi), (prodj, tree J prodj), (prodk, tree K prodk))
--  where [prod1, prodi, prodj, prodk] = map (mult product) [One, I, J, K]

evalTree :: (Quad, Quad, Quad, Quad)
evalTree = (tree One One, tree I I, tree J J, tree K K)

simplify :: Var -> Var
simplify v = simplified
 where
   (_, simplified) = until (\(old, new) -> old == new) (\(old, new) -> (new, simple new)) (Neg v, v)
   simple (Neg (Neg b)) = b
   simple other = other

mult One other = other
mult other One = other
mult I I       = Neg One
mult J J       = Neg One
mult K K       = Neg One
mult I J       = K
mult J I       = Neg K
mult I K       = Neg J
mult K I       = J
mult J K       = I
mult K J       = Neg I
mult (Neg a) b = Neg (mult a b)
mult a (Neg b) = Neg (mult a b)

-- reduce :: Expr -> Var
-- reduce (Leaf ls) = simplify $ foldr mult One ls
-- reduce (Node ns) = reduce (Leaf $ map reduce ns)

showQuad :: Quad -> String
showQuad (Node v p _ _ _ _) = "Root: " ++ (show v) ++ " Product: " ++ (show p) 

reduce :: [Var] -> Var
reduce xs = result
  where (branch1, branchI, branchJ, branchK) = evalTree
        explore (Node _ _ x _ _ _) One = x
        explore (Node _ _ _ x _ _) I   = x
        explore (Node _ _ _ _ x _) J   = x
        explore (Node _ _ _ _ _ x) K   = x

        quad = case head xs of
                One -> branch1
                I   -> branchI
                J   -> branchJ
                K   -> branchK
        (_, Node _ r _ _ _ _) = until (\(input, _) -> null input) (\(x:rest, q) -> (rest, explore q x)) (tail xs, quad)
        result = simplify r

-- reduce :: [Var] -> Var
-- reduce ls = simplify $ foldr mult One ls


check :: ([Var], [Var], [Var]) -> Bool
check (xs, ys, zs) = (reduce xs == I) && (reduce ys == J) && (reduce zs == K)

part :: [Var] -> (Int, Int) -> ([Var], [Var], [Var])
part vars (n, m) = (xs, ys, zs)
  where (xs, rest) = splitAt n vars
        (ys, zs) = splitAt (m-n) rest

-- 1 < x < y < length
sections :: Int -> Int -> [(Int, Int)]
sections x l | x > l - 2 = []
             | otherwise = (zip (repeat x) [x+1..l-1]) ++ (sections (x + 1) l)

genParts :: [Var] -> [([Var], [Var], [Var])]
genParts xs = map (part xs) (sections 1 (length xs))

-- toExpr ('1':xs) prev = (One (Mult prev One)) : (toExpr xs One)
-- toExpr ('i':xs) prev = (I (Mult prev I)) : (toExpr xs I)
-- toExpr ('j':xs) prev = (J (Mult prev J)) : (toExpr xs J)
-- toExpr ('k':xs) prev = (K (Mult prev K)) : (toExpr xs K)

toVar '1' = One
toVar 'i' = I
toVar 'j' = J
toVar 'k' = K

solveCase :: ([Bool], [String]) -> ([Bool], [String])
solveCase (results, input) = (results ++ [result], drop 2 input)
  where [size, reps] = map read $ words (head input)  :: [Int]
        expr = map toVar $ input !! 1
        expression = concat $ replicate reps expr
        result = any check $ genParts expression

solveCases :: [String] -> [Bool]
solveCases input = result
  where (result, _) = until (\(_, is) -> null is) solveCase ([], input)
    
main = do
  input <- openFile "d.in" ReadMode >>= hGetContents >>= return . lines

  let numCases = read $ head input :: Int
  let solved = solveCases $ tail input
  mapM_ putStrLn $ map format (enumerate solved)
