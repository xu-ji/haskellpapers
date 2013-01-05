
{- Initial imports -}
import Data.List
import Data.Char
import Data.Maybe

-- Unassessed Sheets - Jan/13

removeDuplicates :: Eq a=> [a] -> [a]
removeDuplicates xs = nub xs

splitAtIndex :: Eq a => Int -> [a] -> ([a], [a])
splitAtIndex n xs = splitAt n xs

type Vertex = (Float, Float, Float)

distance :: Vertex -> Vertex -> Float
distance (a1, a2, a3) (b1, b2, b3) = ((b1-a1)^2+(b2-a2)^2+(b3-a3)^2)**0.5

fact :: Int -> Int
--fact n = foldr1 (*) [n, n-1 ..1]
fact n = product [n, n-1 .. 1]

sugarEx :: String -> String
--sugarEx (xs) = reverse ( init (sort xs))
sugarEx (x:xs) = reverse $ init $ sort (x:xs)
sugarEx xs = (reverse . init . sort) xs

perm :: Int -> Int -> Int
perm n r = product [n, n-1 .. n-r+1]

--recursive functions always need a base case.
--recursion works here: define choose nCr in terms of (n-1)Cr
choose :: Int -> Int -> Int
--choose n r = div (perm n r) (fact r)
choose n r 
  | n == r    = 1
  | otherwise = div ((choose (n-1) r)*n) (n-r)

aPlusb :: [Int] -> [Int]
aPlusb (a:b:[]) = [a+b, 2*b+a]

fib :: Int -> Int
fib n = ((concat . take n . iterate aPlusb) [0, 1])!! n

stringToNum :: String -> Int
stringToNum n = read n :: Int 

--last returns a char, not a string. 
chop :: Int -> (Int, Int)
chop n = (read start :: Int, read end :: Int)
  where
    start = init (show n)
    end = last (show n):[]

chop2 :: Int -> (Int, Int)
chop2 n = (findFirst n, findLast n)

findFirst :: Int -> Int
findFirst n
  | (last . show) n == '0'  = read ((init . show) n) :: Int
  | otherwise               = findFirst (n -1)

findLast :: Int -> Int
findLast n 
  | n < 10    = n
  | otherwise = findLast (n -10)

splitString :: String -> String
splitString s = let (a, b) = splitAt ((length s) `div` 2) s in a

concatenate :: Int -> Int -> Int
concatenate n m = read ( (show n) ++ (show m) ) :: Int

pos :: Int -> [Int] -> Int
pos n ns = fromJust(elemIndex n ns)

twoSame :: [Int] -> Bool
twoSame (x:xs)
  | xs == [] = False
  | x `elem` xs = True
  | otherwise   = twoSame xs

removeDups :: [Int] -> [Int]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/=x) xs)

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2 
  where 
  primeFactors' :: Int -> Int -> [Int]
  primeFactors' n f 
    | f > n = []
    | (isPrime f) && (n `mod` f == 0) = f : primeFactors' (n `div` f) 2
    | otherwise = primeFactors' n (f+1)

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = not( any (==0) ( map (n `mod`) [2 .. findBound n] ) )

findBound :: Int -> Int
findBound = (ceiling . sqrt . fromIntegral)

hcf :: Int -> Int -> Int
hcf n m = product $ commonFactors (primeFactors n) (primeFactors m) 
  where 
  commonFactors :: [Int] -> [Int] -> [Int]
  commonFactors [] ms = []
  commonFactors (n:ns) ms
    | n `elem` ms = n : commonFactors ns (ms\\[n])
    | otherwise = commonFactors ns ms

lcm :: Int -> Int -> Int
lcm n m = product $ (primeFactors n)++((primeFactors m) \\ (primeFactors n))

backwards :: [a] -> [a]
backwards [] = []
backwards as = foldr plus [] as
  where
  plus :: a -> [a] -> [a]
  plus a as = as ++ [a]

transpose :: String -> String -> String -> String
transpose as bs cs = zipWith (!!) (replicate (length as) as) (indices bs cs)
  where
  indices :: String -> String -> [Int]
  indices [] cs     = []
  indices (b:bs) cs = (fromJust (elemIndex b cs)) : (indices bs cs)

substring :: String -> String -> Bool
substring sub word = or ( map (isPrefix sub) (suffixes word) ) 
  where
  suffixes :: String -> [String]
  suffixes word = take (length word) (iterate tail word)

  isPrefix :: String -> String -> Bool
  isPrefix [] word = True
  isPrefix (p:ps) (w:word)
    | p == w    = isPrefix ps word
    | otherwise = False

-- (start, end) not being passed string is fine - like on one whole line.
nextWord :: String -> (String, String)
nextWord string@(s:tring)
  | s == ' ' || s == '\t' || s == '\n' = nextWord tring
  | ' ' `elem` string || '\t' `elem` string || '\n' `elem` string = (start, tail (end))
  | otherwise = (string, [])
  where
  (start, end) = splitAt (firstIndex string) string 

firstIndex :: String -> Int
firstIndex string = minimum [(oneIndex ' ' string), (oneIndex '\t' string), (oneIndex '\n' string)]
-- can't use fromJust as not all the whitespace might be there.
oneIndex :: Char -> String -> Int
oneIndex c s = maybe (length s) id (elemIndex c s)

-- list comprehensions generate a new list from old one(s).
timesTable :: Int -> Int -> [String]
timesTable a b = [((show x) ++ " times " ++ (show y) ++ " is " ++ (show (x*y)))| x <- [1.. a], y <- [1 .. b]]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = (quicksort [n | n <-xs, n <=x]) ++ [x] ++ (quicksort [n | n <-xs, n > x])

-- routes takes edges, finish, start
routes :: [(Int, Int)] -> Int -> Int -> [[Int]]
routes rs f s 
  | s == f = [[s]]
  | otherwise = map (s:) (concatMap (routes rs f) (multiLookup s rs))

multiLookup :: Int -> [(Int, Int)] -> [Int]
multiLookup s rs = map snd (filter (\(a, b) -> a == s) rs)

infiniteFactorials :: [Int]
infiniteFactorials = scanl (*) (1) ([1, 2 ..])

same :: [Int] -> Bool
same (x:xs) = and (zipWith (==) (x:xs) xs)

-- do not be misled by names. FirstToSatisfy might be more accurate.
-- pre: list must be non-empty
converge :: (a -> a-> Bool) -> [a] -> a
converge f (x:[]) = x
converge f (x1:x2:xs)
  | f x1 x2   = x1
  | otherwise = converge f (x2:xs)

pipeline :: [a -> a] -> [a] -> [a]
pipeline [] as = as
pipeline (f:fs) as = pipeline fs (map f as)

pipeline2 :: [a -> a] -> [a] -> [a]
pipeline2 fs as = map (\a -> foldl apply a fs) as
  where
  apply :: a -> (a -> a) -> a
  apply a f = f a
