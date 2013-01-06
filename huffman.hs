--
--	SKELETON: Haskell_HuffmanCode.
--

module Exam where	-- (do not alter this line; it allows this file
			--  to be imported into another for testing purposes)

import Data.List hiding (insert)

data Huffman a = Tip Int a | Node Int ( Huffman a ) ( Huffman a )
                 deriving (Show)

fcount :: Huffman a -> Int
fcount ( Tip n x ) = n
fcount ( Node n t1 t2 ) = n

instance Eq a => Eq ( Huffman a ) where
  t == t' = fcount t == fcount t'
instance Ord a => Ord ( Huffman a ) where
  t <= t'  = fcount t <= fcount t'

data Step = L | R
            deriving (Eq, Ord, Show)
type Path = [Step]

--
-- example Huffman code tree and test string from the spec.
--
t1 :: Huffman Char
t1 = Node 21 (Tip 9 't') (Node 12 (Node 5 (Tip 2 'k') (Tip 3 'c')) (Tip 7 'a'))

testString :: String
testString = "aatatttactaccattatktk"

------------------------------------------------------------------------
-- Write the functions below, uncommenting the headers as you go along.
------------------------------------------------------------------------
--zipWith, filter and foldr!
-- Pre: The list is ordered
sInsert :: Ord a => a -> [ a ] -> [ a ]
sInsert n ns = let (less, more) = span (<n) ns in less++[n]++more

insert :: Ord a => a -> [ a ] -> [ a ]
insert x [] = [x]
insert x (n:ns)
  | x <= n = x:n:ns
  | otherwise = n: (insert x ns)
 
count :: Eq a => a -> [ a ] -> ( a, Int )
count n ns = (n, length (filter (==n) ns))

countAll :: Eq a => [ a ] -> [ a ] -> [ ( a, Int ) ]
countAll source chosen = map (\a -> count a source) chosen 

table :: Eq a => [ a ] -> [ ( a, Int ) ]
table s = nub(countAll s s)
-- Pre: t1 and t2 are both well formed
-- Pre: t1 <= t2
merge :: Huffman a -> Huffman a -> Huffman a
merge t1 t2 = Node (fcount t1 + fcount t2) t1 t2

-- Pre: The list is non-empty and already sorted
-- Pre: Each tree in the list is well formed
reduce :: Ord a => [ Huffman a ] -> Huffman a
reduce [t] = t
reduce (t1:t2:ts) = reduce (insert (merge t1 t2) ts)

buildTree :: ( Eq a, Ord a ) => [ a ] -> Huffman a
buildTree as = reduce(sort (map (\(a, n) -> Tip n a) (table as)))

--last attempt, by looking at frequency count, is FLAWED! You can't decide whether to go left or right based on frequency count.
-- look at a itself and the elements down each side of the tree - this is what you care about.
--sometimes non-Exhaustive patterns occur because of misnaming a function.
encode :: Eq a => [ a ] -> Huffman a -> Path
encode as tree = concatMap (\a -> encode' a tree) as
  where 
  encode' :: Eq a => a -> Huffman a -> Path
  encode' a (Tip n x)
    | a == x = []
    | otherwise = error "mismatch."
  encode' a (Node n l r)
    | a `elem` (findAll l) = L : (encode' a l)
    | otherwise = R : (encode' a r)

findAll :: Huffman a -> [a]
findAll (Tip n x) = [x]
findAll (Node n l r) = (findAll l)++(findAll r)


--be SUPER CAREFUL - common mistake is mixing up the original and helper function in the helper.
-- the base case will always be at a Tip.
decode :: Eq a => Path -> Huffman a -> [ a ]
decode path tree = decode' tree path tree
  where
  decode' :: Huffman a -> Path -> Huffman a -> [a]
  decode' (Tip n x) [] _ = [x]
  decode' (Tip n x) ps tree = x : decode' tree ps tree
--  decode' _ [] _ = [] <- or, could have put base case here after last tip is found.
  decode' (Node n l r) (p:ps) tree
    | p == L = decode' l ps tree
    | p == R = decode' r ps tree
