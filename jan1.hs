import Data.List

data SuffixTree = Leaf Int | Node [ ( String, SuffixTree ) ] 
                deriving (Eq,Ord,Show)

text1 :: String
text1 = "banana"

text2 :: String
text2 = "mississippi"

tree1 :: SuffixTree
tree1 = Node [("banana",Leaf 0),
              ("a",Node [("",Leaf 5),
                         ("na",Node [("",Leaf 3),
                                     ("na",Leaf 1)])]),
              ("na",Node [("",Leaf 4),
                          ("na",Leaf 2)])]

tree2 :: SuffixTree
tree2 = Node [("mississippi",Leaf 0),
              ("i",Node [("",Leaf 10),
                         ("ssi",Node [("ssippi",Leaf 1),
                                      ("ppi",Leaf 4)]),
                            ("ppi",Leaf 7)]),
              ("s",Node [("si",Node [("ssippi",Leaf 2),
                                     ("ppi",Leaf 5)]),
              ("i",Node [("ssippi",Leaf 3),
                         ("ppi",Leaf 6)])]),
              ("p",Node [("pi",Leaf 8),
                         ("i",Leaf 9)])]

----------------------------------------------------------

--PART I
-- [] == ""
isPrefix :: String -> String -> Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys)
  | x == y = isPrefix xs ys
  | otherwise = False

--pre: xs is a prefix of ys
removePrefix :: String -> String -> String
removePrefix xs = drop (length xs)

removePrefix2 :: Eq a => [a] -> [a] -> [a]
removePrefix2 xs = drop (length xs)

suffixes :: [ a ] -> [ [ a ] ]
suffixes [] = [[]]
suffixes xs = take (length xs) (iterate tail xs) 

prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes xs = take (length xs) (iterate init xs)


isSubstring :: String -> String -> Bool
isSubstring xs ys = or (map (\ as -> isPrefix xs as) (suffixes ys))

findSubstrings :: String -> String -> [ Int ]
findSubstrings xs ys = elemIndices True (map (\as -> isPrefix xs as) 
                       (suffixes ys)) 

findSubstrings2 :: String -> String -> [Int]
findSubstrings2 xs ys = [n | n <- [0..(length ys -1)], 
                        isPrefix xs ((suffixes ys)!!n)]

--PART II

-- can ignore: getIndices Leaf n = [n] as we will never have just a leaf, and just use getIndices (Node ((s, Leaf n):rest))= n: (getIndices (Node rest))
-- but specifying Leaf n is neater.
-- must differentiate between [(String, SuffixTree)] form and Node [(String, SuffixTree)].
-- must always use brackets around pattern matching trees, or compiler will interpret as 2 seperate arguments. 
getIndices :: SuffixTree -> [ Int ]
getIndices (Node []) = []
getIndices (Leaf n) = [n]
getIndices (Node ((s, t):rest))= (getIndices t) ++ getIndices (Node rest)

getIndices2 :: SuffixTree -> [Int]
getIndices2 (Leaf n) = [n]
getIndices2 (Node trees) = concatMap getIndices2 (map snd trees)

-- partitions by largest prefix
partition :: Eq a => [ a ] -> [ a ] -> ( [ a ], [ a ], [ a ] )
partition as bs = (x, (removePrefix2 x as), (removePrefix2 x bs))
  where
    x = if zs == [] then [] else head(zs) 
    zs = [z | z <- prefixes as, y <- prefixes bs, z==y]

-- second case will never happen on its own, leaf does not contain a string. However, it's a base case.
-- second call for isPrefix - searching haskellha for string "happy". "ha" matches. Then search kellha for "ppy" to find whole substring (does not find in this case).
-- c is the rest of the substring we are looking for. 
findSubstringsInTree :: String -> SuffixTree -> [ Int ]
findSubstringsInTree x (Node []) = []
findSubstringsInTree x (Leaf n) = [n]
findSubstringsInTree x (Node ((s, t):rest))
  | isPrefix x s = (getIndices t) ++ (findSubstringsInTree x (Node rest))
  | isPrefix s x = if (s == []) then 
      findSubstringsInTree x (Node rest) else 
      (findSubstringsInTree c t) ++ (findSubstringsInTree x (Node rest))
  | otherwise = findSubstringsInTree x (Node rest)
  where
    (a, b, c) = Main.partition s x

-- not and /= are used in Haskell.
-- must differentiate between SuffixTree and [(String, SuffixTree)]
-- tree is a SuffixTree, rest is a list
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node []) = Node[(s, Leaf n)]
insert (s, n) (Node ((a, tree): rest))
  | p == [] = Node ((a, tree): deNode(Main.insert (s, n) (Node rest)))
  | p == a  = Node ((a, (Main.insert (s1, n) tree)):rest)
  | p /= a = Node ((p, Node[(s1, Leaf n), (a1, tree)]):rest)
    where 
      (p, s1, a1) = Main.partition s a

deNode :: SuffixTree -> [(String, SuffixTree)]
deNode (Node list) = list

--This function is given
buildTree :: String -> SuffixTree 
buildTree s
 = foldl ( flip Main.insert ) ( Node [] ) ( zip ( suffixes s ) [0..length s-1] )

--longestRepeatedSubstringInTree :: SuffixTree -> String


