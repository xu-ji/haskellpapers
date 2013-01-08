import Data.List
import Data.Char
import Data.Maybe

data Tree = Leaf Int | Node Tree Tree
  deriving (Eq, Ord, Show)


buildTree :: [Int] -> Tree
buildTree xs = foldl1 merge (map (\x -> Leaf x) (sort xs))
  where
  merge :: Tree -> Tree -> Tree
  merge (Leaf n) (Leaf x) = Node (Leaf n) (Leaf x)
  merge (Leaf n) (Node t1 t2)
    | greater t2 t1 = Node (merge (Leaf n) t1) t2
    | otherwise     = Node t1 (merge (Leaf n) t2)
  merge (Node t1 t2) (Leaf n)
    | greater t2 t1 = Node (merge (Leaf n) t1) t2
    | otherwise     = Node t1 (merge (Leaf n) t2)
  merge (Node t1 t2) (Node t3 t4) = Node (Node t1 t2) (Node t3 t4)

  greater :: Tree -> Tree -> Bool
  greater t1 t2 = numLeaves t1 >= numLeaves t2

  numLeaves :: Tree -> Int
  numLeaves (Leaf n) = 1
  numLeaves (Node t1 t2) = (numLeaves t1) + (numLeaves t2)

-- traverse base of tree in order
toList :: Tree -> [Int]
toList (Leaf n) = [n]
toList (Node t1 t2) = (toList t1) ++ (toList t2)

swap :: Tree -> Tree
swap (Leaf n) = (Leaf n)
swap (Node t1 t2) = Node (swap t2) (swap t1)

data BTree = BLeaf | BNode BTree BTree
  deriving (Eq, Ord, Show)

makeTrees :: Int -> [BTree]
makeTrees 0 = [BLeaf]
makeTrees n = nub (concatMap addOneNode (makeTrees (n-1)))

addOneNode :: BTree -> [BTree]
addOneNode (BLeaf) = [(BNode BLeaf BLeaf)]
addOneNode (BNode t1 t2) =  addOne' (BNode t1 t2)
  where
  addOne' :: BTree -> [BTree] 
  addOne' (BLeaf) = [(BNode BLeaf BLeaf)]
  addOne' tree = addOneLeft tree ++ addOneRight tree

  addOneLeft :: BTree -> [BTree]
  addOneLeft (BLeaf) = [BNode BLeaf BLeaf]
  addOneLeft (BNode t1 t2) = map (onToRightNode t2) (addOne' t1)

  addOneRight :: BTree -> [BTree]
  addOneRight (BLeaf) = [BNode BLeaf BLeaf]
  addOneRight (BNode t1 t2) = map (onToLeftNode t1) (addOne' t2)

onToRightNode :: BTree -> BTree -> BTree
onToRightNode t2 t1 = BNode t1 t2

onToLeftNode :: BTree -> BTree -> BTree
onToLeftNode t1 t2 = BNode t1 t2

