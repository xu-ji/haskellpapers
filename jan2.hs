import Data.Maybe
import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | OpApp Op BExp BExp
            deriving (Eq,Ord,Show)

data Op = And | Or 
          deriving (Eq,Ord,Show)

type Env = [ ( Index, Bool ) ]

type Id = Int

type BDDNode = ( Id, ( Index, Id, Id ) )

type BDD = [ BDDNode ]

b1,b2,b3,b4,b5,b6,b7,b8 :: BExp
b1 = Prim False
b2 = IdRef 4
b3 = OpApp And ( IdRef 1 ) ( Prim True )
b4 = OpApp And ( IdRef 7 ) ( OpApp Or ( IdRef 2 ) ( Not ( IdRef 3 ) ) )
b5 = Not ( OpApp And ( IdRef 7 ) ( OpApp Or ( IdRef 2 ) ( Not ( IdRef 3 ) ) ) )
b6 = OpApp Or ( OpApp And ( IdRef 1 ) ( IdRef 2 ) )
              ( OpApp And ( IdRef 3 ) ( IdRef 4 ) )
b7 = OpApp Or ( Not ( IdRef 3 ) ) ( OpApp Or ( IdRef 2 ) ( Not ( IdRef 9 ) ) )
b8 = OpApp Or ( IdRef 1 ) ( Not ( IdRef 1 ) )
b9 = OpApp Or ( OpApp And ( IdRef 1 ) ( IdRef 2 ) )
              ( IdRef 3 )


--There is no bbd1: recall that the BDD type above requires there to
--be at least one variable reference!
bdd2,bdd3,bdd4,bdd5,bdd6,bdd7,bdd8, bdd9 :: BDD
bdd2 = [(1,(4,-1,-2))]
bdd3 = [(1,(1,-1,-2))]
bdd4 = [(1,(2,2,3)),(2,(3,4,5)),(4,(7,-1,-2)),(5,(7,-1,-1)),
        (3,(3,6,7)),(6,(7,-1,-2)),(7,(7,-1,-2))]
bdd5 = [(1,(2,2,3)),(2,(3,4,5)),(4,(7,-2,-1)),(5,(7,-2,-2)),
        (3,(3,6,7)),(6,(7,-2,-1)),(7,(7,-2,-1))]
bdd6 = [(1,(1,2,3)),(2,(2,4,5)),(4,(3,8,9)),(8,(4,-1,-1)),
        (9,(4,-1,-2)),(5,(3,10,11)),(10,(4,-1,-1)),(11,(4,-1,-2)),
        (3,(2,6,7)),(6,(3,12,13)),(12,(4,-1,-1)),(13,(4,-1,-2)),
        (7,(3,14,15)),(14,(4,-2,-2)),(15,(4,-2,-2))]
bdd7 = [(1,(2,2,3)),(2,(3,4,5)),(4,(9,-2,-2)),(5,(9,-2,-1)),
        (3,(3,6,7)),(6,(9,-2,-2)),(7,(9,-2,-2))]
bdd8 = [(1,(1,-2,-2))]
bdd9 = [(1,(1,2,3)),(2,(2,4,5)),(4,(3,-1,-2)),(5,(3,-1,-2)),
        (3,(2,6,7)),(6,(3,-1,-2)),(7,(3,-2,-2))]


------------------------------------------------------
-- PART I
-- on BExps
--Pre: The item is in the given table
lookUp :: Eq a => a -> [ ( a, b ) ] -> b
lookUp a = fromJust . (lookup a)

-- hask: /, not, &&, ||  
-- ops And and Or are both binary
eval :: BExp -> Env -> Bool
eval (Prim b) _     = b
eval (IdRef x) e    = lookUp x e
eval (Not exp) e    = not (eval exp e)
eval (OpApp op exp1 exp2) e
  | op == And       = (eval exp1 e) && (eval exp2 e) 
  | op == Or        = (eval exp1 e) || (eval exp2 e)

test1 = eval (Prim False) [(1,False),(2,True)]
test2 = eval (OpApp Or (IdRef 1) (Prim True)) [(2,True),(1,False)]
test3 = eval (Not (OpApp And (IdRef 1) (IdRef 2))) [(1,False),(2,True)]

------------------------------------------------------
-- PART II
-- on BDDs
-- technically i1 might never be a bool, but gotta check.
-- do ALL cases get run through? Yes.
neg :: BDD -> BDD
neg bs = map negNode bs
  where
  negNode :: BDDNode -> BDDNode
  negNode (i1, (ind, i2, i3)) = let [i1', i2', i3'] = map checkAndSwitch [i1, i2, i3] in (i1', (ind, i2', i3'))
  
  checkAndSwitch :: Id -> Id
  checkAndSwitch num
    | num == -2 || num == -1 = -(3 + num)
    | otherwise = num


test4 = neg [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))]
test5 = neg [(1,(2,2,3)),(2,(1,-2,-2)),(3,(1,-2,-1))]
-- could do with map too - map over the bs to find the one which fits n, then find the one which fits either i2 or i3 as necessary.
-- then stop when an n of -2 or -1 is reached, in which case return it.
-- so guards execute everything that fits? What if one returns something?
-- do I even need to remove stuff I've used from origBs?
checkSat :: BDD -> Env -> Bool
checkSat bdds env = checkSat' bdds env 1 bdds
  where
  checkSat' :: BDD -> Env -> Id -> BDD -> Bool
  checkSat' (b@(i1, (id, i2, i3)):is) e n origBs
    | i1 /= n = checkSat' is e n origBs
    | i1 == n = continue (b:is) e n origBs

  continue :: BDD -> Env -> Id -> BDD -> Bool
  continue (b@(i1, (id, i2, i3)):is) e n origBs
    | (lookUp id e) && (i3 < 0) = lookUp i3 boolTable
    | (lookUp id e) && (i3 >=0) = checkSat' origBs e i3 (origBs\\[b])
    | not(lookUp id e) && (i2 < 0) = lookUp i2 boolTable
    | not(lookUp id e) && (i2 >=0) = checkSat' origBs e i2 (origBs\\[b])

  boolTable :: [(Id, Bool)]
  boolTable = [(-2, True),
               (-1, False)]

checkSat2 :: BDD -> Env -> Bool
checkSat2 nodes@(node@(i1, (name, i2, i3)):rest) env
  | i1 == 1 = checkSat' node env nodes
  | otherwise = checkSat2 rest env
  where
  checkSat' :: BDDNode -> Env -> BDD -> Bool
  checkSat' (i1, (name, i2, i3)) env nodes
    | i2 == -1 || i2 == -2 ||
      i3 == -1 || i3 == -2   = if lookUp name env then toBool i3 else toBool i2
    | lookUp name env        = checkSat' (findNode i3 nodes) env nodes
    | otherwise              = checkSat' (findNode i2 nodes) env nodes

  findNode :: Id -> BDD -> BDDNode
  findNode n nodes = head (filter (\(i1, (name, i2, i3)) -> (i1 == n)) nodes)
 
  toBool :: Id -> Bool
  toBool n 
   | n == -1 = False
   | n == -2 = True
   | otherwise = error "not a Bool value."


test6 = checkSat [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))] [(1,True),(2,False)]
test7 = checkSat [(1,(1,2,8)),(8,(2,-2,-1)),(2,(2,-2,-2))] [(2,True),(1,True)]

------------------------------------------------------
-- PART III

--Pre: The expression contains at least one variable reference
--Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
--The question suggests the following definition (in terms of buildBDD')
--but you are free to implement the function differently if you wish.


-- RELIES ON INDS BEING IN CORRECT ORDER - for this, in ascending order.
-- from just the inds, we know 90% of the tree already.
-- to get the final value, we simply use eval.
-- we do not need exp during most of the execution, only for the base case.
-- this is why we don't need to pattern match on exp!
-- NOT bool:bools, but bools++[bool]. It must go at the end!

buildBDD :: BExp -> [ Index ] -> BDD
buildBDD exp inds = buildBDD' exp 1 0 [] inds

buildBDD' :: BExp -> Id -> Int -> [Bool] -> [ Index ] -> BDD
buildBDD' (Not exp) currId pos bools inds = neg(buildBDD' exp currId pos bools inds)
buildBDD' exp currId pos bools inds
  | pos == length(inds) - 1 = [(currId, (inds!!pos, fromBool(eval exp (zip inds (bools++[False]))), fromBool(eval exp (zip inds (bools++[True])))))]
  | otherwise = [(currId, (inds!!pos, currId*2, currId*2+1))] ++
                (buildBDD' exp (currId*2) (pos+1) (bools++[False]) inds) ++
                (buildBDD' exp (currId*2+1) (pos+1) (bools++[True]) inds)

buildBDD2 :: BExp -> [Index] -> BDD
buildBDD2 exp inds = build' exp 1 [] inds
  where
  build' :: BExp -> Id -> [Bool] -> [Index] -> BDD
  build' (Not exp) id bools inds = neg( build' exp id bools inds)
  build' _ _ _ [] = []
  build' exp id bools [i] 
    = [(id, (i, fromBool(eval exp (zip inds (bools++[False]))), 
                fromBool(eval exp (zip inds (bools++[True])))))]
  build' exp id bools (i1:i2:inds)
    = [(id, (i1, id*2, id*2+1))]
      ++(build' exp (id*2) (bools++[False]) (i2:inds))
      ++(build' exp (id*2+1) (bools++[True]) (i2:inds))

fromBool :: Bool -> Int
fromBool b = if b then -2 else -1

------------------------------------------------------
-- PART IV

--The following types are syntactically identical to BDDNode and BDD, but
--are declared separately here to emphasise that you will need to 
--choose a different DAG representation for ROBDDs than for BDDs.
--Do NOT change these types, however!
type ROBDDNode = ( Id, ( Index, Id, Id ) )
type ROBDD = [ ROBDDNode ]

--BExp is *any* valid boolean expression
--Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements

--buildROBDD :: BExp -> [ Index ] -> ROBDD

-------------------------------------------------------
{-
neg1 = neg [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))]
neg2 = neg [(1,(2,2,3)),(2,(1,-2,-2)),(3,(1,-2,-1))]
check1 = checkSat [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))] [(1,True),(2,False)]
check2 = checkSat [(1,(1,2,8)),(8,(2,-2,-1)),(2,(2,-2,-2))] [(2,True),(1,True)]
build1 = buildBDD b3 [1]
build2 = buildBDD b2 [4]
build3 = buildBDD b5 [2,3,7]
build1' = buildROBDD b6 [1,3,2,4]
build2' = buildROBDD b6 [1,2,3,4]
-}
