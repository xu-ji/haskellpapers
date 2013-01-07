import Data.List
import Data.Maybe
import Data.Char

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
bdd2,bdd3,bdd9,bdd4,bdd5,bdd6,bdd7,bdd8 :: BDD
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
bdd9 = [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))]


------------------------------------------------------
-- PART I

--Pre: The item is in the given table
lookUp :: Eq a => a -> [ ( a, b ) ] -> b
lookUp a = fromJust . (lookup a)
{-
type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | OpApp Op BExp BExp
            deriving (Eq,Ord,Show)

data Op = And | Or 
          deriving (Eq,Ord,Show)

type Env = [ ( Index, Bool ) ]
-}
eval :: BExp -> Env -> Bool
eval (Prim b) _     = b
eval (IdRef id) env = lookUp id env
eval (Not e) env    = not (eval e env)
eval (OpApp op e1 e2) env
  | op == And       = (eval e1 env) && (eval e2 env)
  | op == Or        = (eval e1 env) || (eval e2 env) 

test1 = eval (Prim False) [(1,False),(2,True)]
test2 = eval (OpApp Or (IdRef 1) (Prim True)) [(2,True),(1,False)]
test3 = eval (Not (OpApp And (IdRef 1) (IdRef 2))) [(1,False),(2,True)]

------------------------------------------------------
-- PART II
{-
type Id = Int

type BDDNode = ( Id, ( Index, Id, Id ) )

type BDD = [ BDDNode ]
-}
neg :: BDD -> BDD
neg bdd = map neg' bdd
  where
  neg' :: BDDNode -> BDDNode
  neg' (this, (name, n1, n2)) = (this, (name, checkS n1, checkS n2))

  checkS :: Id -> Id
  checkS n
    | n == -1 || n == -2 = -(3+n)
    | otherwise          = n
  
test4 = neg [(1,(1,2,3)),(2,(2,-2,-2)),(3,(2,-2,-1))]
test5 = neg [(1,(2,2,3)),(2,(1,-2,-2)),(3,(1,-2,-1))]

--evaluating not trees, but GRAPHS! Walk through.
--first we must find the starting point.
--then, we can take the found nodes INDIVIDUALLY in turn and "run with it". FOLLOW THE NODES
-- we essentially reduce pile of nodes -> one node + original pile.
-- alternatively, could have initiated checkSat' with ((-1, (0, 0, 0)):(-2, (0, 0, 0)):nodes) and pattern matched for this base case instead of using guards.

checkSat :: BDD -> Env -> Bool
checkSat nodes@(node@(i1, (name, i2, i3)):rest) env
  | i1 == 1 = checkSat' node env nodes
  | otherwise = checkSat rest env
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

{-
type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | OpApp Op BExp BExp
            deriving (Eq,Ord,Show)

data Op = And | Or 
          deriving (Eq,Ord,Show)

type Env = [ ( Index, Bool ) ]
-}
--the id variable in the helper contains the top idRef for that tree.
buildBDD :: BExp -> [ Index ] -> BDD
buildBDD exp ind = snd (build' exp 1 [])
  where
  build' :: BExp -> Id -> Env -> (Id, BDD)
  build' (Prim b) n env = (n, [])
  build' (IdRef id) n env = (id, [(n, (id, toNum(eval exp ((id, False):env)), toNum(eval exp ((id, True):env))))])
  build' (Not exp) n env = (fst (build' exp n env), (neg.snd) (build' exp n env))
--we do create stuff when there is an IdRef and something else.
--2 IdRefs get caught in this case.
  build' (OpApp op (IdRef id) exp2) n env
    = ( id, (n, (id, n+1, 2*n+1)):((snd (build' exp2 (n+1) ((id, False):env)))++(snd (build' exp2 (2*n+1) ((id, True):env)))) )
  build' (OpApp op exp2 (IdRef id)) n env
    = ( id, (n, (id, n+1, 2*n+1)):((snd (build' exp2 (n+1) ((id, False):env)))++(snd (build' exp2 (2*n+1) ((id, True):env)))) )
--with a binary application on two opapps, as here, we do not actually create anything! Just recurse.
  build' (OpApp op exp1 exp2) n env = (fst (build' exp1 n env), (snd (build' exp1 n env)) ++ (snd (build' exp2 (n+1) env)))  


toNum :: Bool -> Id
toNum True = -2
toNum False = -1

{-
--two idRefs
  build' (OpApp op (IdRef id1) (IdRef id2)) n env = (id1, (snd (build' (IdRef id1) n env)) ++ (snd (build' (IdRef id2) n+1 env)))  
  build' (OpApp op (IdRef id) exp2) n env
    = ( id, (n, (id, n+1, 2*n+1)):(snd (build' exp2 (n+1) ((id, False):env))):(snd (build' exp2 (2*n+1) ((id, True):env))) )
  build' (OpApp op exp2 (IdRef id)) n env
    = ( id, (n, (id, n+1, 2*n+1)):(snd (build' exp2 (n+1) ((id, False):env))):(snd (build' exp2 (2*n+1) ((id, True):env))) )
--two further OppApps
  build' (OppApp op exp1 exp2) n env = (fst (build' exp1 n env), (snd build' exp1 n env)++(snd build' exp2 n+1 env)) 
-}
--Potential helper function for buildBDD which you are free
--to define/modify/ignore/delete as you see fit.

--buildBDD' :: BExp -> Id -> [ Index ] -> [ ( Index, Bool ) ] 
--          -> ( Id, BDD )

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
