import Data.List
import Data.Char
import Data.Maybe

type Program = [ Statement ]

type Id = String

data Statement = Assign Id Exp |
                 Cond Exp Program Program |
                 While Exp Program
                 deriving ( Eq, Ord, Show )

data Exp = Const Int | Var Id | BinOp Op Exp Exp
           deriving ( Eq, Ord, Show )

data Op = Add | Mult | Equal | Greater
          deriving ( Eq, Ord, Show )

type State = [ ( Id, Int ) ]


------------------------------------------------------------------------
-- Test cases

p1,p2,p3,p4,p5 :: Program

-- Maximum of two input values
p1 = [ Assign "x" ( Var "in1" ),
       Assign "y" ( Var "in2" ),
       Cond ( BinOp Greater ( Var "x" ) ( Var "y" ) )
          [ Assign "out1" ( Var "x" ) ]
          [ Assign "out1" ( Var "y" ) ]
     ]

-- Integer sum
p2 = [ Assign "n" ( Var "in1" ),
       Assign "a" ( Const 0 ),
       While ( BinOp Greater ( Var "n" ) ( Const 0 ) )
          [ Assign "a" ( BinOp Add ( Var "a" ) 
                                   ( Var "n" ) ),
            Assign "n" ( BinOp Add ( Var "n" ) 
                                   ( Const (-1) ) ) 
          ],
       Assign "out1" ( Var "a" )
     ]

-- Additional programs for testing purposes (esp. Part III)
--
-- Sequence (contrived example), e.g.
-- Main> graph p3
-- [(1,[2]),(2,[3]),(3,[4]),(4,[0])]
p3 = [ Assign "n" ( Const 1 ),
       Assign "n" ( Const 2 ),
       Assign "n" ( Const 2 ),
       Assign "out1" ( Var "n" )
     ]

-- Fib: computes the nth fibonacci number for a given n, e.g.
-- Main> lookUp "out1" (execute p4 [("in1",1)])
-- 1
-- Main> lookUp "out1" (execute p4 [("in1",3)])
-- 2
-- Main> lookUp "out1" (execute p4 [("in1",5)])
-- 5
-- Main> graph p4
-- [(1,[2]),(2,[3]),(3,[4]),(4,[5,9]),(5,[6]),(6,[7]),(7,[8]),(8,[4]),(9,[0])]
p4 = [ Assign "n" ( Var "in1" ),
       Assign "f1" ( Const 0 ),
       Assign "f2" ( Const 1 ),
       While ( BinOp Greater ( Var "n" ) ( Const 0 ) )
          [ Assign "temp" ( BinOp Add ( Var "f1" ) ( Var "f2" ) ),
            Assign "f1" ( Var "f2" ),
            Assign "f2" ( Var "temp" ),
            Assign "n" ( BinOp Add ( Var "n" ) ( Const (-1) ) )
          ],
       Assign "out1" ( Var "f1" )
      ]

-- PowerSum: sums x^i for i=0..n, where x and n are inputs, e.g.
-- Main> lookUp "out1" (execute p5 [("in1",3),("in2",0)])
-- 1
-- Main> lookUp "out1" (execute p5 [("in1",3),("in2",2)])
-- 13
-- Main> lookUp "out1" (execute p5 [("in1",1),("in2",5)])
-- 6
-- Main> graph p5
-- [(1,[2]),(2,[3]),(3,[4]),(4,[5,12]),(5,[6]),(6,[7]),(7,[8,10]),
--  (8,[9]),(9,[7]),(10,[11]),(11,[4]),(12,[0])]
p5 = [ Assign "x" ( Var "in1" ),
       Assign "n" ( Var "in2" ),
       Assign "a" ( Const 0 ),
       While ( BinOp Greater ( Var "n" ) ( Const (-1) ) )
          [ Assign "p" ( Const 1 ),
            Assign "i" ( Var "n" ),
            While ( BinOp Greater ( Var "i" ) ( Const 0 ) )
               [ Assign "p" ( BinOp Mult ( Var "p" ) ( Var "x" ) ), 
                 Assign "i" ( BinOp Add ( Var "i" ) ( Const (-1) ) )
               ],
            Assign "a" ( BinOp Add ( Var "a" ) ( Var "p" ) ),
            Assign "n" ( BinOp Add ( Var "n" ) ( Const (-1) ) )
          ],
       Assign "out1" ( Var "a" )
      ]

---------------------------------------------------------------------
-- Part I
--pre: state always contains binding for Id, and only one instance.
lookUp :: Id -> State -> Int
lookUp x states = maybe (error "Not found") id (lookup x states)

update :: ( Id, Int ) -> State -> State
update pair [] = [pair]
update (i, val) ((s1, sval):state)
  | i == s1 = (i, val):state
  | otherwise = (s1, sval): (update (i, val) state)

--can't pattern match on the exp!
--uses a simple stack
assignments :: Program -> Int
assignments program = assign' program 0
  where
  assign' :: Program -> Int -> Int
  assign' ((Assign i e):pro) n   
    = assign' pro (n + 1)
  assign' ((Cond e p1 p2):pro) n 
    = assign' pro (n + (assign' p1 0) + (assign' p2 0))
  assign' ((While e p1):pro) n 
    = assign' pro (n + (assign' p1 0)) 

-- you cannot use guards on pattern matching data types.
assignments2 :: Program -> Int
assignments2 program = sum (map count program)
  where
  count :: Statement -> Int
  count (Assign i a)   = 1
  count (Cond e p1 p2) = (assignments2 p1) + (assignments2 p2)
  count (While e p1)   = assignments2 p1
  

---------------------------------------------------------------------
-- Part II

apply :: Op -> Int -> Int -> Int
apply op a b = (fromJust (lookup op opTable)) a b
 
--in lookup tables for functions, always put brackets around.
opTable :: [(Op, (Int -> Int -> Int))]
opTable = [(Add, (+)),
           (Mult, (*)),
           (Equal, (equals)),
           (Greater, (greater))]

equals :: Int -> Int -> Int
equals a b
  | a == b = 1
  | otherwise = 0

greater :: Int -> Int -> Int
greater a b
  | a > b = 1
  | otherwise = 0

--data Exp = Const Int | Var Id | BinOp Op Exp Exp
eval :: Exp -> State -> Int
eval (Const i) _ = i
eval (Var i) state = lookUp i state 
eval (BinOp op e1 e2) state = apply op (eval e1 state) (eval e2 state) 

executeStatement :: Statement -> State -> State
executeStatement (Assign i e) state = update (id, val) state
  where (id, val) = (i, (eval e state))
executeStatement (Cond e pro1 pro2) state
  | eval e state == 1 = execute pro1 state
  | otherwise         = execute pro2 state
executeStatement (While e pro1) state
  | eval e state == 1 = executeStatement (While e pro1) (execute pro1 state)
  | otherwise         = state

--execute must perform program actions one at a time + reflect state
execute :: Program -> State -> State
execute [] state = state
execute (p:program) state = execute program (executeStatement p state)  
---------------------------------------------------------------------
-- Part III

type Graph = [ ( Int, [ Int ] ) ]

buildGraph2 :: Program -> Graph
buildGraph2 p = g
  where
  (r, n, g) = graph' p 1 0 []

--  graph' takes a program, START (next available label), END (where to link to at the end)
--  and the graph accumulated so far
--  returns (ROOT, NEXTSTART, g) 
--  = (r, n, g) where g is the new graph, r is its root ie top label, n is the next available label
  graph' :: Program -> Int -> Int -> Graph -> (Int, Int, Graph)
  graph' [] n endlink g
    = (endlink, n, g) -- should be switched??
  graph' (s:programs) n endlink g
    = graphSt s n' r g'
    where
    (r, n', g') = graph' programs n endlink g
-- the root of the programs graph becomes the endlink of the graph appended onto it


-- graphSt takes a single statement, the START, the END
-- and the resulting graph of graphing all the rest of the programs
-- to create the overarching graph, with its root and next available label
-- (output same as graph')
  graphSt :: Statement -> Int -> Int -> Graph -> (Int, Int, Graph)
  graphSt (Assign x e) start end g
    = (start, start+1, (start, [end]):g)

-- the only one where no programs have to be recursively built!
  graphSt (Cond p pro1 pro2) start end g
    = (start, n2, (start, [r1, r2]):g2)

    where
    (r1, n1, g1) = graph' pro1 (start + 1) end g  
    (r2, n2, g2) = graph' pro2 r1 end g

  graphSt (While p pro) start end g
    = (start, n1, (start, [r1, end]):g1)
   
    where
    (r1, n1, g1) = graph' pro (start + 1) start g
-- returns to the beginning - so the endlink MUST be start!




--Pre: All programs are non-empty
buildGraph :: Program -> Graph
buildGraph p
    = graph p 1 0
    where
    graph :: Program -> Int -> Int -> Graph
    graph [] n t
        = []

    graph ((Assign _ _) : stats) n t
        = if null stats
            then [(n, [t])]
            else [(n, [(n+1)])] ++ 
                    graph stats (n+1) t

    graph ((Cond exp p1 p2): stats) n t
        = [(n, [(n+1), rNo])] ++
            (graph p1 (n+1) t') ++
            (graph p2 rNo t') ++
            (graph stats nAfterStat t)
        where
        t' = if null stats
                then t
                else nAfterStat
        rNo = (n+1) + dp1
        dp1 = deepMax p1
        nAfterStat = dp1 + n + deepMax p2

    graph ((While _ p1): stats) n t
        = [(n, [(n+1), t'])] ++
            (graph p1 (n+1) n) ++
            (graph stats rNo t)
        where
        dp1 = deepMax p1
        rNo = dp1 + n + 1
        t' = if null stats
                    then t
                    else rNo

deepMax :: Program -> Int
deepMax [] = 0
deepMax ((Assign _ _): statements) = 1 + deepMax statements
deepMax ((Cond _ p1 p2): statements) = 1 + (deepMax p1) + (deepMax p2)
                                        + (deepMax statements)
deepMax ((While _ p1): statements) = 1 + (deepMax p1) + (deepMax statements)
