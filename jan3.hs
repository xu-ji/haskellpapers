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

lookUp :: Id -> State -> Int

update :: ( Id, Int ) -> State -> State

assignments :: Program -> Int

count :: Statement -> Int

---------------------------------------------------------------------
-- Part II

apply :: Op -> Int -> Int -> Int

eval :: Exp -> State -> Int

executeStatement :: Statement -> State -> State

execute :: Program -> State -> State

---------------------------------------------------------------------
-- Part III

type Graph = [ ( Int, [ Int ] ) ]

graph :: Program -> Graph

