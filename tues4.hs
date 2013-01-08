import Data.Maybe
import Data.List
import Data.Char

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Ord, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Ord, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT ( TFun t t' ) 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT ( TVar a ) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [ ( String, Type ) ]

primTypes :: TypeTable
primTypes = [ ( "+", TFun TInt ( TFun TInt TInt ) ),
              ( ">", TFun TInt ( TFun TInt TBool ) ),
              ( "==", TFun TInt ( TFun TInt TBool ) ),
              ( "not", TFun TBool TBool ) ]

type Env = TypeTable    -- i.e. [(String, Type)]

type Sub = TypeTable    -- i.e. [(String, Type)]  

---------------------------------------------------------------------------------------

-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [ ( a, b ) ] -> b
lookUp x = fromJust . (lookup x)

tryToLookUp :: Eq a => a -> b -> [ ( a, b ) ] -> b
tryToLookUp x alt table = maybe alt id (lookup x table)

tryToLookUp2 :: Eq a => a -> b -> [ ( a, b ) ] -> b
tryToLookUp2 x alt table
  | null results = alt
  | otherwise = head(map snd results) 
  where
  results = (filter (\(a, b) -> a ==x) table)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [ ( a, b ) ] -> [ a ]
reverseLookUp y table = map fst (filter (\ (a, b) -> b ==y) table)

reverseLookUp2 :: Eq b => b -> [ ( a, b ) ] -> [ a ]
reverseLookUp2 y table = [a | (a, b) <- table, b ==y] 

occurs :: String -> Type -> Bool
occurs s (TInt) = False
occurs s (TBool) = False
occurs s (TFun t1 t2) = (occurs s t1) || (occurs s t2)
occurs s (TVar s2) = s == s2
occurs s (TErr) = False
------------

-- PART II
-- Pre: The expression is well formed
-- Pre: The expression does not contain any instance of the Fun constructor
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
-- can use call type finding function to use use guards with types
-- can use let+in expressions with types!
inferType :: Expr -> Env -> Type
inferType (Number i) _           = TInt
inferType (Boolean b) _          = TBool
inferType (Id s) env             = lookUp s env
inferType (Prim f) env           = lookUp f primTypes 
inferType (Cond e1 e2 e3) env
  | inferType e1 env == TBool &&
    inferType e2 env == inferType e3 env = inferType e3 env
  | otherwise                    = TErr
inferType (App e1 e2) env
  | let (TFun t t') = inferType e1 env in (inferType e2 env == t)
                                 = let (TFun t t') = inferType e1 env in t'
  | otherwise                    = TErr

------------

-- PART III

-- unify :: Type -> Type -> Maybe Sub
-- unify t t'
--   = unifyPairs [(t, t')] []

-- I don't actually care about the subs - I only care about the type! If there are any variables!
applySub :: Sub -> Type -> Type
applySub subs (TFun t1 t2) = (TFun (applySub subs t1) (applySub subs t2))
applySub subs (TVar s) = tryToLookUp s (TVar s) subs
applySub subs other = other 


--anything with a list -> list comp!
applySub2 :: Sub -> Type -> Type
applySub2 [] t      = t
applySub2 (s:subs) t = applySub2 subs (apply' t s)
  where
  apply' :: Type -> (String, Type) -> Type
  apply' (TInt) _ = TInt
  apply' (TBool) _ = TBool
  apply' (TFun t1 t2) sub = (TFun (apply' t1 sub) (apply' t2 sub))
  apply' (TVar s) (s1, t)
    | s == s1   = t
    | otherwise = (TVar s) 

{-
data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Ord, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Ord, Show)

type env = [(String, Type)]
-}
--Trust the spec!
-- if it says (the rest equals this) - literally all the rest equals this.
-- use every input given
-- think about previously defined functions
-- do NOT do more work than you have to - given subs and type, focus on the type. 
unifyPairs :: [ ( Type, Type ) ] -> Sub -> Maybe Sub
unifyPairs [] sub = Just sub
unifyPairs ((TInt, TInt):rest) sub 
  = unifyPairs rest sub
unifyPairs ((TBool, TBool):rest) sub 
  = unifyPairs rest sub
unifyPairs ((TVar v, TVar v'):rest) sub
 | v == v' = unifyPairs rest sub
 | otherwise = Nothing
unifyPairs ((TVar v, t):rest) sub
 | occurs v t = Nothing
 | otherwise =  unifyPairs newRest ((v, t):sub)
  where
  newRest = map (\(t1, t2)-> (applySub [(v, t)] t1, applySub [(v, t)] t2)) rest
unifyPairs ((t, TVar v):rest) sub
 | occurs v t = Nothing
 | otherwise =  unifyPairs newRest ((v, t):sub)
  where
  newRest = map (\(t1, t2)-> (applySub [(v, t)] t1, applySub [(v, t)] t2)) rest
unifyPairs ((TFun t1 t2, TFun t1' t2'):rest) sub
  = unifyPairs ((t1, t1'):(t2, t2'):rest) sub 
unifyPairs _ _ = Nothing
------------

-- PART IV

-- inferPolyType :: Expr -> Type

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification.

-- inferPolyType' :: Expr -> Env -> [ String ] -> ( Sub, Type, [ String ] )

-- inferPolyType' :: Expr -> Env -> Int -> ( Sub, Type, Int )

----------------------------------------------------------------------------------------
-- Test examples for Part II...

exampleEnv :: Env
exampleEnv = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt
ex2 = Boolean False
type2 = TBool
ex3 = Prim "not"
type3 =  TFun TBool TBool
ex4 = App (Prim "not") (Boolean True)
type4 = TBool
ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool
ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr
ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool
ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

exs1to8 :: [ Expr ]
exs1to8 = [ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 ]

types1to8 :: [ Type ]
types1to8 = [ type1, type2, type3, type4, type5, type6, type7, type8 ]

----------------------------------------------------------------------------------------
-- Test examples for Part III...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

alluas, allubs :: [ Type ]
alluas = [ u1a, u2a, u3a, u4a, u5a, u6a ]
allubs = [ u1b, u2b, u3b, u4b, u5b, u6b ]

allSubs :: [ Maybe Sub ]
allSubs = [ sub1, sub2, sub3, sub4, sub5, sub6 ]


----------------------------------------------------------------------------------------
-- The following relate to Part IV only...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool
ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")
ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool
ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))
ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) (TVar "a3"))
ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) (TFun (TVar "a2") (TVar "a3"))

exs9to14 :: [ Expr ]
exs9to14 = [ ex9, ex10, ex11, ex12, ex13, ex14 ]
types9to14 :: [ Type ]
types9to14 = [type9, type10, type11, type12, type13, type14 ]

