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
lookUp a = fromJust . (lookup a)

tryToLookUp :: Eq a => a -> b -> [ ( a, b ) ] -> b
tryToLookUp value alt table = maybe alt id (lookup value table)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [ ( a, b ) ] -> [ a ]
reverseLookUp val table = map fst (filter (\(a, b) -> (b==val)) table)

occurs :: String -> Type -> Bool
occurs s (TInt) = False
occurs s (TBool) = False
occurs s (TVar x) = s == x
occurs s (TFun t1 t2) = occurs s t1 || occurs s t2
------------

-- PART II

-- Pre: The expression is well formed
-- Pre: The expression does not contain any instance of the Fun constructor
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
--can you pattern match whilst using guards? Not strictly - but you CAN cheat by using functions that return the type.
--can't use let "let (TFun a b) = (inferType e1 env) in (a == inferType e2 env) = b" as b is not in scope on other side of equals sign.
inferType :: Expr -> Env -> Type
inferType (Number n) _ = TInt
inferType (Boolean b) _ = TBool
inferType (Id s) env = lookUp s env
inferType (Prim s) _ = lookUp s primTypes
inferType (Cond e1 e2 e3) env
  | (inferType e1 env == TBool) && (inferType e1 env == inferType e2 env) = inferType e1 env
  | otherwise = TErr
inferType (App e1 e2) env
  | (a == inferType e2 env) = b
  | otherwise = TErr
  where (TFun a b) = (inferType e1 env)

------------

-- PART III
{-
data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
-}

applySub :: Sub -> Type -> Type
applySub s (TFun t1 t2) = TFun (applySub s t1) (applySub s t2)
applySub s (TVar string) = tryToLookUp string (TVar string) s
applySub s rest = rest


--does Haskell proceed to next case if the guards do not kick in?
--problem being need to cover all type combinations
--Not in scope: data constructor `Maybe'? MAYBE IS THE TYPE CONSTRUCTOR YOU FOOL. You need the data constructors Nothing or Just.
unifyPairs :: [ ( Type, Type ) ] -> Sub -> Maybe Sub
unifyPairs [] sub = Just sub
unifyPairs ((TInt, TInt):rest) sub = unifyPairs rest sub
unifyPairs ((TBool, TBool):rest) sub = unifyPairs rest sub
unifyPairs ((TVar v, TVar v'):rest) sub
  | v == v' = unifyPairs rest sub
unifyPairs ((TVar v, rT):rest) sub
  | occurs v rT = Nothing
  | otherwise = unifyPairs (map (\(a, b) -> (applySub [(v, rT)] a, applySub [(v, rT)] b)) rest) ((v,rT):sub)
unifyPairs ((rT, TVar v):rest) sub
  | occurs v rT = Nothing
  | otherwise = unifyPairs (map (\(a, b) -> (applySub [(v, rT)] a, applySub [(v, rT)] b)) rest) ((v,rT):sub)
unifyPairs ((TFun t1 t2, TFun t3 t4):rest) sub = unifyPairs ((t1,t2):(t3,t4):rest) sub
unifyPairs ((TFun t1 t2, rT):rest) sub = unifyPairs ((t1,t2):rest) sub
unifyPairs ((rT, TFun t1 t2):rest) sub = unifyPairs ((t1,t2):rest) sub
unifyPairs _ _ = Nothing

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

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
