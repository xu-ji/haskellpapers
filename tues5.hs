module Test where

import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq,Ord,Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------------

--Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                            Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                            Prefix "off" STOP])

clock = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), Prefix "end" STOP])

maker = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch = ("SWITCH", Ref "OFF")

off = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS = [((0,1),"tick"),((1,0),"tock")]

playLTS = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS = [((0,1),"make"),((1,0),"ready")]

userLTS = [((0,1),"ready"),((1,0),"use")]

makerUserLTS = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS = [((0,1),"on"),((1,0),"off")]

---------------------------------------------------------------------------
--PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp key = fromJust . (lookup key)

states :: LTS -> [State]
states = nub . (concatMap (\((f, t), action) -> [f, t]))

transitions :: State -> LTS -> [Transition]
transitions s lts = filter (\((f, t), action) -> f == s) lts 

alphabet :: LTS -> Alphabet
alphabet = nub . snd . unzip
---------------------------------------------------------------------------
--PART II
{-

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq,Ord,Show)

type ProcessDef = (Id, Process)
-}

actions :: Process -> [Id]
actions (STOP) = []
actions (Ref id) = []
actions (Prefix id pro) = id : (actions pro) 
actions (Choice pros) = concatMap actions pros
 
--Pre: The first item in the list of process definitions is
--     that of the start process.
-- remember the original list of pdefs is WHAT YOU HAVE AT YOUR DISPOSAL.
accepts :: [Id] -> [ProcessDef] -> Bool
accepts ids (p:pdefs) = accepts' ids p (p:pdefs)
  where
  accepts' :: [Id] -> ProcessDef -> [ProcessDef] -> Bool
  accepts' [] _ _ = True
  accepts' ids (name, STOP) pdefs = False
  accepts' ids (name, (Ref id)) pdefs = accepts' ids newPdef pdefs
    where 
    newPdef = (id, lookUp id pdefs)
  accepts' (i:ids) (name, (Prefix id pro)) pdefs
    | i == id   = accepts' ids (name, pro) pdefs
    | otherwise = False
  accepts' (i:ids) (name, (Choice pros)) pdefs = or (map (\pro -> accepts' (i:ids) (name, pro) pdefs) pros)

---------------------------------------------------------------------------
--PART III
{-
type Transition = ((State, State), Id)

type LTS = [Transition]
-}
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
composeTransitions ((s, t), a) ((s', t'), a') a1 a2 m
  | a == a' = [(((lookUp (s, s') m), (lookUp (t, t') m)), a)]
  | a `elem` a2 && a' `elem` a1 = []
  | a' `elem` a1 = [(((lookUp (s, s') m), (lookUp (t, s') m)), a)]
  | a `elem` a2 = [(((lookUp (s, s') m), (lookUp (s, t') m)), a')]
  | otherwise = [(((lookUp (s, s') m), (lookUp (s, t') m)), a'), (((lookUp (s, s') m), (lookUp (t, s') m)), a)]

-- in general, map does not produce non exhaustive mistakes. 
-- map (\a -> a +1) [] = []
pruneTransitions :: [Transition] -> LTS
pruneTransitions (l:lts) = prune' l (l:lts)
  where 
  prune' :: Transition -> [Transition] -> [Transition]
  prune' ((f, t), h) origLTS 
    = ((f, t), h): (concatMap (\trans -> prune' trans origLTS) 
                   (filter (\((a, b), c) -> a == t) origLTS))
-- can't have terms starting with capitals - interpreted as data constructor!
pruneBasket :: [Transition] -> LTS
pruneBasket lts = prune lts [0] [] []
  where
  prune :: [Transition] -> [State] -> [Transition] -> [Transition] -> LTS
  prune [] vStates visited nVisited
    | exists vStates nVisited = prune nVisited vStates visited []
    | otherwise = visited
  prune (((f, t), n):ts) vStates visited nVisited
    | f `elem` vStates 
       = prune ts (t:vStates) (((f, t), n):visited) nVisited
    | otherwise 
       = prune ts vStates visited (((f, t), n):nVisited)

exists :: [State] -> [Transition] -> Bool
exists states trans = any (\((a, b), c) -> a `elem` states ) trans
---------------------------------------------------------------------------
--PART IV

--compose :: LTS -> LTS -> LTS

---------------------------------------------------------------------------
--PART V

--buildLTS :: [ProcessDef] -> LTS
--Pre: All process references (Ref constructor) have a corresponding
--     definition in the list of ProcessDefs.



