module Exam where

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

--Pre: The item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x = fromJust . (lookup x)
--does \\ remove all instances of something? No, it removes one instance.

states :: LTS -> [State]
states lts = nub (concatMap (\(a, b) -> [a, b]) (map fst lts))

states2 :: LTS -> [State]
states2 lts = nub (first ++ sec)
  where (first, sec) = (unzip (map fst lts))

transitions :: State -> LTS -> [Transition]
transitions n lts = filter (\((s1, s2), i) -> (s1 == n)) lts 

alphabet :: LTS -> Alphabet
alphabet = nub . (map snd)
---------------------------------------------------------------------------
--PART II

--Note: actions gives it in the correct order!
actions :: Process -> [Id]
actions (STOP) = []
actions (Ref i) = [i]
actions (Prefix i pro) = i: (actions pro)
actions (Choice pros) = concatMap actions pros

--Pre: The first item in the list of process definitions is
--     that of the start process.
-- this is extremely important. It means we only need to evaluate the HEAD of the list of processDefs. 
-- could also try mapping every possible route. NO. Instead - use pattern matching to follow the route.
-- first version relied on carrying the rest of the processDefs along in the second argument. This is not necessary - once picked, we stay on that processDef unless there is a Ref (at which point we can use the orig list carried as an extra variable).
-- so it is changed to only take the top processDef <- this is all you need
accepts :: [Id] -> [ProcessDef] -> Bool
accepts ids (p:pdef) = accepts' ids p (p:pdef)
  where
  accepts' :: [Id] -> ProcessDef -> [ProcessDef] -> Bool
  accepts' [] _ _ 
    = True
  accepts' (i:ids) (name, STOP) _ 
    = False
  accepts' (i:ids) (name, (Ref newName)) pdefs
    = or (map (\(pdef) -> accepts' (i:ids) pdef pdefs) sel)
     where sel = (filter (\(id, process) -> (id == newName)) pdefs)
  accepts' (i:ids) (name, (Prefix id process)) pdefs
    | i == id = accepts' ids (name, process) pdefs
    | otherwise = False
  accepts' (ids) (name, (Choice (pros))) pdefs 
    = or (map (\ pdef -> accepts' (ids) pdef pdefs) (makeDefs name pros))

makeDefs :: Id -> [Process] -> [ProcessDef]
makeDefs _ [] = []
makeDefs name (p:pros) = (name, p): (makeDefs name pros)

---------------------------------------------------------------------------
--PART III

--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
-- clearly Haskell only follows the First guard to satisfy the condition.
composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
composeTransitions ((s, t), a) ((s', t'), a') alpha1 alpha2 map
  | a == a' 
      = [(((lookUp (s, s') map), (lookUp (t, t') map)), a)]
  | a `elem` alpha2 && a' `elem` alpha1 
      = []
  | a' `elem` alpha1 
      = [(((lookUp (s, s') map), (lookUp (t, s') map)), a)]
  | a `elem` alpha2 
      = [(((lookUp (s, s') map), (lookUp (s, t') map)), a')]
  | otherwise = [(((lookUp (s, s') map), (lookUp (t, s') map)), a), 
                 (((lookUp (s, s') map), (lookUp (s, t') map)), a')]

pruneTransitions :: [Transition] -> LTS
pruneTransitions trans = findTrans trans [0] [] [] 

findTrans :: [Transition] -> [State] -> [Transition] -> [Transition] -> [Transition]
findTrans [] vStates visited nVisited
  | exists vStates nVisited = findTrans nVisited vStates visited []
  | otherwise = visited
findTrans (((from, to), name):trans) vStates visited nVisited
  | from `elem` vStates = findTrans trans (to:vStates) (((from, to), name):visited) nVisited
  | otherwise = findTrans trans vStates visited (((from, to), name):nVisited)

exists :: [State] -> [Transition] -> Bool
exists states trans = any (\ ((a, b), name) -> a `elem` states) trans

---------------------------------------------------------------------------
--PART IV

--compose :: LTS -> LTS -> LTS

---------------------------------------------------------------------------
--PART V

--buildLTS :: [ProcessDef] -> LTS
--Pre: All process references (Ref constructor) have a corresponding
--     definition in the list of ProcessDefs.


