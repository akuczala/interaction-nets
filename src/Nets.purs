module Nets
  ( Net(..)
  , Pair
  , Redex(..)
  , Tree(..)
  , TreePair
  , VarLabel
  , annihilate
  , fixedPoint
  , makeDelta
  , makeGamma
  , makePair
  , reduce
  , reduceAll
  , reduceArr
  , substitute
  ) where

import Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Array (filter, find)
import Data.Foldable (oneOfMap)
import Data.List.Lazy as L
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple, fst)
import Lex (succ)

type VarLabel = String
type Pair a b = { fst :: a, snd :: b }
type TreePair = Pair Tree Tree
data Tree
  = Gamma TreePair
  | Delta TreePair
  | Epsilon
  | Var VarLabel

derive instance Eq Tree

instance Show Tree where
  show (Gamma g) = "(" <> show g.fst <> ", " <> show g.snd <> ")"
  show (Delta d) = "{" <> show d.fst <> ", " <> show d.snd <> "}"
  show (Epsilon) = "o"
  show (Var s) = s

data Redex = Redex Tree Tree

derive instance Eq Redex
instance Show Redex where
  show (Redex x y) = show x <> " ~ " <> show y

data Net = Net { root :: Tree, redexes :: Array Redex }

makePair :: forall a b. a -> b -> Pair a b
makePair a b = { fst: a, snd: b }

makeGamma :: Tree -> Tree -> Tree
makeGamma a b = Gamma $ makePair a b

makeDelta :: Tree -> Tree -> Tree
makeDelta a b = Delta $ makePair a b

-- erase :: forall f. Epsilon -> f TreePair -> Array Redex
-- erase e 

annihilate :: TreePair -> TreePair -> Array Redex
annihilate x y = [ Redex x.fst y.fst, Redex x.snd y.snd ]

-- commute :: TreePair -> TreePair -> Tuple (Tuple Redex)
-- commute x y = [

-- ]

erase :: TreePair -> Array Redex
erase x = [ Redex Epsilon x.fst, Redex Epsilon x.snd ]

type ReduceState = { vars :: Set VarLabel, lastGen :: String }

initReduceState :: ReduceState
initReduceState = { vars: S.empty, lastGen: "a" }

addVar :: VarLabel -> State ReduceState ReduceState
addVar v = modify $ \s -> s { vars = S.insert v s.vars }

addVarsFromTree :: Tree -> State ReduceState Unit
addVarsFromTree (Var s) = do
  _ <- addVar s
  pure unit
addVarsFromTree (Epsilon) = pure unit
addVarsFromTree (Gamma g) = do
  addVarsFromTree g.fst
  addVarsFromTree g.snd

addVarsFromTree (Delta d) = do
  addVarsFromTree d.fst
  addVarsFromTree d.snd

varGen :: String -> String
varGen x = succ x

thing2 :: VarLabel -> (Set VarLabel) -> VarLabel
thing2 x s = go x
  where
  go x = if S.member x s then go (varGen x) else x

-- thing2 x s = iterateUntil (\v -> S.member v s) (pure x)

newVar :: State { vars :: (Set VarLabel), lastGen :: String } VarLabel
newVar = do
  newState <- modify f
  pure newState.lastGen
  where
  f state = state { vars = S.insert new state.vars, lastGen = new }
    where
    new = thing2 state.lastGen state.vars

reduce :: Redex -> State ReduceState (Array Redex)
-- VOID
reduce (Redex Epsilon Epsilon) = pure []
-- ERASE
reduce original@(Redex Epsilon t) = pure $ case t of
  (Gamma g) -> erase g
  (Delta g) -> erase g
  _ -> [ original ]
reduce original@(Redex t Epsilon) = pure $ case t of
  (Gamma g) -> erase g
  (Delta g) -> erase g
  _ -> [ original ]
-- ANNIHILATE
reduce (Redex (Gamma x) (Gamma y)) = pure $ annihilate x y
reduce (Redex (Delta x) (Delta y)) = pure $ annihilate x y
-- COMMUTE
reduce (Redex (Gamma g) (Delta d)) = do
  x <- newVar
  y <- newVar
  z <- newVar
  w <- newVar
  pure
    [ Redex (makeDelta (Var x) (Var y)) g.fst
    , Redex (makeDelta (Var z) (Var w)) g.snd
    , Redex (makeGamma (Var x) (Var z)) d.fst
    , Redex (makeGamma (Var y) (Var w)) d.snd
    ]
reduce (Redex (Delta d) (Gamma g)) = do
  x <- newVar
  y <- newVar
  z <- newVar
  w <- newVar
  pure
    [ Redex (makeDelta (Var x) (Var y)) g.fst
    , Redex (makeDelta (Var z) (Var w)) g.snd
    , Redex (makeGamma (Var x) (Var z)) d.fst
    , Redex (makeGamma (Var y) (Var w)) d.snd
    ]
-- OTHERWISE
reduce r@(Redex (Var _) _) = pure [ r ]
reduce r@(Redex _ (Var _)) = pure [ r ]

reduceAll :: Redex -> Array Redex
reduceAll r@(Redex leftTree rightTree) = fst $ runState
  ( do
      addVarsFromTree leftTree
      addVarsFromTree rightTree
      reduce r

  )
  initReduceState

reduceArr :: Array Redex -> Array Redex
reduceArr = map reduceAll >>> join

getVarRedex :: Redex -> Maybe { varLabel :: VarLabel, tree :: Tree, redex :: Redex }
getVarRedex redex@(Redex (Var s) tree) = Just { varLabel: s, tree, redex }
getVarRedex redex@(Redex tree (Var s)) = Just { varLabel: s, tree, redex }
getVarRedex _ = Nothing

mapTreeVars :: (VarLabel -> Tree) -> Tree -> Tree
mapTreeVars f (Var s) = f s
mapTreeVars _ Epsilon = Epsilon
mapTreeVars f (Gamma g) = makeGamma (mapTreeVars f g.fst) (mapTreeVars f g.snd)
mapTreeVars f (Delta g) = makeDelta (mapTreeVars f g.fst) (mapTreeVars f g.snd)

mapRedexVars :: (VarLabel -> Tree) -> Redex -> Redex
mapRedexVars f (Redex x y) = Redex (mapTreeVars f x) (mapTreeVars f y)

substitute :: Array Redex -> Array Redex
substitute [ r ] = [ r ]
substitute arr = fromMaybe arr do
  { varLabel, tree, redex } <- oneOfMap getVarRedex arr
  let subs label = if label == varLabel then tree else Var label
  pure $ map (mapRedexVars subs) $ filter (\r -> r /= redex) arr

fixedPoint :: forall a. Eq a => (a -> a) -> (a -> a)
fixedPoint f = fixed
  where
  fixed a = if nextA == a then a else fixed nextA
    where
    nextA = f a