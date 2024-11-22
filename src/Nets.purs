module Nets
  ( module Nets.Tree
  , Net(..)
  , Redex(..)
  , evalIso
  , flipRedex
  , reduce
  , reduceAll
  , reduceArr
  , substitute
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (State, evalState, get, modify_, put, runState)
import Data.Array (filter)
import Data.Foldable (oneOfMap)
import Data.Lens.Zoom (zoom)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Nets.Tree
import Utils (emptyBimap)

data Redex = Redex Tree Tree

derive instance Eq Redex
instance Show Redex where
  show (Redex x y) = show x <> " ~ " <> show y

flipRedex :: Redex -> Redex
flipRedex (Redex x y) = (Redex y x)

sortRedex :: Redex -> Redex
sortRedex r@(Redex x y) = if x < y then r else flipRedex r

instance HasVars Redex where
  getVars (Redex t1 t2) = S.union (getVars t1) (getVars t2)
  mapVars = mapRedexVars
  rename varMap (Redex x y) = Redex (rename varMap x) (rename varMap y)

data Net = Net { root :: Tree, redexes :: Array Redex }

type ReduceState = VarGenState

initReduceState :: ReduceState
initReduceState = initVarGenState

annihilate :: TreePair -> TreePair -> Array Redex
annihilate x y = [ Redex x.fst y.fst, Redex x.snd y.snd ]

erase :: TreePair -> Array Redex
erase x = [ Redex Epsilon x.fst, Redex Epsilon x.snd ]

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
reduce (Redex (Delta d) (Gamma g)) = reduce (Redex (Gamma g) (Delta d))
-- OTHERWISE
reduce r@(Redex (Var _) _) = pure [ r ]
reduce r@(Redex _ (Var _)) = pure [ r ]

reduceAll :: Redex -> Array Redex
reduceAll r@(Redex leftTree rightTree) = evalState
  do
    zoom _vars do
      modify_ $ addVarsFromTree leftTree
      modify_ $ addVarsFromTree rightTree
    reduce r
  initReduceState

reduceArr :: Array Redex -> Array Redex
reduceArr = map reduceAll >>> join

getVarRedex :: Redex -> Maybe { varLabel :: VarLabel, tree :: Tree, redex :: Redex }
getVarRedex redex@(Redex (Var s) tree) = Just { varLabel: s, tree, redex }
getVarRedex redex@(Redex tree (Var s)) = Just { varLabel: s, tree, redex }
getVarRedex _ = Nothing

mapRedexVars :: (VarLabel -> Tree) -> Redex -> Redex
mapRedexVars f (Redex x y) = Redex (mapVars f x) (mapVars f y)

substitute :: Array Redex -> Array Redex
substitute [ r ] = [ r ]
substitute arr = fromMaybe arr do
  { varLabel, tree, redex } <- oneOfMap getVarRedex arr
  let subs label = if label == varLabel then tree else Var label
  pure $ map (mapRedexVars subs) $ filter (\r -> r /= redex) arr

instance Ord Redex where
  compare r1 r2 =
    let
      (Redex x1 y1) = sortRedex r1
      (Redex x2 y2) = sortRedex r2
    in
      case compare x1 x2 of
        EQ -> compare y1 y2
        x -> x

evalIso :: forall a. Isomorphic a => a -> a -> Boolean
evalIso x y = evalState (isomorphic x y) emptyBimap

-- Given (A ~ B), (C ~ D), first check tree isomorphisms A <-> C & B <-> D
-- then try A <-> D and B <-> C
instance Isomorphic Redex where
  isomorphic r1 r2 = do
    varMap <- get
    let Tuple isIso s = runState (liftIso r1 r2) varMap
    if isIso then do
      put s
      pure true
    else
      liftIso r1 (flipRedex r2)
    where
    liftIso (Redex x1 y1) (Redex x2 y2) = (lift2 (&&) (isomorphic x1 x2) (isomorphic y1 y2))

