module Nets
  ( Net
  , RedexF(..)
  , Redex
  , evalIso
  , flipRedex
  , module Nets.Tree
  , reduce
  , reduceAll
  , reduceArr
  , substitute
  ) where

import Nets.Tree
import Prelude

import Control.Apply (lift2)
import Control.Monad.State (State, evalState, get, modify_, put, runState)
import Data.Array (filter)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr, oneOfMap)
import Data.Lens.Zoom (zoom)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Utils (emptyBimap)

data RedexF a = Redex (TreeF a) (TreeF a)

derive instance Functor RedexF

instance Foldable RedexF where
  foldl f b0 (Redex l r) = foldl f (foldl f b0 l) r
  foldr f b0 (Redex l r) = foldr f (foldr f b0 r) l
  foldMap = foldMapDefaultL

type Redex = RedexF VarLabel

derive instance Eq Redex
instance Show Redex where
  show (Redex x y) = show x <> " ~ " <> show y

flipRedex :: Redex -> Redex
flipRedex (Redex x y) = (Redex y x)

sortRedex :: Redex -> Redex
sortRedex r@(Redex x y) = if x < y then r else flipRedex r

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
reduce (Redex t Epsilon) = reduce (Redex Epsilon t)
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
getVarRedex (Redex tree (Var s)) = getVarRedex (Redex (Var s) tree)
getVarRedex _ = Nothing

mapRedexVars :: (VarLabel -> Tree) -> Redex -> Redex
mapRedexVars f (Redex x y) = Redex (mapTreeVars f x) (mapTreeVars f y)

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

