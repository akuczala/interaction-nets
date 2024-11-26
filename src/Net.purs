module Net
  ( Net
  , NetF(..)
  , Redex
  , RedexF(..)
  , evalIso
  , flipRedex
  , module Net.Tree
  , reduce
  , reduceAll
  , reduceArr
  , reduceNet
  , substitute
  ) where

import Net.Tree
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

derive instance Eq a => Eq (RedexF a)

instance Show Redex where
  show (Redex x y) = show x <> " ~ " <> show y

flipRedex :: forall a. (RedexF a) -> (RedexF a)
flipRedex (Redex x y) = (Redex y x)

sortRedex :: Redex -> Redex
sortRedex r@(Redex x y) = if x < y then r else flipRedex r

newtype NetF a = Net { root :: TreeF a, redexes :: Array (RedexF a) }

derive instance Eq a => Eq (NetF a)

type Net = NetF VarLabel

instance Show Net where
  show (Net n) = "Net " <> show n

type ReduceState = VarGenState

initReduceState :: ReduceState
initReduceState = initVarGenState

annihilate :: TreePair -> TreePair -> Array Redex
annihilate x y = [ Redex x.fst y.fst, Redex x.snd y.snd ]

erase :: NullaryNode -> TreePair -> Array Redex
erase n x = [ Redex (Nullary n) x.fst, Redex (Nullary n) x.snd ]

reduce :: Redex -> State ReduceState (Array Redex)
-- VOID
reduce (Redex (Nullary _) (Nullary _)) = pure []
-- ERASE
reduce original@(Redex (Nullary n) t) = pure $ case t of
  (Binary _ g) -> erase n g
  _ -> [ original ]
reduce (Redex t (Nullary n)) = reduce (Redex (Nullary n) t)
reduce (Redex (Binary b1 p1) (Binary b2 p2)) =
  -- ANNIHILATE
  if b1 == b2 then pure $ annihilate p1 p2
  -- COMMUTE
  else do
    x <- newVar
    y <- newVar
    z <- newVar
    w <- newVar
    pure
      [ Redex (Binary b2 $ makePair (Var x) (Var y)) p1.fst
      , Redex (Binary b2 $ makePair (Var z) (Var w)) p1.snd
      , Redex (Binary b1 $ makePair (Var x) (Var z)) p2.fst
      , Redex (Binary b1 $ makePair (Var y) (Var w)) p2.snd
      ]
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

reduceNet :: Net -> Net
reduceNet (Net { root, redexes }) = Net { root, redexes: reduceArr redexes }

getVarRedex :: Redex -> Maybe { varLabel :: VarLabel, tree :: Tree, redex :: Redex }
getVarRedex redex@(Redex (Var s) tree) = Just { varLabel: s, tree, redex }
getVarRedex (Redex tree (Var s)) = getVarRedex (Redex (Var s) tree)
getVarRedex _ = Nothing

mapRedexVars :: (VarLabel -> Tree) -> Redex -> Redex
mapRedexVars f (Redex x y) = Redex (mapTreeVars f x) (mapTreeVars f y)

substitute :: Net -> Net
substitute (Net net) = case net.redexes of
  [] -> (Net net)
  redexes -> fromMaybe (Net net) do
    { varLabel, tree, redex } <- oneOfMap getVarRedex redexes
    let subs label = if label == varLabel then tree else Var label
    pure $ Net
      { root: net.root >>= subs
      , redexes: map (mapRedexVars subs) $ filter (\r -> r /= redex) redexes
      }

instance Ord Redex where
  compare r1 r2 =
    let
      (Redex x1 y1) = sortRedex r1
      (Redex x2 y2) = sortRedex r2
    in
      case compare x1 x2 of
        EQ -> compare y1 y2
        x -> x

evalIso :: forall t a. Isomorphic t a => t a -> t a -> Boolean
evalIso x y = evalState (isomorphic x y) emptyBimap

-- Given (A ~ B), (C ~ D), first check tree isomorphisms A <-> C & B <-> D
-- then try A <-> D and B <-> C
instance Ord a => Isomorphic RedexF a where
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
