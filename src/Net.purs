module Net
  ( Net
  , NetF(..)
  , _redexes
  , _root
  , evalIso
  , module Net.Parser
  , module Net.Redex
  , module Net.Tree
  , reduce
  , reduceAll
  , reduceArr
  , reduceNet
  , substitute
  , validateNetVars
  )
  where

import Net.Parser
import Net.Redex
import Net.Tree
import Prelude

import Control.Monad.State (State, evalState, modify_)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, all, foldMapDefaultL, foldl, foldr, oneOfMap)
import Data.Lens (lens)
import Data.Lens.Zoom (Lens', zoom)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Utils (emptyBimap)

newtype NetF a = Net { root :: TreeF a, redexes :: Array (RedexF a) }

instance Newtype (NetF a) { root :: TreeF a, redexes :: Array (RedexF a) }

derive instance Eq a => Eq (NetF a)

instance Foldable NetF where
  foldl f b0 (Net n) = foldl (foldl f) (foldl f b0 n.root) n.redexes
  foldr f b0 (Net n) = foldr f (foldr g b0 n.redexes) n.root
    where
    g r b = foldr f b r
  foldMap = foldMapDefaultL

type Net = NetF VarLabel

instance Show Net where
  show (Net n) = "Net " <> show n

_root :: Lens' Net Tree
_root = lens (unwrap >>> _.root) (\(Net n) t -> Net $ n {root = t})

_redexes :: Lens' Net (Array Redex)
_redexes = lens (unwrap >>> _.redexes) (\(Net n) r -> Net $ n {redexes = r})

type ReduceState = VarGenState

initReduceState :: ReduceState
initReduceState = initVarGenState

reduce :: Redex -> State ReduceState (Array Redex)
-- VOID
reduce (Redex (Nullary _) (Nullary _)) = pure []
-- ERASE
reduce original@(Redex (Nullary n) t) = pure $ case t of
  (Binary _ p) -> [ Redex (Nullary n) p.fst, Redex (Nullary n) p.snd ]
  _ -> [ original ]
reduce (Redex t (Nullary n)) = reduce (Redex (Nullary n) t)
reduce (Redex (Binary b1 p1) (Binary b2 p2)) =
  -- ANNIHILATE
  if b1 == b2 then pure $ [ Redex p1.fst p2.fst, Redex p1.snd p2.snd ]
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

evalIso :: forall t a. Isomorphic t a => t a -> t a -> Boolean
evalIso x y = evalState (isomorphic x y) emptyBimap

validateNetVars :: forall a t. Ord a => Foldable t => t a -> Either (Map a Int) (Map a Int)
validateNetVars t = do
  varCounts <- validateVars t
  if all (eq 2) $ M.values varCounts then Right varCounts
  else Left varCounts