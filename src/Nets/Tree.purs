module Nets.Tree
  ( Pair
  , Tree(..)
  , TreePair
  , VarGenState
  , VarLabel
  , _vars
  , addVarsFromTree
  , class HasVars
  , class Isomorphic
  , evalVarGen
  , getVars
  , initVarGenState
  , isomorphic
  , makeDelta
  , makeGamma
  , makePair
  , mapVars
  , newVar
  , rename
  , varGen
  )
  where

import Prelude

import Control.Monad.State (class MonadState, State, evalState, gets, modify, modify_)
import Data.Array (all, zipWithA)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Lex (succ)
import Type.Proxy (Proxy(..))
import Utils (Bimap, bimapInsert)

class HasVars a where
  getVars :: a -> Set VarLabel
  mapVars :: (VarLabel -> Tree) -> a -> a
  rename :: Map VarLabel VarLabel -> a -> a

class Isomorphic a where
  isomorphic :: a -> a -> State (Bimap VarLabel) Boolean


-- TODO test
instance (Ord a, Isomorphic a) => Isomorphic (Array a) where
  isomorphic xs ys =
    let
      xss = A.sort xs
      yss = A.sort ys
    in
      map (all identity) $ zipWithA isomorphic xss yss


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
  show (Epsilon) = "•"
  show (Var s) = s

instance Ord Tree where
  compare t1 t2 = compare (show t1) (show t2)

makePair :: forall a b. a -> b -> Pair a b
makePair a b = { fst: a, snd: b }

makeGamma :: Tree -> Tree -> Tree
makeGamma a b = Gamma $ makePair a b

makeDelta :: Tree -> Tree -> Tree
makeDelta a b = Delta $ makePair a b


mapTreeVars :: (VarLabel -> Tree) -> Tree -> Tree
mapTreeVars f (Var s) = f s
mapTreeVars _ Epsilon = Epsilon
mapTreeVars f (Gamma g) = makeGamma (mapTreeVars f g.fst) (mapTreeVars f g.snd)
mapTreeVars f (Delta g) = makeDelta (mapTreeVars f g.fst) (mapTreeVars f g.snd)

instance HasVars Tree where
  getVars t = addVarsFromTree t S.empty
  mapVars = mapTreeVars
  rename varMap t = mapVars (\s -> Var $ fromMaybe s (M.lookup s varMap)) t

varGen :: String -> String
varGen x = succ x

addVar :: VarLabel -> Set VarLabel -> Set VarLabel
addVar v = S.insert v

addVarsFromTree :: Tree -> Set VarLabel -> Set VarLabel
addVarsFromTree (Var s) = addVar s
addVarsFromTree (Epsilon) = identity
addVarsFromTree (Gamma g) = addVarsFromTree g.fst <<< addVarsFromTree g.snd

addVarsFromTree (Delta d) = addVarsFromTree d.fst <<< addVarsFromTree d.snd

findNextVar :: VarLabel -> (Set VarLabel) -> VarLabel
findNextVar x s = go x
  where
  go x = if S.member x s then go (varGen x) else x

type VarGenState = { vars :: (Set VarLabel), lastGen :: String }

_vars :: forall a r. Lens' { vars :: a | r } a
_vars = prop (Proxy :: Proxy "vars")

initVarGenState :: VarGenState
initVarGenState = { vars: S.empty, lastGen: "a" }

evalVarGen :: forall a. State VarGenState a -> a
evalVarGen s = evalState s initVarGenState

newVar :: forall m. MonadState VarGenState m => m VarLabel
newVar = do
  newState <- modify f
  pure newState.lastGen
  where
  f state = state { vars = S.insert new state.vars, lastGen = new }
    where
    new = findNextVar state.lastGen state.vars

-- walk both trees together and add variable equivalences to a set
-- terminate if equivalences are inconsistent
instance Isomorphic Tree where
  isomorphic t1 t2 = case [ t1, t2 ] of
    [ Gamma x, Gamma y ] -> pairIso x y
    [ Delta x, Delta y ] -> pairIso x y
    [ Epsilon, Epsilon ] -> pure true
    [ Var x, Var y ] -> varIso x y
    _ -> pure false
    where
    pairIso x y = do
      l <- isomorphic x.fst y.fst
      r <- isomorphic x.snd y.snd
      pure $ l && r
    varIso x y = do
      Tuple maybxFriend maybyFriend <- lookup x y
      case [ maybxFriend, maybyFriend ] of
        [ Just xFriend, Just yFriend ] -> pure $ xFriend == y && yFriend == x
        [ Nothing, Nothing ] -> do
          modify_ $ bimapInsert x y
          pure true
        _ -> pure false

    lookup
      :: VarLabel
      -> VarLabel
      -> State (Bimap VarLabel) (Tuple (Maybe VarLabel) (Maybe VarLabel))
    lookup s1 s2 = gets $ bimap (M.lookup s1) (M.lookup s2)