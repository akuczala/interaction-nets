module Net.Tree
  ( BinaryNode(..)
  , NullaryNode(..)
  , Operator(..)
  , Pair
  , Tree(..)
  , TreeF(..)
  , TreePair
  , VarGenState
  , VarGenStateF
  , VarLabel
  , _vars
  , addVarsFromTree
  , class Isomorphic
  , evalTree
  , evalVarGen
  , getVars
  , initVarGenState
  , isomorphic
  , makeDelta
  , makeEpsilon
  , makeGamma
  , makeNumber
  , makeOperator
  , makePair
  , mapTreeVars
  , newVar
  , rename
  , validateVars
  , varGen
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (class MonadState, State, evalState, gets, modify, modify_)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMapDefaultL, foldr)
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

getVars :: forall a t. Ord a => Foldable t => t a -> Set a
getVars = S.fromFoldable

validateVars :: forall a t. Ord a => Foldable t => t a -> Either (Map a Int) (Map a Int)
validateVars t = foldr go (Right M.empty) t
  where
  go x vars = do
    xs <- vars
    let count = fromMaybe 0 (M.lookup x xs)
    let newVarCount = M.insert x (count + 1) xs
    if count < 2 then Right newVarCount
    else Left newVarCount

class Isomorphic t a where
  isomorphic :: t a -> t a -> State (Bimap a) Boolean

type VarLabel = String

type Pair a = { fst :: a, snd :: a }

-- derive instance Functor Pair
mapPair :: forall a b. (a -> b) -> Pair a -> Pair b
mapPair f p = { fst: f p.fst, snd: f p.snd }

data Operator = Add | Sub | Mul | Div

derive instance Eq Operator
instance Show Operator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data BinaryNode = Gamma | Delta | Operator Operator

derive instance Eq BinaryNode

data NullaryNode = Epsilon | Num Number

derive instance Eq NullaryNode

data TreeF a
  = Binary BinaryNode (Pair (TreeF a))
  | Nullary NullaryNode
  | Var a

derive instance Functor TreeF

instance Apply TreeF where
  apply = ap

instance Applicative TreeF where
  pure = Var

instance Bind TreeF where
  bind t f = case t of
    Var v -> f v
    Nullary n -> Nullary n
    Binary b p -> Binary b $ makePair (bind p.fst f) (bind p.snd f)

instance Monad TreeF

foldlTree :: forall b a. (b -> a -> b) -> b -> TreeF a -> b
foldlTree f b0 t = case t of
  Var a -> f b0 a
  Nullary _ -> b0
  Binary _ p -> foldlTree f (foldlTree f b0 p.fst) p.snd

foldrTree :: forall b a. (a -> b -> b) -> b -> TreeF a -> b
foldrTree f b0 t = case t of
  Var a -> f a b0
  Nullary _ -> b0
  Binary _ p -> foldrTree f (foldrTree f b0 p.snd) p.fst

instance Foldable TreeF where
  foldl = foldlTree
  foldr = foldrTree
  foldMap = foldMapDefaultL

type TreePair = Pair Tree

type Tree = TreeF VarLabel

derive instance Eq a => Eq (TreeF a)

instance Show (Tree) where
  show (Binary b p) = x.leftBracket <> show p.fst <> x.sep <> show p.snd <> x.rightBracket
    where
    x = case b of
      Gamma -> { leftBracket: "(", rightBracket: ")", sep: " " }
      Delta -> { leftBracket: "{", rightBracket: "}", sep: " " }
      Operator o -> { leftBracket: "[", rightBracket: "]", sep: " " <> show o <> " " }
  show (Nullary n) = case n of
    Epsilon -> "•"
    Num x -> show x
  show (Var s) = s

instance (Eq (TreeF a), Show (TreeF a)) => Ord (TreeF a) where
  compare t1 t2 = compare (show t1) (show t2)

makePair :: forall a. a -> a -> Pair a
makePair a b = { fst: a, snd: b }

makeGamma :: forall a. TreeF a -> TreeF a -> TreeF a
makeGamma a b = Binary Gamma $ makePair a b

makeDelta :: forall a. TreeF a -> TreeF a -> TreeF a
makeDelta a b = Binary Delta $ makePair a b

makeEpsilon :: forall a. TreeF a
makeEpsilon = Nullary Epsilon

makeOperator :: forall a. Operator -> TreeF a -> TreeF a -> TreeF a
makeOperator o a b = Binary (Operator o) $ makePair a b

makeNumber :: forall a. Number -> TreeF a
makeNumber x = Nullary (Num x)

mapTreeVars :: forall a. (a -> TreeF a) -> TreeF a -> TreeF a
mapTreeVars = flip bind

evalTree :: forall a. TreeF a -> TreeF a
evalTree (Binary (Operator o) { fst: Nullary (Num x), snd: Nullary (Num y) }) = Nullary $ Num (op x y)
  where
  op = case o of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> (/)
evalTree (Binary b p) = Binary b $ mapPair (evalTree) p
evalTree x = x

rename :: forall t a. Ord a => Functor t => Map a a -> t a -> t a
rename varMap t = map (\s -> fromMaybe s (M.lookup s varMap)) t

varGen :: String -> String
varGen x = succ x

addVarsFromTree :: forall a. Ord a => TreeF a -> Set a -> Set a
addVarsFromTree t = S.union (getVars t)

findNextVar :: VarLabel -> (Set VarLabel) -> VarLabel
findNextVar x s = go x
  where
  go x = if S.member x s then go (varGen x) else x

type VarGenStateF a = { vars :: (Set a), lastGen :: a }
type VarGenState = VarGenStateF VarLabel

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
instance Ord a => Isomorphic TreeF a where
  isomorphic t1 t2 = case [ t1, t2 ] of
    [ Binary b1 p1, Binary b2 p2 ] -> if b1 == b2 then pairIso p1 p2 else pure false
    [ Nullary n1, Nullary n2 ] -> pure (n1 == n2)
    [ Var x, Var y ] -> varIso x y
    _ -> pure false
    where
    pairIso x y = lift2 (&&) (isomorphic x.fst y.fst) (isomorphic x.snd y.snd)
    varIso x y = do
      Tuple maybxFriend maybyFriend <- lookup x y
      case [ maybxFriend, maybyFriend ] of
        [ Just xFriend, Just yFriend ] -> pure $ xFriend == y && yFriend == x
        [ Nothing, Nothing ] -> do
          modify_ $ bimapInsert x y
          pure true
        _ -> pure false

    lookup
      :: forall a
       . Ord a
      => a
      -> a
      -> State (Bimap a) (Tuple (Maybe a) (Maybe a))
    lookup s1 s2 = gets $ bimap (M.lookup s1) (M.lookup s2)