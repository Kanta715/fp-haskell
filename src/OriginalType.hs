module OriginalType(
  Name(..),
  BirthYear(..),
  BirthMonth(..),
  BirthDay(..),
  User(..),
  toTanaka,
  Age(..),
  Person(..),
  A,
  B,
  C,
  cFunc,
  IntTuple,
  EitherT,
  E(..),
  MyList(..),
  Error,
  Tree(..),
  singleton,
  treeInsert,
  haveInTree,
  Supports,
  Supports(..),
  YesNo(..),
  YesNo
) where

data Name = LastAndFirst          String String |
            LastAndMiddleAndFirst String String String
            deriving (Show)

data BirthYear  = BirthYear Int deriving (Show)

data BirthMonth = BirthMonth Int deriving (Show)

data BirthDay   = BirthDay Int deriving (Show)

data User = User Name BirthYear BirthMonth BirthDay deriving (Show)

toTanaka :: Name -> String
toTanaka (LastAndFirst          _        first) = "Tanaka" ++ first
toTanaka (LastAndMiddleAndFirst _ middle first) = "Tanaka" ++ middle ++ first

data Age = Age Int deriving (Show)

-- レコード構文
data Person = Person {
  name :: Name,
  age  :: Age
} deriving (Show)

type A = String
type B = Int
type C = (String, Int)
cFunc :: C -> A
cFunc (a, b) = a

type IntTuple v = (Int, v)

data EitherT l r = Left l | Right r deriving (Show)

data E l r = L l | R r deriving (Show)

data MyList a = Nil | List a (MyList a) deriving (Show)

data Error = Error String deriving (Show)

data Tree a = None | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton a = Node a None None

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a None = singleton a
treeInsert a (Node x left right)
 | a == x = Node x left right
 | a  < x = Node x (treeInsert a left) right
 | a  > x = Node x left (treeInsert a right)

haveInTree :: (Ord a) => a -> Tree a -> Bool
haveInTree a None = False
haveInTree a (Node v left right)
 | a == v = True
 | a  < v = haveInTree a left
 | a  > v = haveInTree a right

data Supports = Soccer | Tennis | Baseball

instance Eq Supports where
  (==) Soccer Soccer     = True
  (==) Tennis Tennis     = True
  (==) Baseball Baseball = True
  (==) _        _        = False

class YesNo a where
  yesno :: a -> Bool

instance YesNo Supports where
  yesno Soccer   = False
  yesno Tennis   = False
  yesno Baseball = True

instance YesNo (MyList a) where
  yesno Nil = False
  yesno _   = True


