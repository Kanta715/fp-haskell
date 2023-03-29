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
  E(..)
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
