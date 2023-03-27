module OriginalType(
  Name(..),
  BirthYear(..),
  BirthMonth(..),
  BirthDay(..),
  User(..),
  toTanaka,
  Age(..),
  Person(..)
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
