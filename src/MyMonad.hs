module MyMonad (
  MyMaybe(..),
  getPlus100MyMaybe
) where

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq, Ord, Read)

instance Functor MyMaybe where
  fmap f MyNothing  = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
  pure = return

  MyNothing <*> _         = MyNothing
  _         <*> MyNothing = MyNothing
  MyJust f  <*> MyJust x  = MyJust (f x)

instance Monad MyMaybe where
  return x = MyJust x

  MyNothing >>= f = MyNothing
  MyJust x  >>= f = f x

getPlus100MyMaybe :: Int -> MyMaybe Int
getPlus100MyMaybe x = return (x + 100)
