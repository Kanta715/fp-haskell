module List.List(filterUpperCase) where

filterUpperCase :: [Char] -> [Char]
filterUpperCase charList = [char | char <- charList, elem char ['A'..'Z']]
