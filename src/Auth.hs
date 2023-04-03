module Auth (password) where

password :: IO ()
password = do
  putStrLn "Please typing password"
  pass <- getLine
  let passwords = ["NBPASS", "nbpass", "PASSNB", "passnb"]
  let isActiveUser = pass `elem` passwords
  if (isActiveUser) then putStrLn "Welcome to Haskell World!" else putStrLn "Please enter the correct password."
