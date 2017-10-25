module StringManip where

import Data.Char

uppercase, lowercase :: String -> String
uppercase = map toUpper
lowercase = map toLower

capitalise :: String -> String
capitalis x =
  let capWord []     = []
      capWord (x:xs) = toUpper x : xs
  in unwords (map capWord (words x))
