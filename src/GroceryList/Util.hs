module GroceryList.Util where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs