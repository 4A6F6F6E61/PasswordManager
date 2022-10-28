module Main where

import Data.Password.Bcrypt
import Data.Text

main :: IO ()
main = do
    let pass = mkPassword $ pack ""
    passHash <- hashPassword pass
    print $ unPasswordHash passHash
    print $ checkPassword pass passHash