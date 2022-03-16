module Main where

import Data.Password.Bcrypt
import System.IO
import Control.Exception
import Data.Text
import Crypto.Hash

main :: IO ()
main = do
    choice <- prompt "\
        \Are you Sure you want to change the password?\n\
        \ALL CURRENTLY SAVED LOGINS WILL BE REMOVED\n\
        \\n\
        \1. Yes\n\
        \2. No\
    \" "$ "
    handle choice
    where
        handle :: String -> IO ()
        handle choice
            | choice == "1" = do
                newPass <- pwprompt "Enter new password: " "$ "
                confirm <- pwprompt "\nConfirm new password: " "$ "
                if newPass == confirm
                    then do
                        let pass = mkPassword $ pack newPass
                        passHash <- hashPassword pass
                        writeFile "password.enc" $ unpack $ unPasswordHash passHash
                        print $ unPasswordHash passHash
                        putStrLn "Password changed"
                    else putStrLn "Passwords do not match"
                main
            | choice == "2" = main
            | otherwise     = main

removePar :: Text -> Text
removePar = Data.Text.dropWhile (=='\"') . dropWhileEnd (=='\"')

prompt :: String -> String -> IO String
prompt head prompt = do
    putStrLn head
    putStr prompt
    hFlush stdout
    withEcho True getLine

pwprompt :: String -> String  -> IO String
pwprompt head prompt = do
    putStrLn head
    putStr prompt
    hFlush stdout
    withEcho False getLine

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action