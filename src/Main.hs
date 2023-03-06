module Main where

import Data.Password.Bcrypt
import System.IO
import Control.Exception
import Data.Text
import Crypto.Hash

data Entry = Entry {
  site     :: String,
  username :: String,
  password :: String
} deriving (Show)

main :: IO ()
main = do
    clear
    main2

main2 :: IO ()
main2 = do
    key <- readFile "password.enc"
    password <- pwprompt "Enter your password" "$ "
    let pass = mkPassword $ pack password
    let hash = PasswordHash {
        unPasswordHash = pack key
    }
    if checkPassword pass hash == PasswordCheckSuccess
        then startupDialog
        else do
            clear
            putStrLn "\nWrong password"
            main2

startupDialog :: IO ()
startupDialog = do
    putChar '\n'
    clear
    choise <- prompt "\
        \What would you like to do?\n\
        \1. Add a new password\n\
        \2. Change main password\n\
        \3. View a password for a site\n\
        \4. View all passwords\n\
        \5. Exit\n\
    \" "$ "
    case choise of
        "1" -> addPassword
        "2" -> changeMainPassword
        "3" -> putStrLn "3"
        "4" -> putStrLn "4"
        "5" -> putStrLn "exit"
        _   -> putStrLn "Invalid input"

addPassword :: IO ()
addPassword = do
    clear
    site <- prompt "Enter the site name" "Site: "
    username <- prompt "Enter the username" "Username: "
    password <- pwprompt "Enter the password" "Password: "
    encryptAndSave Entry {
        site = site,
        username = username,
        password = password
    }

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

encryptAndSave :: Entry -> IO ()
encryptAndSave entry = do
    appendFile "passwords.enc" $ encryptedEntry ++ "\n"
    putStrLn "\nPassword saved"
    main2
    where
        encryptedPassword :: String
        encryptedPassword = password entry
        encryptedEntry :: String
        encryptedEntry = site entry ++ ":" ++  username entry ++ ":" ++ encryptedPassword

changeMainPassword :: IO ()
changeMainPassword = do
    clear
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
                clear
                newPass <- pwprompt "Enter new password: " "$ "
                clear
                confirm <- pwprompt "\nConfirm new password: " "$ "
                if newPass == confirm
                    then do
                        let pass = mkPassword $ pack newPass
                        passHash <- hashPassword pass
                        writeFile "password.enc" $ unpack $ unPasswordHash passHash
                        print $ unPasswordHash passHash
                        clear
                        putStrLn "Password changed"
                    else do
                        clear
                        putStrLn "Passwords do not match"
                clear
                main
            | choice == "2" = do
                clear
                main
            | otherwise     = do 
                clear
                main

removePar :: Text -> Text
removePar = Data.Text.dropWhile (=='\"') . dropWhileEnd (=='\"')

clear :: IO ()
clear = putStr "\ESC[2J"
