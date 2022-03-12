module Main where

import Data.Password.Bcrypt
    (checkPassword,
      mkPassword,
      PasswordCheck(PasswordCheckSuccess),
      PasswordHash(PasswordHash, unPasswordHash)) 
import System.IO (hFlush, hGetEcho, hSetEcho, stdin, stdout)
import Control.Exception (bracket_)
import Data.Text (pack)
import Crypto.Hash (hash)

data Entry = Entry {
  site     :: String,
  username :: String,
  password :: String
} deriving (Show)

main :: IO ()
main = do
    key <- readFile "password.enc"
    password <- pwprompt "Enter your password" "$ "
    let pass = mkPassword $ pack password
    let hash = PasswordHash {
        unPasswordHash = pack key
    }
    if checkPassword pass hash == PasswordCheckSuccess
        then startupDialog
        else putStrLn "\nWrong password"

startupDialog :: IO ()
startupDialog = do
    putChar '\n'
    choise <- prompt "\
        \What would you like to do?\n\
        \1. Add a new password\n\
        \2. View a password for a site\n\
        \3. View all passwords\n\
        \4. Exit\
    \" "$ "
    case choise of
        "1" -> addPassword'
        "2" -> putStrLn "2"
        "3" -> putStrLn "3"
        "4" -> putStrLn "exit"
        _   -> putStrLn "Invalid input"

addPassword :: IO ()
addPassword = do
    putStrLn "Enter the site name"
    putStr "Site: "
    hFlush stdout
    site <- getLine
    putChar '\n'
    putStrLn "Enter the username"
    putStr "Username: "
    username <- getLine
    putStrLn "Enter the password"
    putStr "Password: "
    password <- getLine
    putStrLn "--"
    encryptAndSave Entry {
        site = site,
        username = username,
        password = password
    }

addPassword' :: IO ()
addPassword' = do
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
    where
        encryptedPassword :: String
        encryptedPassword = password entry
        encryptedEntry :: String
        encryptedEntry = site entry ++ ":" ++  username entry ++ ":" ++ encryptedPassword