module Main where

type Str = [Char]

main :: IO ()
main = do
    putStrLn "What would you like to do?"
    putStrLn "1. Add a new password"
    putStrLn "2. View a password for a site"
    putStrLn "3. View all passwords"
    putStrLn "4. Exit"
    choise <- getLine
    case choise of
        "1" -> addPassword
        "2" -> putStrLn "2"
        "3" -> putStrLn "3"
        "4" -> putStrLn "exit"
        _   -> putStrLn "Invalid input"

addPassword :: IO ()
addPassword = do
    putStrLn "Enter the site name"
    site <- getLine
    putStrLn "Enter the username"
    username <- getLine
    putStrLn "Enter the password"
    password <- getLine
    print "--"
    encryptAndSave site username password

encryptAndSave :: Str -> Str -> Str -> IO ()
encryptAndSave site username password = do
    let encryptedPassword = password
    let newPassword = site ++ ":" ++  username ++ ":" ++ encryptedPassword
    appendFile "passwords" $ newPassword ++ "\n"
    putStrLn "Password saved"