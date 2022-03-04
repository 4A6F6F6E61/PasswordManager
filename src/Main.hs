module Main where

type Str = [Char]

main :: IO ()
main = do
    --let pass = mkPassword "ejYrkGnzPqZM1!"
    --passHash <- hashPassword pass
    --print passHash
    putStrLn "Enter your password"
    password <- getLine
    if password == "password"
        then startupDialog
        else putStrLn "Wrong password"

startupDialog :: IO ()
startupDialog = do
    putStrLn "\
        \What would you like to do?\n\
        \1. Add a new password\n\
        \2. View a password for a site\n\
        \3. View all passwords\n\
        \4. Exit\
    \"
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
    putStrLn "--"
    encryptAndSave site username password

encryptAndSave :: Str -> Str -> Str -> IO ()
encryptAndSave site username password =
    do
        appendFile "passwords.enc" $ newPassword ++ "\n"
        putStrLn "Password saved"
    where
        encryptedPassword :: Str
        encryptedPassword = password
        newPassword :: Str
        newPassword = site ++ ":" ++  username ++ ":" ++ encryptedPassword