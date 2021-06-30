-- main = do
--     ch <- getLine
--     if ch /= "y"
--         then return()
--         else main
-- import System.Random

-- num = randomIO :: IO Int

-- main = do
--     putStr "Player 1 Enter: "
--     a <- getLine
--     putStr "Player 2 Enter: "
--     b <- getLine
--     y = do 
--         if (a == b)
--             then do_true
--             else y
--     y

-- do_true = do
--     putStrLn $ "You Win!!!!"

-- do_false = do
--     putStrLn $ "You Lose!!!! Try again!!"
--     main

nameReturnAndCarryOn = do putStr "What is your first name? "
                          first <- getLine
                          putStr "And your last name? "
                          last <- getLine
                          let full = first++" "++last
                          putStrLn ("Pleased to meet you, "++full++"!")
                          return full
                          putStrLn "I am not finished yet!"

-- 1 >> 2 >>= 3 >>= 4 

-- print_player1 = do
--     putStrLn "Player1 input: "
--     a <- getLine
--     return a

-- print_player2 a = do
--     putStrLn "Player2 input: "
--     b <- getLine
--     return $ a == b

-- check_it c = do
--     if (c)
--         then putStrLn $ "Congrats You Win!!!"
--         else
--             do
--                 putStrLn $ "You Lose!!!! Try Again!!!!"
--                 main

-- main = print_player1 >>= print_player2 >>= check_it
 
-- main :: IO ()
-- main = do putStrLn "Try to guess the secret word ([1-100])..."
--           secret <- randomRIO (1, 100)
--           play secret
 
 
-- play :: Int -> IO ()
-- play secret = do guesses <- playGame secret 0
--                  putStrLn $ "You win! You took " ++ show guesses ++ " guesses!"
 
 
-- playGame :: Int -> Int -> IO Int
-- playGame secret guesses = do putStr "? "
--                              input <- getLine
--                              let guess = read input :: Int
--                              if guess == secret then
--                                 do return (guesses + 1)
--                              else if guess < secret then
--                                 do putStrLn "Too small!"
--                                    playGame secret (guesses + 1)
--                              else do putStrLn "Too big!"
--                                      playGame secret (guesses + 1)   

-- Normal Guessing Game
main = do
    putStrLn $ "Player 1 input: "
    a <- getLine
    next a

next a = do
    play a
    putStrLn $ "You Win!!!!"

play a = do
    putStrLn $ "Player 2 input: "
    b <- getLine
    if a == b
        then return 0
        else
            do
                putStrLn $ "try again!!"
                play a

-- Guessing Game with counter
-- main' = do
--     putStrLn $ "Player 1 input: "
--     a <- getLine
--     let save = read a ::Int
--     next' save 3

-- next' :: Int -> Int -> IO ()
-- next' a 3 = do
--     final <- play' a 3 0
--     if  final == "1"
--         then
--             do
--                 putStrLn $ "You Win!!!!"
--         else
--             putStrLn $ "You Lose!!!!"

-- play' :: Int -> Int -> Int -> IO [Char]
-- play' a c g = do
--     putStrLn $ "Player 2 input: "
--     inp <- getLine
--     let b = read inp ::Int
--     if a == b
--         then return "1"
--         else
--             if c /= (g + 1)
--                 then
--                     do
--                         putStrLn $ "try again!!"
--                         play' a c (g + 1)
--                 else
--                     return "0"

-- Guessing game with counter
main' = do
    putStrLn $ "Player 1 input: "
    a <- getLine
    putStrLn $ "Counter Limit: "
    b <- getLine
    let save = read a ::Int
    let save' = read b ::Int
    next' save save'

next' :: Int -> Int -> IO ()
next' a b = do
    final <- play' a b 0
    if  final == "1"
        then
            do
                putStrLn $ "You Win!!!! with " ++ final ++ " guesses"
        else
            putStrLn $ "You Lose!!!!"

play' :: Int -> Int -> Int -> IO [Char]
play' a c g = do
    putStrLn $ "Player 2 input: "
    inp <- getLine
    let b = read inp ::Int
    if a == b
        then return "1"
        else
            if c /= (g + 1)
                then
                    do
                        putStrLn $ "try again!!"
                        play' a c (g + 1)
                else
                    return "0"