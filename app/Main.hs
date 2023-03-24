module Main where

factorial :: Int -> Int
factorial x = if x == 0 then 1 else x*factorial (x-1)



askFact = do
    putStrLn "n! where n =:"
    n <- getLine
    let n' = read n :: Int -- this line fixes the whole thing...
    putStrLn (show (factorial n'))   



main :: IO ()
main = do
    askFact
