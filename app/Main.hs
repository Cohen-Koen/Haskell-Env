module Main where

import System.Exit
import Control.Concurrent

clear :: IO ()
clear = putStr "\ESC[2J"

factorial :: Int -> Int
factorial x = if x == 0 then 1 else x*factorial (x-1)
gammaFunction :: Int -> Double
gammaFunction x = if x == 0 then 1 else (fromIntegral x) * gammaFunction (x-1)
translateToBinary :: Int -> [Int]
translateToBinary x = if x == 0 then [] else (translateToBinary (x `div` 2)) ++ [x `mod` 2]


statsOfNumber :: IO ()
statsOfNumber = do
    clear
    putStrLn "Enter a number:"
    n <- getLine
    let n' = read n :: Int
    clear
    putStrLn ("The factorial of " ++ n ++ " is: " ++ show (factorial n'))
    putStrLn ("The binary representation of " ++ n ++ " is: " ++ show (translateToBinary n'))
    putStrLn ("The gamma function of " ++ n ++ " is: " ++ show (gammaFunction n'))
    putStrLn ("The first ten multiples of " ++ n ++ " are: " ++ show (firstTenMultiples n'))
    threadDelay 5000000
    clear
    menu

firstTenMultiples :: Int -> [Int]
firstTenMultiples x = [x, x*2..x*10]


askFact :: IO ()
askFact = do
    putStrLn "n! where n =:"
    n <- getLine
    let n' = read n :: Int -- this line fixes the whole thing...
    putStrLn ("The factorial of " ++ n ++ " is: " ++ show (factorial n'))   
    menu

askTrans :: IO ()
askTrans = do
    putStrLn "Enter a number to translate to binary:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The binary representation of " ++ n ++ " is: " ++ show (translateToBinary n'))
    menu



askGamma = do
    putStrLn "Enter a number to calculate the gamma function:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The gamma function of " ++ n ++ " is: " ++ show (gammaFunction n'))
    menu

menu :: IO ()
menu = do           -- DO is a monad, which is looping through
    clear
    putStrLn "Which Task would you like to run"
    putStrLn "1. Stats of a number"
    putStrLn "2. Generate a value table from function"
    putStrLn "3. Program Info"
    putStrLn "4. Exit"
    n <- getLine
    let n' = read n :: Int
    if n' == 1 then statsOfNumber
    -- else if n' == 2 then valueTable
    -- else if n' == 3 then programInfo
    else if n' == 4 then do
        clear
        exitWith (ExitSuccess)
    else putStrLn "Invalid input"
    menu

main :: IO ()
main = do
    menu


