module Main where
import System.Exit
import Control.Concurrent
phi = 1.6180339887498948482045868343656381 
e = 2.71828182845904523536028747135266249 
clear :: IO ()
clear = putStr "\ESC[2J"

factorial :: Int -> Int
factorial x = if x == 0 then 1 else x*factorial (x-1)
gammaFunction :: Int -> Double
gammaFunction x = if x == 0 then 1 else (fromIntegral x) * gammaFunction (x-1)
translateToBinary :: Int -> [Int]
translateToBinary x = if x == 0 then [] else (translateToBinary (x `div` 2)) ++ [x `mod` 2]

enterRtn :: IO ()
enterRtn = do
    putStrLn "Press enter to return to menu"
    getLine
    menu

programInfo :: IO ()
programInfo = do
    putStrLn "Program Ver 0.01"
    putStrLn "Author: Cohen-Koen"
    putStrLn "This program is a collection of useful math functions and et ceterra"
    enterRtn

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
    enterRtn

firstTenMultiples :: Int -> [Int]
firstTenMultiples 0 = return 0
firstTenMultiples x = [x, x*2..x*10]


askFact :: IO ()
askFact = do
    putStrLn "n! where n =:"
    n <- getLine
    let n' = read n :: Int -- this line fixes the whole thing...
    putStrLn ("The factorial of " ++ n ++ " is: " ++ show (factorial n'))   
    enterRtn

askTrans :: IO ()
askTrans = do
    putStrLn "Enter a number to translate to binary:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The binary representation of " ++ n ++ " is: " ++ show (translateToBinary n'))
    enterRtn



askGamma = do
    putStrLn "Enter a number to calculate the gamma function:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The gamma function of " ++ n ++ " is: " ++ show (gammaFunction n'))
    enterRtn

specificFunctionsMenu :: IO()
specificFunctionsMenu = do
    putStrLn ("What function would you like to use?")
    putStrLn ("1. Factorial")
    putStrLn ("2. Binary Translation")
    putStrLn ("3. Gamma Function")
    putStrLn ("4. Back")
    n <- getLine
    let n' = read n :: Int
    if n' == 1 then
        askFact
    else if n' == 2 then
        askTrans
    else if n' == 3 then
        askGamma
    else if n' == 4 then
        menu
    else putStrLn ("Invalid input")
    



usefulConstants :: IO ()
usefulConstants = do
    putStrLn "What constant do you want to know?"
    putStrLn "1. Euler's number"
    putStrLn "2. Pi"
    putStrLn "3. Tau"
    putStrLn "4. Golden Ratio"
    putStrLn "5. Back"
    n <- getLine
    let n' = read n :: Int
    clear
    if n' == 1 then do
        putStrLn "e is approximately \n"
        print(show(e))
        enterRtn
        
    else if n' == 2 then do
        putStrLn "Pi is approximately \n"
        print(show(pi))
        enterRtn
        
    else if n' == 3 then do
        putStrLn "Tau is approximately \n"
        print(show(pi*2))
        enterRtn
        
    else if n' == 4 then do
        putStrLn "The golden ratio is approximately \n"
        print(show(phi))
        enterRtn
        
    else if n' == 5 then menu
    else putStrLn "Invalid input"
    


menu :: IO ()
menu = do           -- DO is a monad, which is looping through
    clear
    putStrLn "Which Task would you like to run"
    putStrLn "1. Stats of a number"
    putStrLn "2. Useful Constants"
    putStrLn "3. Specific Functions"
    putStrLn "4. Program Info"
    putStrLn "5. Exit"
    n <- getLine
    let n' = read n :: Int
    clear
    if n' == 1 then statsOfNumber
    else if n' == 2 then usefulConstants
    else if n' == 3 then specificFunctionsMenu
    else if n' == 4 then programInfo
    else if n' == 5 then do
        clear
        exitWith (ExitSuccess)
    else putStrLn "Invalid input"
    menu

main :: IO ()
main = do
    menu


