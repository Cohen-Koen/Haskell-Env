module Main where

factorial :: Int -> Int
factorial x = if x == 0 then 1 else x*factorial (x-1)
gammaFunction :: Int -> Double
gammaFunction x = if x == 0 then 1 else (fromIntegral x) * gammaFunction (x-1)
translateToBinary :: Int -> [Int]
translateToBinary x = if x == 0 then [] else (translateToBinary (x `div` 2)) ++ [x `mod` 2]

askFact :: IO ()
askFact = do
    putStrLn "n! where n =:"
    n <- getLine
    let n' = read n :: Int -- this line fixes the whole thing...
    putStrLn ("The factorial of " ++ n ++ " is: " ++ show (factorial n'))   

askTrans :: IO ()
askTrans = do
    putStrLn "Enter a number to translate to binary:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The binary representation of " ++ n ++ " is: " ++ show (translateToBinary n'))

askBinary :: IO ()
askGamma = do
    putStrLn "Enter a number to calculate the gamma function:"
    n <- getLine
    let n' = read n :: Int
    putStrLn ("The gamma function of " ++ n ++ " is: " ++ show (gammaFunction n'))

main :: IO ()
main = do
    askFact
    askTrans
    askGamma
    askBinary
