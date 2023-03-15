module Main where

x = [1..1000]
y = [1/x^z | x <- [1..1000]]
z = 2

main :: IO ()
main = do
    --   putStrLn "Hello, Haskell!"
    print x
    print "-------------------------------------------"
    print y

    
