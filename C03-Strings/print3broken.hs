module Print3Broken where

printSecond :: IO ()
printSecond = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
      --where greeting = "Yarrrrr"

greeting = "Yarrrrr"
