import System.Environment

printUsage :: String -> IO ()
printUsage progName = do
  putStrLn $ "Usage: " ++ progName ++ " [help] [fibo x])"
  putStrLn "       where x is a positive interger lesser than 9"

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

doFibonacci :: String -> IO ()
doFibonacci x = do
  let i = read x :: Int
  if (i >= 0 && i < 10)
    then putStrLn $ show $ fibo i
    else printUsage =<< getProgName

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  case args of
    ("help":_  ) -> printUsage name
    ("fibo":x:_) -> doFibonacci x
    _            -> return ()
