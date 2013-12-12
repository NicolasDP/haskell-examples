import Options.Applicative

data OptionsData = OptionsData
  { fiboNumber    :: Int
  , factorNumber  :: Int
  , prefixMessage :: String }

sample :: Parser OptionsData
sample = OptionsData
  <$> option
      ( long "fibo"
     <> short 'f'
     <> metavar "n"
     <> value (-1)
     <> help "print the result of Fibonacci n where 0 <= n < 10")
  <*> option
      ( long "factor"
     <> short '!'
     <> metavar "n"
     <> value (-1)
     <> help "print the result of !n where 0 <= n < 10")
  <*> strOption
      ( long "prefix"
     <> metavar "PREFIX"
     <> help "add a prefix message at the output" )

factor :: Int -> Int
factor 0 = 1
factor 1 = 1
factor n = n * (factor $ n - 1)

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

doDispatchOptions :: OptionsData -> IO ()
doDispatchOptions (OptionsData (-1) (-1) _)      = return ()
doDispatchOptions (OptionsData f    (-1) prefix) = putStrLn $ prefix ++ " " ++ results
  where results | f >= 0 && f < 10 = "fibo(" ++ (show f) ++ ") = " ++ (show $ fibo f)
                | otherwise        = "unable to compute fibonacci for n = " ++ (show f)
doDispatchOptions (OptionsData (-1) f    prefix) = putStrLn $ prefix ++ " " ++ results
  where results | f >= 0 && f < 10 = "!" ++ (show f) ++ " = " ++ (show $ factor f)
                | otherwise        = "unable to compute !n = " ++ (show f)
doDispatchOptions (OptionsData f    n    prefix) = do
  doDispatchOptions $ OptionsData f (-1) prefix
  doDispatchOptions $ OptionsData (-1) n prefix

main :: IO ()
main = execParser opts >>= doDispatchOptions
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "print the result of fibonacci or of a factorial"
     <> header "getopt example!" )
