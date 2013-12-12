import Options.Applicative
import System.Directory
import Data.List

data OptionsData = OptionsData
  { optionArg :: String }

sample :: Parser OptionsData
sample = OptionsData
  <$> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "PATH"
     <> help "print the @PATH@'s content tree")

doDispatch :: String -> String-> [String] -> IO ()
doDispatch _      _    []        = return ()
doDispatch indent path (('.':_):xs) = doDispatch indent path xs
doDispatch indent path (a   :xs) = do
  putStrLn $ indent ++ a
  isDir <- doesDirectoryExist $ path ++ "/" ++ a
  if isDir then doDispatch (" |--" ++ indent) (path ++ "/" ++ a) =<< getDirectoryContents (path ++ "/" ++ a) else return ()
  doDispatch indent path xs

doTree :: OptionsData -> IO ()
doTree a =
  getDirectoryContents (optionArg a) >>= doDispatch ""  (optionArg a)

main :: IO ()
main = execParser opts >>= doTree
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "show the directory tree"
     <> header "treeh - a mini-tree in Haskell" )
