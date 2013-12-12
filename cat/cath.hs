import Options.Applicative
import System.Directory
import Data.List

data OptionsData = OptionsData
  { optionCountLine :: Bool
  , optionEndOfLine :: Bool
  , optionArgs      :: [String] }

sample :: Parser OptionsData
sample = OptionsData
  <$> switch
      ( short 'n'
     <> help "show the line number at the begin of each line")
  <*> switch
      ( short 'e'
     <> help "show the end of line caracter as a '$' at the end of each line")
  <*> many (argument str
      ( metavar "FILES..." ))

doCat :: Int -> OptionsData -> IO ()
doCat lineCounter a = do
  mapM_ showContent $ optionArgs a
  where
    showLine :: String -> String
    showLine line =
      if (optionEndOfLine a) then line ++ "$" else line

    showContent :: String -> IO ()
    showContent file = do
      content <- readFile file
      let linesOfFile = if (optionCountLine a)
                           then zipWith (++) (map (\x -> "    " ++ show x ++ "  ") [1..]) $ lines content
                           else lines content
      mapM_ (putStrLn . showLine)  linesOfFile

main :: IO ()
main = execParser opts >>= doCat 0
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "show the content of a file"
     <> header "cath - a cat in Haskell" )
