import System.Environment
import Control.Monad
import Control.Exception

import Prelude hiding(catch)
import Sinterp
import Evaluator
import Parser
import Data.List

class Pretty a where
  pretty :: a -> String

instance Pretty Value where
  pretty (NumberV n) = pretty n
  pretty (BooleanV b) = show b
  pretty (FunV _ _ _) = "<function>"
  pretty (ListV xs) = "[" ++ (intercalate "," (map pretty xs)) ++ "]"
  
instance Pretty Numeric where  
  pretty (IntN n) = show n
  pretty (FloatN n) = show n

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    fs -> mapM_ (run <=< readFile) fs
    
run input = do      
  let parsed= parse input
  case parsed of
      Left err -> putStrLn err
      Right expr -> 
        catch (let eval' = eval expr in putStrLn . pretty $ eval') ((\msg -> putStrLn (show msg)) :: Control.Exception.SomeException -> IO ())    
      
repl = do
  putStrLn "Ctrl-C to exit"
  forever $ do
    putStr "> "
    getLine >>= run
    