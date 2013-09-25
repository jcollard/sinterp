module Main(main) where

import Sinterp
import Evaluator
import Parser

import Control.Exception
import Control.Monad
import System.Environment
import System.IO

class Pretty a where
  pretty :: a -> String

instance Pretty Value where
  pretty (NumberV n) = pretty n
  pretty (BooleanV b) = show b
  pretty (FunV _ _ _) = "<function>"
--  pretty (ConsV head tail) = (pretty head) ++ ":" ++ (pretty tail)
  pretty v@(ConsV head tail) = "[" ++ (pretty head) ++ (pretty' tail)
  pretty EmptyV = "[]"

pretty' :: Value -> String
pretty' (ConsV head tail) = "," ++ (pretty' head) ++ (pretty' tail)
pretty' EmptyV = "]"
pretty' v = pretty v
  
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
        catch (let eval' = eval . desugar $ expr in putStrLn . pretty $ eval') ((\msg -> putStrLn (show msg)) :: Control.Exception.SomeException -> IO ())    
      
repl = do
  putStrLn "Ctrl-C to exit"
  forever $ do
    putStr "> "
    hFlush stdout
    getLine >>= run
    
