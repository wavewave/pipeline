module Main where

import Data.Char
import Control.Monad.Identity
import HEP.Util.Coroutine

type Question = String 
data Answer = Y | N deriving Eq

type Expert a = CoroutineT Answer Question Identity a 

data Fruit 
  = Apple 
  | Kiwifruit
  | Banana
  | Orange 
  | Lemon 
  deriving Show 

identifyFruit :: Expert Fruit
identifyFruit = do 
  yellow <- yield "Is it yellow?"
  if yellow == Y 
    then do 
      long <- yield "Is it long?"
      if long == Y 
        then return Banana 
        else return Lemon
    else do 
      orange <- yield "Is it orange?"
      if orange == Y 
        then return Orange 
        else do 
          fuzzy <- yield "Is it fuzzy?"
          if fuzzy == Y 
            then return Kiwifruit
            else return Apple

main :: IO ()
main = do 
    putStrLn "pipeline tester"
    run identifyFruit
 where
    run :: Expert Fruit -> IO () 
    run exp = handle $ runIdentity $ runCoroutineT exp
   
    handle (Yield q cont) = do 
      putStrLn q 
      l <- getLine 
      case map toLower l of 
        "y" -> run $ cont Y 
        "yes" -> run $ cont Y 
        "n" -> run $ cont N 
        "no" -> run $ cont N 
        _ -> putStrLn "Plesae answer 'yes' or 'no'" >> handle (Yield q cont)

    handle (Result fruit) = do 
      putStrLn $ "The fruit you have is: " ++ show fruit
