{-# LANGUAGE MultiWayIf #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import System.Directory
import System.Exit
import System.FilePath
import System.Process

guardEither :: (Monad m) => String -> Bool -> EitherT String m ()
guardEither err b = if b then return () else left err

runPrintErr :: (MonadIO m) => EitherT String m r -> m ()
runPrintErr act = runEitherT act >>= (\x -> case x of 
                                              Left err -> liftIO $ putStrLn err
                                              Right _ -> return () )

data Action = Tarball FilePath
            | Build 
            deriving Show

main :: IO ()
main = do 
    putStrLn "new pipeline"
    tvar <- atomically $ newTVar []
    forkIO $ forever $ do
      input <- getLine 
      case words input of
        x:xs -> do
          if | x == "tarball" -> atomically $ putQueue tvar (Tarball (concat xs))
             | x == "build" -> atomically $ putQueue tvar Build
             | otherwise -> return ()
        [] -> return ()        
    forever $ worker4us tvar >> threadDelay 1000000
    return ()

putQueue :: TVar [a] -> a -> STM ()
putQueue tvar x = do       
    xs <- readTVar tvar
    writeTVar tvar (xs ++ [x])


getQueue :: TVar [a] -> STM a
getQueue tvar = do 
    mx <- readTVar tvar
    case mx of
      [] -> retry
      x:xs -> writeTVar tvar xs >> return x 

worker4us :: TVar [Action] -> IO ()
worker4us tvar = do
  x <- atomically (getQueue tvar)
  case x of
    Tarball fp -> runPrintErr (tarballOp fp)
    Build -> runPrintErr buildOp

tarballOp :: FilePath -> EitherT String IO ()
tarballOp fp = do
    cdir <- liftIO getCurrentDirectory
    tdir <- liftIO getTemporaryDirectory
    liftIO $ setCurrentDirectory tdir
    (excode,sout,serr) <- liftIO $ readProcessWithExitCode "tar" [ "xvzf", fp ] ""
    guardEither "failure during tar" (excode == ExitSuccess)
    liftIO $ print sout
    liftIO $ setCurrentDirectory cdir

buildOp :: EitherT String IO ()
buildOp = do
    liftIO $ putStrLn "build!"

