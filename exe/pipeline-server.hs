{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- server program for pipeline
--
-----------------------------------------------------------------------------

module Main where

import Data.Monoid (mconcat) 
import Web.Scotty 


main :: IO ()
main = do 
  putStrLn "starting pipeline-server"

  scotty 3000 $ do 
    get "/:word" $ do 
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

