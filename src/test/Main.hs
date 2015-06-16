{-|
Module      : Main
Description : An example program using reflex-wx
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes, RecursiveDo #-}
module Main where

import Data.Monoid

import Reflex.WX

--program :: (MonadWidget t m) => m ()
program = do 
  frame [text := "Test Application"] $ do
    t <- textCtrl []
    b <- button [ text := "Click" ]
    return $ column 0 [ minsize (sz 250 50) $ widget t
                      , glue
                      , widget b
                      ]
  return ()

main :: IO ()
main = host program
