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

import qualified Graphics.UI.WX as W

import Reflex.WX

--program :: (MonadComponent t m) => m ()
program = do 
  frame [text := "Test Application"] $ do
    rec b <- button [text := "Click"]
        cmd <- command b
        d1 <- mapDyn (("Presses: " ++).show) =<< count (mergeWith (<>) [cmd,c])
        d2 <- mapDyn (("Presses: " ++).show) =<< count (mergeWith (<>) [cmd,d])
        t <- label [text :~ d1]
        s <- label [text :~ d2]
        (_,(c,d)) <- panel [] $ do
                   setLayout (W.column 10)
                   b1 <- button [text := "Click me!"]
                   b2 <- button [text := "Me, too!"]
                   c1 <- command b1
                   c2 <- command b2
                   d1 <- command b1
                   d2 <- command b2
                   return (mergeWith (<>) [c1,c2], mergeWith (<>) [d1,d2])
        e <- entry [ text := "Maybe" ]
    t1 <- get text t
    t2 <- get text e
    label [text :~ t1]
    label [text :~ t2]
    return ()
  return ()

main :: IO ()
main = host program
