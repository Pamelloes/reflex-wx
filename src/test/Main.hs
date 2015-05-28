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
        ct <- count (mergeWith (<>) [cmd,c])
        dyn <- mapDyn (("Presses: " ++).show) ct
        t <- label [text :~ dyn]
        (_,c) <- panel [] $ do
                   setLayout (W.column 10)
                   b1 <- button [text := "Click me!"]
                   b2 <- button [text := "Me, too!"]
                   c1 <- command b1
                   c2 <- command b2
                   return (mergeWith (<>) [c1,c2])
        t' <- get text t
        label [text :~ t']
    return ()
  return ()

main :: IO ()
main = host program
