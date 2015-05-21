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
  frame [W.text := "Test Application"] $ do
    rec b <- button [W.text := "Click"]
        cmd <- command b
        ct <- count (mergeWith (<>) [cmd,c])
        dyn <- mapDyn (("Presses: " ++).show) ct
        staticText [W.text :~ dyn]
        (_,c) <- panel [] $ do
                   setLayout (W.column 10)
                   b1 <- button [W.text := "Click me!"]
                   b2 <- button [W.text := "Me, too!"]
                   c1 <- command b1
                   c2 <- command b2
                   return (mergeWith (<>) [c1,c2])
    return ()
  return ()

main :: IO ()
main = host program
