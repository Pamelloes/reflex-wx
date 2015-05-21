{-|
Module      : Main
Description : An example program using reflex-wx
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes, RecursiveDo #-}
module Main where

import Reflex.WX
import qualified Graphics.UI.WX as W

--program :: (MonadComponent t m) => m ()
program = do 
  frame [W.text := "$wag"] $ do
    b <- button [W.text := "click"]
    cmd <- command b
    ct <- count cmd
    dyn <- mapDyn (show) ct
    staticText [W.text :~ dyn]
    staticText [W.text := "meh"]
    panel [] $ do
      setLayout (W.column 10)
      button [W.text := "click me!"]
      button [W.text := "me, too!"]
    return ()
  frame [W.text := "Bitchez"] $ do
    button [W.text := "I'm amazing"]
  return ()

main :: IO ()
main = host program
