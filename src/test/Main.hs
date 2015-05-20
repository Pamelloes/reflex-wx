{-|
Module      : Main
Description : An example program using reflex-wx
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Main where

import Reflex.WX
import Graphics.UI.WX (text,Frame)

program :: (MonadComponent t m) => m (Component t (Frame ()),())
program = frame [text := "swag"] $ do
  b <- button [text := "click"]
  cmd <- command b
  ct <- count cmd
  dyn <- mapDyn (show) ct
  staticText [text :~ dyn]
  return ()

main :: IO ()
main = host $ fmap fst program
