{-|
Module      : Reflex.WX.Class
Description : This module contains the basic type definitions used within a
              reflex-wx program.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.WX.Class ( Prop (..)
                       , Component (..)
                       , AnyWindow (..)
                       , MonadComponent (..)
                       , towp
                       , wrapEvent
                       ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.Host.Class

data Prop t w = forall a. W.Attr w a := a
              | forall a. W.Attr w a :~ Dynamic t a
newtype Component t w = Component (w,[Prop t w])

data AnyWindow = forall w. AW (W.Window w)

class ( Reflex t, MonadIO m, MonadSample t m, MonadHold t m
      , MonadReflexCreateTrigger t m, MonadFix m) => MonadComponent t m | m -> t where
  askParent      :: m AnyWindow
  addIOEvent     :: Event t (m ()) -> m ()

  pushComponents ::  AnyWindow -> m ()
  setLayout      :: ([W.Layout] -> W.Layout) -> m ()
  addComponent   :: (W.Widget w) => Component t w -> m ()
  popComponents  :: m (W.Layout)

towp :: MonadComponent t m => w -> Prop t w -> m (W.Prop w)
towp w (a := v) = return $ a W.:= v
towp w (a :~ v) = do
  addIOEvent $ fmap (\x -> liftIO $ W.set w [a W.:= x]) (updated v)
  cv <- sample $ current v
  return $ a W.:= cv

wrapEvent :: MonadComponent t m => 
             W.Event w (IO ()) -> Component t w -> m (Event t ())
wrapEvent e (Component (w,_)) = do
  {-let k=hash (e,w)
  h <- ComponentM $ gets (\(a@ComponentState{eventMap=e}) -> M.member k e)
  if h then
    ComponentM $ gets (\(ComponentState{eventMap=e}) -> event (e M.! k) )
  else do-}
    n <- newEventWithTrigger $ \et -> do
           --W.set w [W.on e W.:= fireEvents [ et :=> 
           return $ W.set w [ W.on e W.:= W.propagateEvent ]
    --ComponentM $ modify (\(a@ComponentState{eventMap=e})
    --                      ->a{eventMap=M.insert k (AnyEvent n) e})
    return n
