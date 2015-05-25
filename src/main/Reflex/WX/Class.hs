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
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Reflex.WX.Class ( Attr (..)
                       , Prop (..)
                       , Component (..)
                       , AnyWindow (..)
                       , MonadComponent (..)
                       , towp
                       , wrapEvent
                       ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State hiding (get)

import Data.Dependent.Sum
import qualified Data.Dynamic as D
import Data.Typeable

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.Host.Class hiding (fireEvents)

data Attr t w a = Attr {
  get :: forall m. MonadComponent t m => Component t w -> m (Dynamic t a),
  at  :: W.Attr w a
}
data Prop t w = forall a. Typeable a => Attr t w a := a
              | forall a. Typeable a => Attr t w a :~ Dynamic t a

newtype Component t w = Component (w,[Prop t w])

data AnyWindow = forall w. AW (W.Window w)

class (Reflex t, MonadIO m, MonadHold t m, MonadReflexCreateTrigger t m
      ,MonadFix m) => MonadComponent t m | m -> t where
  askParent       :: m AnyWindow
  addIOEvent      :: Event t (IO ()) -> m ()

  pushComponents  ::  AnyWindow -> m ()
  setLayout       :: ([W.Layout] -> W.Layout) -> m ()
  addComponent    :: (W.Widget w) => Component t w -> m ()
  popComponents   :: m (W.Layout)

  fireEvents      :: (a -> [DSum (EventTrigger t)]) -> m (a -> IO ())

find :: (Typeable a, Typeable t, Reflex t) =>
         W.Attr w a -> [Prop t w] -> Maybe (Either a (Dynamic t a))
find _ [] = Nothing
find a ((b := x):r)
  | n == m = case D.fromDynamic (D.toDyn x) of
               Just x  -> Just (Left x)
               Nothing -> find a r
  where n = W.attrName a
        m = W.attrName (at b)
find a ((b :~ x):r)
  | n == m = case D.fromDynamic (D.toDyn x) of
               Just x  -> Just (Right x)
               Nothing -> find a r
  where n = W.attrName a
        m = W.attrName (at b)
find a (_:r) = find a r
{-
get :: forall a t m w. (Typeable a, Typeable t, MonadComponent t m) => 
       W.Attr w a -> Component t w -> m (Behavior t a)
get a (Component (w,p)) = do
  let name = W.attrName a

  let walk :: [Prop t w] -> Maybe (Behavior t a)
      walk [] = Nothing
      walk ((b := x):r)
        | name == n = case D.fromDynamic (D.toDyn x) of
                        Just x  -> Just (constant x)
                        Nothing -> walk r
        where n  = W.attrName b
      walk ((b :~ x):r)
        | name == n = case D.fromDynamic (D.toDyn x) of
                        Just x  -> Just x
                        Nothing -> walk r
        where n  = W.attrName b
      walk (_:r) = walk r
  case walk p of
    Just mv -> return mv
    Nothing -> do
                 c <- liftIO $ W.get w a
                 return $ constant c
-}

towp :: MonadComponent t m => w -> Prop t w -> m (W.Prop w)
towp w (a := v) = return $ (at a) W.:= v
towp w (a :~ v) = do
  addIOEvent $ fmap (\x -> W.set w [(at a) W.:= x]) (updated v)
  cv <- sample $ current v
  return $ (at a) W.:= cv

wrapEvent :: forall t m w. MonadComponent t m => 
             W.Event w (IO ()) -> Component t w -> m (Event t ()) 
wrapEvent e (Component (w,_)) = do
  {-let k=hash (e,w)
  h <- ComponentM $ gets (\(a@ComponentState{eventMap=e}) -> M.member k e)
  if h then
    ComponentM $ gets (\(ComponentState{eventMap=e}) -> event (e M.! k) )
  else do-}
    let fire :: EventTrigger t () -> [DSum (EventTrigger t)]
        fire et = [et :=> ()]
    f <- fireEvents fire
    n <- newEventWithTrigger $ \et -> do
           W.set w [W.on e W.:= f et]
           return $ W.set w [W.on e W.:= W.propagateEvent]
    --ComponentM $ modify (\(a@ComponentState{eventMap=e})
    --                      ->a{eventMap=M.insert k (AnyEvent n) e})
    return n
