{-|
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
module Reflex.WX.Internal (host
                          ) where

import Control.Concurrent.STM

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Dependent.Sum
import qualified Data.Dynamic as D
import Data.Typeable

import qualified Graphics.UI.WX as W
import qualified Graphics.UI.WXCore as W hiding (Event)

import Reflex
import Reflex.Host.Class
import Reflex.WX.Class hiding (get)

data AnyComp t = forall w. (W.Widget w) => AC (Component t w)
data ECRec t = forall w a. (Typeable w,Eq w,Typeable (EventMap a)) => 
               ECR (W.Event w a,w) (Event t (EventMap a))

findEvent :: (Typeable w,Eq w,Typeable t,Typeable (EventMap a)) =>
             W.Event w a -> w -> [ECRec t] 
               -> Maybe (Event t (EventMap a))
findEvent e w []     = Nothing
findEvent e w ((ECR (e1,w1) a):rs) 
  | n == m = case D.fromDynamic (D.toDyn (w1,a)) of
               Just (x,y)  -> if w == x then Just y else findEvent e w rs
               Nothing     -> findEvent e w rs
  where n = W.attrName (W.on e)
        m = W.attrName (W.on e1)
findEvent e w (_:rs) = findEvent e w rs

data ComponentState t = ComponentState {
  mvar    :: TChan [DSum (EventTrigger t)],
  parent  :: AnyWindow,
  parents :: [AnyWindow],
  ioEvent :: [Event t (IO ())],
  ecache  :: [ECRec t]
}

type ComponentInternal t m = StateT (ComponentState t) m

newtype ComponentM t m a = ComponentM { 
  unCM :: ComponentInternal t m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadFix, Typeable)
instance MonadSample t m => MonadSample t (ComponentM t m) where
  sample = ComponentM . lift . sample
instance MonadHold t m => MonadHold t (ComponentM t m) where
  hold a = ComponentM . lift . (hold a)
instance MonadReflexCreateTrigger t m =>
         MonadReflexCreateTrigger t (ComponentM t m) where
  newEventWithTrigger = ComponentM . lift . newEventWithTrigger
{-
instance MonadReflexHost t m => MonadReflexHost t (ComponentM t m) where
  fireEventsAndRead dm a = ComponentM . lift $ fireEventsAndRead dm a
  subscribeEvent         = ComponentM . lift . subscribeEvent
  runFrame               = ComponentM . lift . runFrame
  runHostFrame           = ComponentM . lift . runHostFrame
-}
instance (Typeable t, Reflex t, MonadIO m, MonadHold t m
         ,MonadReflexCreateTrigger t m, MonadFix m
         ) => MonadComponent t (ComponentM t m) where
  askParent        = do
                       ComponentState{parent=p} <- ComponentM $ get
                       return p
  pushParent p     = ComponentM $ modify (\(s@ComponentState{
                                               parent=q,
                                               parents=ps
                                           }) -> s{parent=p,parents=q:ps})
  popParent        = do
                       s@(ComponentState{parent=q,parents=p:ps})<-ComponentM$get
                       ComponentM$put s{parent=p,parents=ps}
                       return q

  addIOEvent e     = ComponentM $ modify (\(s@ComponentState{ioEvent=i})->
                                       s{ioEvent=e:i})
  cacheEvent e c d = do
                       s@(ComponentState{ecache=ec}) <- ComponentM $ get
                       case findEvent e (widget c) ec of
                         Just ev -> return ev
                         Nothing -> do
                                      a <- d
                                      ComponentM $ put s{
                                        ecache = (ECR (e,widget c) a):ec
                                      }
                                      return a
  fireEvents f     = do
                       ComponentState{mvar=v} <- ComponentM $ get
                       return $ \a -> atomically $ writeTChan v (f a)

whileM :: Monad m => (m Bool) -> m a -> m ()
whileM c l = do
  v <- c
  if v then l >> whileM c l
  else return ()

host :: ComponentM Spider (HostFrame Spider) a -> IO ()
host c = W.start $ do
  runSpiderHost $ do
    v <- liftIO $ atomically newTChan
    let istate = ComponentState v undefined [] [] []

    (_,s) <- runHostFrame $ runStateT (unCM c) istate
    let ComponentState{ioEvent = ie} = s

    ieh <- subscribeEvent $ mergeWith (>>) ie
    whileM (liftIO W.wxcAppGetTopWindow >>= return . (/=W.objectNull)) $ do
      liftIO $ W.wxcAppYield
      whileM (fmap not $ liftIO $ atomically $ isEmptyTChan v) $ do
        e <- liftIO $ atomically $ readTChan v
        io <- fireEventsAndRead e $ do
                r <- readEvent ieh
                case r of
                  Just f  -> f
                  Nothing -> return $ return ()
        liftIO $ io
    liftIO $ W.wxcAppExit
    return ()
