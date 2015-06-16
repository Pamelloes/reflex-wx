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

data AnyComp t = forall w. (W.Widget w) => AC (Widget t w)
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

data WidgetState t = WidgetState {
  mvar    :: TChan [DSum (EventTrigger t)],
  parent  :: AnyWindow,
  parents :: [AnyWindow],
  ioEvent :: [Event t (IO ())],
  ecache  :: [ECRec t]
}

newtype WidgetM t m a = WidgetM { 
  unWM :: StateT (WidgetState t) m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadFix, Typeable)
instance MonadSample t m => MonadSample t (WidgetM t m) where
  sample = WidgetM . lift . sample
instance MonadHold t m => MonadHold t (WidgetM t m) where
  hold a = WidgetM . lift . (hold a)
instance MonadReflexCreateTrigger t m =>
         MonadReflexCreateTrigger t (WidgetM t m) where
  newEventWithTrigger = WidgetM . lift . newEventWithTrigger
{-
instance MonadReflexHost t m => MonadReflexHost t (WidgetM t m) where
  fireEventsAndRead dm a = WidgetM . lift $ fireEventsAndRead dm a
  subscribeEvent         = WidgetM . lift . subscribeEvent
  runFrame               = WidgetM . lift . runFrame
  runHostFrame           = WidgetM . lift . runHostFrame
-}
instance (Typeable t, Reflex t, MonadIO m, MonadHold t m
         ,MonadReflexCreateTrigger t m, MonadFix m
         ) => MonadWidget t (WidgetM t m) where
  askParent        = do
                       WidgetState{parent=p} <- WidgetM get
                       return p
  pushParent p     = WidgetM $ modify (\(s@WidgetState{
                                            parent=q,
                                            parents=ps
                                        }) -> s{parent=p,parents=q:ps})
  popParent        = do
                       s@(WidgetState{parent=q,parents=p:ps}) <- WidgetM get
                       WidgetM $ put s{parent=p,parents=ps}
                       return q

  addIOEvent e     = WidgetM $ modify (\(s@WidgetState{ioEvent=i}) ->
                                        s{ioEvent=e:i})
  cacheEvent e w d = do
                       s@(WidgetState{ecache=ec}) <- WidgetM get
                       case findEvent e (wxwidget w) ec of
                         Just ev -> return ev
                         Nothing -> do
                                      a <- d
                                      WidgetM $ put s{
                                        ecache = (ECR (e,wxwidget w) a):ec
                                      }
                                      return a
  fireEvents f     = do
                       WidgetState{mvar=v} <- WidgetM get
                       return $ \a -> atomically $ writeTChan v (f a)

whileM :: Monad m => (m Bool) -> m a -> m ()
whileM c l = do
  v <- c
  if v then l >> whileM c l
  else return ()

host :: WidgetM Spider (HostFrame Spider) a -> IO ()
host w = W.start $ do
  runSpiderHost $ do
    v <- liftIO $ atomically newTChan
    let istate = WidgetState v undefined [] [] []

    (_,s) <- runHostFrame $ runStateT (unWM w) istate
    let WidgetState{ioEvent = ie} = s

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
