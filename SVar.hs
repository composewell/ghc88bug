{-# LANGUAGE CPP                        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}

module SVar
    ( MonadAsync
    , SVar (..)
    , ChildEvent(ChildStop, ChildYield)
    , ThreadAbort(ThreadAbort)
    )
where

import Control.Concurrent
       (ThreadId)
import Control.Exception
       (SomeException(..), Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control
       (MonadBaseControl)

data ThreadAbort = ThreadAbort deriving Show

instance Exception ThreadAbort

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

data SVar m a = SVar
    { readOutputQ    :: m [ChildEvent a]
    }

type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)
