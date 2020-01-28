{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds            #-}

module Bug
    ( fromStreamVar
    , SVar(..)
    , ChildEvent(..)
    )
where

import Control.Exception (fromException)
import Control.Monad.Catch (throwM)

import Control.Concurrent (ThreadId)
import Control.Exception (SomeException(..), Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Stream (Stream, mkStream, foldStream)

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

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar m a -> Stream m a
fromStreamVar sv = mkStream $ \yld sng stp -> do
    list <- readOutputQ sv
    foldStream yld sng stp $ processEvents $ reverse list

    where

    {-# INLINE processEvents #-}
    processEvents [] = mkStream $ \yld sng stp -> do
        foldStream yld sng stp $ fromStreamVar sv

    processEvents (ev : es) = mkStream $ \yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop _ e -> do
                case e of
                    Nothing -> do
                        stop <- return True
                        if stop
                        then stp
                        else foldStream yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                foldStream yld sng stp rest
                            Nothing -> throwM ex
