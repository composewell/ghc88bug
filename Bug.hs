{-# LANGUAGE FlexibleContexts #-}

module Bug
    ( fromStreamVar
    )
where

import Control.Exception (fromException)
import Control.Monad.Catch (throwM)

import SVar
    ( MonadAsync,
      SVar(..),
      ChildEvent(ChildStop, ChildYield),
      ThreadAbort(ThreadAbort))
import Stream ( Stream, mkStream, foldStream )

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
