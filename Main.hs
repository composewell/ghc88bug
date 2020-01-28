{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                #-}

module Main (main) where

import Control.Concurrent (ThreadId)
import Control.Exception (fromException)
import Control.Exception (SomeException(..), Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Prelude hiding (map, mapM, concatMap, foldr)

newtype Stream m a =
    MkStream (forall r.
              (a -> Stream m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

instance IsStream Stream where
    toStream = id
    fromStream = id

-- removing the inline phase makes the problem disappear
{-# INLINE [1] mkStream #-}
mkStream :: IsStream t
    => (forall r. (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \yld sng stp ->
    let yieldk a r = yld a (toStream r)
     in k yieldk sng stp

drain :: (Monad m) => Stream m a -> m ()
drain m = go m
    where
    go (MkStream k) =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go r
        in k yieldk single stop

data ChildEvent a =
      ChildYield a
    | ChildStop (Maybe SomeException)

data SVar m a = SVar
    { readOutputQ    :: m [ChildEvent a]
    }

type MonadAsync m = (MonadIO m, MonadThrow m)

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar m a -> Stream m a
fromStreamVar sv = mkStream $ \yld sng stp -> do
    list <- readOutputQ sv
    let MkStream k = processEvents $ list
     in k yld sng stp

    where

    processEvents [] = MkStream $ \yld sng stp -> do
        let MkStream k = fromStreamVar sv
         in k yld sng stp

    processEvents (ev : es) = MkStream $ \yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop e -> do
                case e of
                    Nothing -> do
                        stop <- return True
                        if stop
                        then stp
                        else
                            let MkStream k = rest
                             in k yld sng stp
                    Just ex -> throwM ex

infiniteStream :: SVar IO Int
infiniteStream = SVar $ return $ fmap ChildYield [1]

main :: IO ()
main = drain $ fromStreamVar infiniteStream
