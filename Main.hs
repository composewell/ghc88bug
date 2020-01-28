{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                #-}

module Main (main) where

import Control.Exception (SomeException(..))
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))

newtype Stream m a =
    MkStream (forall r.
              (a -> Stream m a -> m r) -- yield
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
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \yld stp ->
    let yieldk a r = yld a (toStream r)
     in k yieldk stp

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
fromStreamVar sv = mkStream $ \yld stp -> do
    list <- readOutputQ sv
    let MkStream k = processEvents $ list
     in k yld stp

    where

    processEvents [] = MkStream $ \yld stp -> do
        let MkStream k = fromStreamVar sv
         in k yld stp

    processEvents (ev : es) = MkStream $ \yld stp -> do
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
                             in k yld stp
                    Just ex -> throwM ex

drain :: (Monad m) => Stream m a -> m ()
drain m = go m
    where
    go (MkStream k) =
        let stop = return ()
            yieldk _ r = go r
        in k yieldk stop

infiniteStream :: SVar IO Int
infiniteStream = SVar $ return $ fmap ChildYield [1]

main :: IO ()
main = drain $ fromStreamVar infiniteStream
