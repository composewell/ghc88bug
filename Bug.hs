{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                #-}

module Bug
    ( fromStreamVar
    , SVar(..)
    , ChildEvent(..)
    , drain
    )
where

import Control.Concurrent (ThreadId)
import Control.Exception (fromException)
import Control.Exception (SomeException(..), Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import Prelude hiding (map, mapM, concatMap, foldr)

newtype Stream m a =
    MkStream (forall r.
              (a -> Stream m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

{-# INLINE [2] mkStream #-}
mkStream :: IsStream t
    => (forall r. (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \yld sng stp ->
    let yieldk a r = yld a (toStream r)
     in k yieldk sng stp

{-# INLINE [1] yieldM #-}
yieldM :: (Monad m) => m a -> Stream m a
yieldM m = MkStream $ \_ single _ -> m >>= single

{-# INLINE foldStream #-}
foldStream
    :: IsStream t
    => (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStream yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        MkStream k = toStream m
     in k yieldk sng stp

{-# INLINE map #-}
map :: (a -> b) -> Stream m a -> Stream m b
map f xs = go xs
    where
      go m = MkStream $ \yld sng stp ->
                 let yieldk a r = yld (f a) (go r)
                     single a   = sng (f a)
                 in  foldStream yieldk single stp m
{-# INLINE [1] drain #-}
drain :: (Monad m, IsStream t) => t m a -> m ()
drain m = go m
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go r
        in foldStream yieldk single stop m1


instance Monad m => Functor (Stream m) where
    fmap = map

instance MonadTrans Stream where
    lift = yieldM

class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

instance IsStream Stream where
    toStream = id
    fromStream = id

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
