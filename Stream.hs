{-# LANGUAGE RankNTypes                #-}

module Stream
    ( Stream (..)
    , mkStream
    , foldStream
    , drain
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
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
