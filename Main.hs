module Main (main) where

import qualified Stream as Stream
import qualified Bug as Bug

main :: IO ()
main = Stream.drain $ Bug.fromStreamVar infiniteStream

infiniteStream :: Bug.SVar IO Int
infiniteStream = Bug.SVar $ return $ fmap Bug.ChildYield [1]
