module Main (main) where

import qualified Stream as Stream
import qualified Bug as Bug
import qualified SVar as SV

main :: IO ()
main = Stream.drain $ Bug.fromStreamVar infiniteStream

infiniteStream :: SV.SVar IO Int
infiniteStream = SV.SVar $ return $ fmap SV.ChildYield [1]
