module Haskore.IOExtensions
  ( readBinaryFile, writeBinaryFile
  ) where

import System.IO

readBinaryFile :: FilePath -> IO String
readBinaryFile f  = hGetContents =<< openBinaryFile f ReadMode

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f s = do h <- openBinaryFile f WriteMode
                         hPutStr h s
                         hClose h

