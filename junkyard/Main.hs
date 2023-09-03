module Main where

import Foreign
import Foreign.C
import Sqlite3.Bindings
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Initializing... "
  0 <- sqlite3_initialize
  putStrLn "done."

  putStr "Setting my_extension to auto-load... "
  0 <- sqlite3_auto_extension my_extension
  putStrLn "done."

  putStr "Opening connection... "
  (Just _conn, 0) <- sqlite3_open ":memory:"
  putStrLn "done."

  putStr "Shutting down... "
  0 <- sqlite3_shutdown
  putStrLn "done."

foreign import capi "junkyard.h &my_extension"
  my_extension :: FunPtr (Sqlite3 -> Ptr CString -> Sqlite3_api_routines -> IO CInt)
