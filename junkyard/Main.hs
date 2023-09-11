module Main where

import Control.Monad
import Data.Array qualified as Array
import Data.Text qualified as Text
import Foreign
import Foreign.C
import Sqlite3.Bindings
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Initializing... "
  _SQLITE_OK <- sqlite3_initialize
  putStrLn "done."

  putStr "Setting my_entrypoint to auto-load... "
  _SQLITE_OK <- sqlite3_auto_extension my_entrypoint
  putStrLn "done."

  putStr "Opening connection... "
  (Just conn, _SQLITE_OK) <- sqlite3_open ":memory:"
  putStrLn "done."

  putStr "Preparing statement... "
  Right statement <- sqlite3_prepare_v2 conn "SELECT toUpper('foo')"
  putStrLn "done."

  putStr "Stepping statement... "
  _SQLITE_ROW <- sqlite3_step statement
  putStrLn "done."

  putStr "Getting statement column 0... "
  text <- sqlite3_column_text statement 0
  putStrLn ("done: " ++ Text.unpack text)

  putStr "Stepping statement... "
  _SQLITE_DONE <- sqlite3_step statement
  putStrLn "done."

  putStr "Finalizing statement... "
  _SQLITE_OK <- sqlite3_finalize statement
  putStrLn "done."

  putStr "Shutting down... "
  _SQLITE_OK <- sqlite3_shutdown
  putStrLn "done."

myEntrypoint :: Sqlite3 -> Ptr CString -> IO CInt
myEntrypoint connection errMsg = do
  putStrLn "Hello from myEntrypoint!"

  sqlite3_create_function
    connection
    "toUpper"
    1
    (_SQLITE_UTF8 .|. _SQLITE_DETERMINISTIC)
    \context args -> do
      let value = args Array.! 0
      ty <- sqlite3_value_type value
      if ty == SQLITE_TEXT
        then do
          text <- sqlite3_value_text value
          sqlite3_result_text context (Text.toUpper text)
        else do
          sqlite3_result_text context "not text"

  pure _SQLITE_OK

foreign import capi "junkyard.h &my_entrypoint"
  my_entrypoint :: FunPtr (Sqlite3 -> Ptr CString -> Sqlite3_api_routines -> IO CInt)

foreign export capi
  myEntrypoint :: Sqlite3 -> Ptr CString -> IO CInt
