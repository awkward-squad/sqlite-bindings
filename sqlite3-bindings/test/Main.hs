module Main where

import Control.Exception (bracket)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Sqlite3.Bindings
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  (defaultMain . testGroup "tests")
    [ testCase "sqlite3_backup_init / sqlite3_backup_finish" test_sqlite3_backup_init,
      testCase "sqlite3_open / sqlite3_close" test_sqlite3_open
    ]

test_sqlite3_backup_init :: IO ()
test_sqlite3_backup_init = do
  connect ":memory:" \c1 ->
    connect ":memory:" \c2 -> do
      backup <- check (backup_init c2 "main" c1 "main")
      check (backup_finish backup)

test_sqlite3_open :: IO ()
test_sqlite3_open = do
  connect ":memory:" \_ -> pure ()
  connect "" \_ -> pure ()

--

connect :: Text -> (Sqlite3 -> IO a) -> IO a
connect name action =
  bracket (check (open name)) (check . close) action

--

backup_finish :: Sqlite3_backup -> IO (Either Text ())
backup_finish backup = do
  code <- sqlite3_backup_finish backup
  pure (inspect code ())

backup_init :: Sqlite3 -> Text -> Sqlite3 -> Text -> IO (Either Text Sqlite3_backup)
backup_init dstConnection dstName srcConnection srcName =
  sqlite3_backup_init dstConnection dstName srcConnection srcName >>= \case
    Nothing -> Left <$> sqlite3_errmsg dstConnection
    Just backup -> pure (Right backup)

close :: Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  pure (inspect code ())

open :: Text -> IO (Either Text Sqlite3)
open name = do
  (maybeConn, code) <- sqlite3_open name
  case maybeConn of
    Nothing -> pure (inspect code undefined)
    Just conn -> do
      let result = inspect code conn
      case result of
        Left _ -> void (close conn)
        Right _ -> pure ()
      pure result

--

check :: IO (Either Text a) -> IO a
check action =
  action >>= \case
    Left msg -> assertFailure (Text.unpack msg)
    Right result -> pure result

inspect :: CInt -> a -> Either Text a
inspect code result =
  if code == _SQLITE_OK
    then Right result
    else Left (sqlite3_errstr code)
