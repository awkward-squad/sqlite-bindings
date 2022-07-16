module Main where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
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
      testCase "sqlite3_bind_blob" test_sqlite3_bind_blob,
      testCase "sqlite3_open / sqlite3_close" test_sqlite3_open
    ]

test_sqlite3_backup_init :: IO ()
test_sqlite3_backup_init = do
  connect ":memory:" \c1 ->
    connect ":memory:" \c2 -> do
      backup <- check (backup_init c2 "main" c1 "main")
      check (backup_finish backup)

test_sqlite3_bind_blob :: IO ()
test_sqlite3_bind_blob = do
  connect ":memory:" \conn -> do
    (statement, _) <- check (prepare_v2 conn "select ?")
    check (bind_blob statement 1 ByteString.empty)
    check (bind_blob statement 1 (ByteString.pack [0]))
    check (finalize statement)

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

bind_blob :: Sqlite3_stmt -> Int -> ByteString -> IO (Either Text ())
bind_blob statement index blob = do
  code <- sqlite3_bind_blob statement index blob
  pure (inspect code ())

close :: Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  pure (inspect code ())

finalize :: Sqlite3_stmt -> IO (Either Text ())
finalize statement = do
  code <- sqlite3_finalize statement
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

prepare_v2 :: Sqlite3 -> Text -> IO (Either Text (Sqlite3_stmt, Text))
prepare_v2 conn sql = do
  (maybeStatement, unusedSql, code) <- sqlite3_prepare_v2 conn sql
  pure case maybeStatement of
    Nothing -> inspect code undefined
    Just statement -> Right (statement, unusedSql)

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
    else Left (sqlite3_errstr code <> " (" <> Text.pack (show code) <> ")")
