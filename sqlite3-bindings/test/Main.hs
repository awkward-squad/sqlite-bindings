module Main where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Sqlite3.Bindings
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  (defaultMain . testGroup "tests")
    [ testCase "sqlite3_autovacuum_pages" test_sqlite3_autovacuum_pages,
      testCase "sqlite3_backup_*" test_sqlite3_backup,
      testCase "sqlite3_bind_blob" test_sqlite3_bind_blob,
      testCase "sqlite3_open / sqlite3_close" test_sqlite3_open
    ]

test_sqlite3_autovacuum_pages :: IO ()
test_sqlite3_autovacuum_pages = do
  withConnection ":memory:" \conn -> do
    countRef <- newIORef (0 :: Int)
    check (exec conn "pragma auto_vacuum = full")
    check (exec conn "create table foo(bar)")
    check (autovacuum_pages conn (Just \_ _ n _ -> modifyIORef' countRef (+ 1) $> n))
    check (exec conn "insert into foo values (1)")
    check (autovacuum_pages conn Nothing)
    check (exec conn "insert into foo values (1)")
    count <- readIORef countRef
    assertEqual "" 1 count

test_sqlite3_backup :: IO ()
test_sqlite3_backup = do
  withConnection ":memory:" \conn1 ->
    withConnection ":memory:" \conn2 ->
      withBackup (conn1, "main") (conn2, "main") \backup -> do
        sqlite3_backup_pagecount backup >>= assertEqual "" 0
        sqlite3_backup_remaining backup >>= assertEqual "" 0
  withConnection ":memory:" \conn1 ->
    withConnection ":memory:" \conn2 -> do
      check (exec conn1 "create table foo (bar)")
      check (exec conn1 "insert into foo values (1)")
      withBackup (conn1, "main") (conn2, "main") \backup -> do
        sqlite3_backup_pagecount backup >>= assertEqual "" 0
        sqlite3_backup_remaining backup >>= assertEqual "" 0
        backup_step backup 0 >>= assertEqual "" (Right ())
        sqlite3_backup_pagecount backup >>= assertEqual "" 2
        sqlite3_backup_remaining backup >>= assertEqual "" 2
        backup_step backup 1 >>= assertEqual "" (Right ())
        sqlite3_backup_remaining backup >>= assertEqual "" 1
        backup_step backup 1 >>= assertEqual "" (Left "no more rows available (101)")
        sqlite3_backup_remaining backup >>= assertEqual "" 0

test_sqlite3_bind_blob :: IO ()
test_sqlite3_bind_blob = do
  withConnection ":memory:" \conn -> do
    withStatement conn "select ?" \(statement, _) -> do
      check (bind_blob statement 1 ByteString.empty)
      check (bind_blob statement 1 (ByteString.pack [0]))

test_sqlite3_open :: IO ()
test_sqlite3_open = do
  withConnection ":memory:" \_ -> pure ()
  withConnection "" \_ -> pure ()

------------------------------------------------------------------------------------------------------------------------
-- Exception-safe acquire/release actions

withConnection :: Text -> (Sqlite3 -> IO a) -> IO a
withConnection name =
  bracket (check (open name)) (check . close)

withBackup :: (Sqlite3, Text) -> (Sqlite3, Text) -> (Sqlite3_backup -> IO a) -> IO a
withBackup (conn1, name1) (conn2, name2) action =
  bracket (check (backup_init conn2 name2 conn1 name1)) (check . backup_finish) action

withStatement :: Sqlite3 -> Text -> ((Sqlite3_stmt, Text) -> IO a) -> IO a
withStatement conn sql =
  bracket (check (prepare_v2 conn sql)) (\(statement, _) -> check (finalize statement))

------------------------------------------------------------------------------------------------------------------------
-- API wrappers that return Either Text

autovacuum_pages :: Sqlite3 -> Maybe (Text -> Word -> Word -> Word -> IO Word) -> IO (Either Text ())
autovacuum_pages conn callback = do
  code <- sqlite3_autovacuum_pages conn callback
  pure (inspect code ())

backup_finish :: Sqlite3_backup -> IO (Either Text ())
backup_finish backup = do
  code <- sqlite3_backup_finish backup
  pure (inspect code ())

backup_init :: Sqlite3 -> Text -> Sqlite3 -> Text -> IO (Either Text Sqlite3_backup)
backup_init dstConnection dstName srcConnection srcName =
  sqlite3_backup_init dstConnection dstName srcConnection srcName >>= \case
    Nothing -> Left <$> sqlite3_errmsg dstConnection
    Just backup -> pure (Right backup)

backup_step :: Sqlite3_backup -> Int -> IO (Either Text ())
backup_step backup n = do
  code <- sqlite3_backup_step backup n
  pure (inspect code ())

bind_blob :: Sqlite3_stmt -> Int -> ByteString -> IO (Either Text ())
bind_blob statement index blob = do
  code <- sqlite3_bind_blob statement index blob
  pure (inspect code ())

close :: Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  pure (inspect code ())

exec :: Sqlite3 -> Text -> IO (Either Text ())
exec conn sql = do
  (maybeErrorMsg, code) <- sqlite3_exec conn sql Nothing
  pure case inspect code () of
    Left errorMsg -> Left (errorMsg <> maybe Text.empty ("; " <>) maybeErrorMsg)
    Right () -> Right ()

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

------------------------------------------------------------------------------------------------------------------------
-- Test helpers

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
