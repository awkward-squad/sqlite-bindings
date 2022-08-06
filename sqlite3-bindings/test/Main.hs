module Main where

import Control.Exception (SomeException, bracket, catch, mask, throwIO, uninterruptibleMask_)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Sqlite3.Bindings
import qualified System.IO.Temp as Temporary
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  withSqliteLibrary do
    (defaultMain . testGroup "tests")
      [ testCase "autovacuum_pages" test_autovacuum_pages,
        testCase "backup_*" test_backup,
        testCase "bind_*" test_bind,
        testCase "blob_*" test_blob,
        testCase "busy_handler" test_busy_handler,
        testCase "busy_timeout" test_busy_timeout,
        testCase "changes / changes64 / total_changes / total_changes64" test_changes,
        testCase "clear_bindings" test_clear_bindings,
        testCase "collation_needed" test_collation_needed,
        testCase "column_*" test_column,
        testCase "column_database_name / column_origin_name / column_name / column_table_name" test_column_name,
        testCase "column_decltype" test_column_decltype,
        testCase "column_type" test_column_type,
        testCase "commit_hook" test_commit_hook,
        testCase "compileoption_get" test_compileoption_get,
        testCase "create_collation" test_create_collation,
        testCase "last_insert_rowid" test_last_insert_rowid,
        testCase "open / close" test_open,
        testCase "rollback_hook" test_rollback_hook
      ]

test_autovacuum_pages :: IO ()
test_autovacuum_pages = do
  withConnection ":memory:" \conn -> do
    countRef <- newIORef (0 :: Int)
    exec conn "pragma auto_vacuum = full" >>= check
    exec conn "create table foo(bar)" >>= check
    autovacuum_pages conn (Just \_ _ n _ -> modifyIORef' countRef (+ 1) $> n) >>= check
    exec conn "insert into foo values (1)" >>= check
    autovacuum_pages conn Nothing >>= check
    exec conn "insert into foo values (1)" >>= check
    count <- readIORef countRef
    assertEqual "" 1 count

test_backup :: IO ()
test_backup = do
  withConnection ":memory:" \conn1 ->
    withConnection ":memory:" \conn2 ->
      withBackup (conn1, "main") (conn2, "main") \backup -> do
        sqlite3_backup_pagecount backup >>= assertEqual "" 0
        sqlite3_backup_remaining backup >>= assertEqual "" 0

  withConnection ":memory:" \conn1 ->
    withConnection ":memory:" \conn2 -> do
      exec conn1 "create table foo (bar)" >>= check
      exec conn1 "insert into foo values (1)" >>= check
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

-- TODO sqlite3_bind_value
test_bind :: IO ()
test_bind = do
  withConnection ":memory:" \conn -> do
    withStatement conn "select ?" \(statement, _) -> do
      sqlite3_bind_parameter_count statement >>= assertEqual "" 1

      bind_blob statement 1 ByteString.empty >>= check
      bind_blob statement 1 (ByteString.pack [0]) >>= check
      bind_double statement 1 0 >>= check
      bind_int statement 1 0 >>= check
      bind_int64 statement 1 0 >>= check
      bind_null statement 1 >>= check
      bind_pointer statement 1 () "foo" >>= check
      bind_text statement 1 "" >>= check
      bind_text statement 1 "foo" >>= check
      bind_zeroblob statement 1 0 >>= check
      bind_zeroblob statement 1 1 >>= check

      bind_int statement 0 0 >>= assertEqual "" (Left "column index out of range (25)")
      bind_int statement 2 0 >>= assertEqual "" (Left "column index out of range (25)")

    withStatement conn "select ?, :foo, @bar, $baz" \(statement, _) -> do
      sqlite3_bind_parameter_count statement >>= assertEqual "" 4
      sqlite3_bind_parameter_index statement ":foo" >>= assertEqual "" (Just 2)
      sqlite3_bind_parameter_index statement "@bar" >>= assertEqual "" (Just 3)
      sqlite3_bind_parameter_index statement "$baz" >>= assertEqual "" (Just 4)
      sqlite3_bind_parameter_name statement 0 >>= assertEqual "" Nothing
      sqlite3_bind_parameter_name statement 1 >>= assertEqual "" Nothing
      sqlite3_bind_parameter_name statement 2 >>= assertEqual "" (Just ":foo")
      sqlite3_bind_parameter_name statement 3 >>= assertEqual "" (Just "@bar")
      sqlite3_bind_parameter_name statement 4 >>= assertEqual "" (Just "$baz")

test_blob :: IO ()
test_blob = do
  withConnection ":memory:" \conn -> do
    exec conn "create table foo (bar)" >>= check
    exec conn "insert into foo values (x'01020304')" >>= check
    rowid1 <- sqlite3_last_insert_rowid conn
    exec conn "insert into foo values (x'05060708')" >>= check
    rowid2 <- sqlite3_last_insert_rowid conn
    withBlob conn "main" "foo" "bar" rowid1 True \blob -> do
      blob_read blob 0 0 >>= assertEqual "" (Right ByteString.empty)
      blob_read blob 2 0 >>= assertEqual "" (Right (ByteString.pack [1, 2]))
      blob_read blob 2 2 >>= assertEqual "" (Right (ByteString.pack [3, 4]))
      blob_read blob 5 0 >>= assertEqual "" (Left "SQL logic error (1)")
      blob_read blob 0 5 >>= assertEqual "" (Left "SQL logic error (1)")

      blob_write blob (ByteString.pack [5, 6]) 0 >>= check
      blob_read blob 4 0 >>= assertEqual "" (Right (ByteString.pack [5, 6, 3, 4]))
      blob_write blob (ByteString.pack [1, 2, 3, 4, 5]) 0 >>= assertEqual "" (Left "SQL logic error (1)")

      blob_reopen blob rowid2 >>= check
      blob_read blob 4 0 >>= assertEqual "" (Right (ByteString.pack [5, 6, 7, 8]))

test_busy_handler :: IO ()
test_busy_handler = do
  Temporary.withSystemTempDirectory "sqlite3-bindings" \dir -> do
    let name = Text.pack dir <> "/database.sqlite3"
    withConnection name \conn1 -> do
      withConnection name \conn2 -> do
        exec conn1 "begin immediate transaction" >>= check
        invoked <- newIORef False
        withBusyHandler conn2 (\_ -> writeIORef invoked True >> pure False) do
          exec conn2 "begin immediate transaction"
            >>= assertEqual "" (Left "database is locked (5); database is locked")
          readIORef invoked >>= \case
            False -> assertFailure "didn't invoke busy handler"
            True -> pure ()

test_busy_timeout :: IO ()
test_busy_timeout = do
  withConnection ":memory:" \conn -> do
    busy_timeout conn 0 >>= check
    busy_timeout conn 1 >>= check
    busy_timeout conn (-1) >>= check

test_changes :: IO ()
test_changes = do
  withConnection ":memory:" \conn -> do
    sqlite3_changes conn >>= assertEqual "" 0
    sqlite3_changes64 conn >>= assertEqual "" 0
    sqlite3_total_changes conn >>= assertEqual "" 0
    sqlite3_total_changes64 conn >>= assertEqual "" 0
    exec conn "create table foo (bar)" >>= check
    exec conn "insert into foo values (1), (2)" >>= check
    sqlite3_changes conn >>= assertEqual "" 2
    sqlite3_changes64 conn >>= assertEqual "" 2
    sqlite3_total_changes conn >>= assertEqual "" 2
    sqlite3_total_changes64 conn >>= assertEqual "" 2
    exec conn "update foo set bar = 3 where bar = 1" >>= check
    sqlite3_changes conn >>= assertEqual "" 1
    sqlite3_changes64 conn >>= assertEqual "" 1
    sqlite3_total_changes conn >>= assertEqual "" 3
    sqlite3_total_changes64 conn >>= assertEqual "" 3
    exec conn "delete from foo" >>= check
    sqlite3_changes conn >>= assertEqual "" 2
    sqlite3_changes64 conn >>= assertEqual "" 2
    sqlite3_total_changes conn >>= assertEqual "" 5
    sqlite3_total_changes64 conn >>= assertEqual "" 5
    exec conn "delete from foo" >>= check
    sqlite3_changes conn >>= assertEqual "" 0
    sqlite3_changes64 conn >>= assertEqual "" 0
    sqlite3_total_changes conn >>= assertEqual "" 5
    sqlite3_total_changes64 conn >>= assertEqual "" 5

test_clear_bindings :: IO ()
test_clear_bindings = do
  withConnection ":memory:" \conn -> do
    withStatement conn "select 1" \(statement, _) -> do
      clear_bindings statement >>= check
    withStatement conn "select ?" \(statement, _) -> do
      clear_bindings statement >>= check
      bind_int statement 1 0 >>= check
      clear_bindings statement >>= check

test_collation_needed :: IO ()
test_collation_needed = do
  withConnection ":memory:" \conn -> do
    let collationNeeded :: Text -> IO ()
        collationNeeded = \case
          "foo" -> do
            _ <- create_collation conn "foo" (Just compare)
            pure ()
          _ -> pure ()
    withCollationNeeded conn collationNeeded do
      exec conn "create table foo (bar collate foo)" >>= check

test_column :: IO ()
test_column = do
  withConnection ":memory:" \conn ->
    withStatement conn "select 1, 2.0, 'foo', x'0102', null" \(statement, _) -> do
      sqlite3_step statement >>= assertEqual "" _SQLITE_ROW
      sqlite3_column_count statement >>= assertEqual "" 5

      sqlite3_column_int statement 0 >>= assertEqual "" 1
      sqlite3_column_int statement 4 >>= assertEqual "" 0
      sqlite3_column_int64 statement 0 >>= assertEqual "" 1
      sqlite3_column_int64 statement 4 >>= assertEqual "" 0
      sqlite3_column_double statement 1 >>= assertEqual "" 2.0
      sqlite3_column_double statement 4 >>= assertEqual "" 0.0
      sqlite3_column_text statement 2 >>= assertEqual "" "foo"
      sqlite3_column_text statement 4 >>= assertEqual "" Text.empty
      sqlite3_column_blob statement 3 >>= assertEqual "" (ByteString.pack [1, 2])
      sqlite3_column_blob statement 4 >>= assertEqual "" ByteString.empty
      _ <- sqlite3_column_value statement 0
      pure ()

test_column_decltype :: IO ()
test_column_decltype = do
  withConnection ":memory:" \conn -> do
    withStatement conn "select 1" \(statement, _) -> do
      sqlite3_column_decltype statement 0 >>= assertEqual "" Nothing

    exec conn "create table foo (bar)" >>= check
    withStatement conn "select bar from foo" \(statement, _) -> do
      sqlite3_column_decltype statement 0 >>= assertEqual "" Nothing

    exec conn "create table foo2 (bar2 oink)" >>= check
    withStatement conn "select bar2 from foo2" \(statement, _) -> do
      sqlite3_column_decltype statement 0 >>= assertEqual "" (Just "oink")

test_column_name :: IO ()
test_column_name = do
  withConnection ":memory:" \conn -> do
    withStatement conn "select 1" \(statement, _) -> do
      sqlite3_column_database_name statement 0 >>= assertEqual "" Nothing
      sqlite3_column_name statement 0 >>= assertEqual "" (Just "1")
      sqlite3_column_origin_name statement 0 >>= assertEqual "" Nothing
      sqlite3_column_table_name statement 0 >>= assertEqual "" Nothing

    exec conn "create table foo (bar)" >>= check
    withStatement conn "select bar as baz from foo" \(statement, _) -> do
      sqlite3_column_database_name statement 0 >>= assertEqual "" (Just "main")
      sqlite3_column_name statement 0 >>= assertEqual "" (Just "baz")
      sqlite3_column_origin_name statement 0 >>= assertEqual "" (Just "bar")
      sqlite3_column_table_name statement 0 >>= assertEqual "" (Just "foo")

test_column_type :: IO ()
test_column_type = do
  withConnection ":memory:" \conn -> do
    exec conn "create table foo (a, b, c, d, e)" >>= check
    exec conn "insert into foo (a, b, c, d, e) values (1, 2.0, 'foo', x'0102', null)" >>= check
    withStatement conn "select a, b, c, d, e from foo" \(statement, _) -> do
      sqlite3_step statement >>= assertEqual "" _SQLITE_ROW
      sqlite3_column_type statement 0 >>= assertEqual "" SQLITE_INTEGER
      sqlite3_column_type statement 1 >>= assertEqual "" SQLITE_FLOAT
      sqlite3_column_type statement 2 >>= assertEqual "" SQLITE_TEXT
      sqlite3_column_type statement 3 >>= assertEqual "" SQLITE_BLOB
      sqlite3_column_type statement 4 >>= assertEqual "" SQLITE_NULL

test_commit_hook :: IO ()
test_commit_hook = do
  ref <- newIORef (0 :: Int)
  withConnection ":memory:" \conn -> do
    sqlite3_commit_hook conn (writeIORef ref 1 >> pure 0)
    exec conn "create table foo (bar)" >>= check
    exec conn "begin" >>= check
    exec conn "insert into foo (bar) values (1)" >>= check
    exec conn "commit" >>= check
    readIORef ref >>= assertEqual "" 1
    sqlite3_commit_hook conn (writeIORef ref 2 >> pure 0)
    exec conn "begin" >>= check
    exec conn "insert into foo (bar) values (1)" >>= check
    exec conn "commit" >>= check
    readIORef ref >>= assertEqual "" 2

test_compileoption_get :: IO ()
test_compileoption_get = do
  case sqlite3_compileoption_get 0 of
    Nothing -> pure ()
    Just !_ -> pure ()
  assertEqual "" Nothing (sqlite3_compileoption_get (-1))

test_create_collation :: IO ()
test_create_collation = do
  withConnection ":memory:" \conn -> do
    create_collation conn "dayofweek" (Just compareDayOfWeek) >>= check
    exec conn "create table foo (bar collate dayofweek)" >>= check
    exec conn "insert into foo values ('wednesday'), ('sunday'), ('oink'), ('monday')" >>= check
    rows <- execReturn conn "select bar from foo order by bar collate dayofweek" >>= check
    assertEqual "" [["monday"], ["wednesday"], ["sunday"], ["oink"]] rows
  where
    compareDayOfWeek :: Text -> Text -> Ordering
    compareDayOfWeek s1 s2 =
      case (dayOfWeek s1, dayOfWeek s2) of
        (Just n1, Just n2) -> compare n1 n2
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> compare s1 s2
      where
        dayOfWeek :: Text -> Maybe Int
        dayOfWeek = \case
          "monday" -> Just 0
          "tuesday" -> Just 1
          "wednesday" -> Just 2
          "thursday" -> Just 3
          "friday" -> Just 4
          "saturday" -> Just 5
          "sunday" -> Just 6
          _ -> Nothing

test_last_insert_rowid :: IO ()
test_last_insert_rowid = do
  withConnection ":memory:" \conn -> do
    sqlite3_last_insert_rowid conn >>= assertEqual "" 0
    exec conn "create table foo (bar)" >>= check
    exec conn "insert into foo values (1)" >>= check
    sqlite3_last_insert_rowid conn >>= assertEqual "" 1
    exec conn "create table bar (baz primary key) without rowid" >>= check
    exec conn "insert into bar values (1)" >>= check
    exec conn "insert into bar values (2)" >>= check
    sqlite3_last_insert_rowid conn >>= assertEqual "" 1

test_open :: IO ()
test_open = do
  withConnection ":memory:" \_ -> pure ()
  withConnection "" \_ -> pure ()

test_rollback_hook :: IO ()
test_rollback_hook = do
  ref <- newIORef (0 :: Int)
  withConnection ":memory:" \conn -> do
    sqlite3_rollback_hook conn (writeIORef ref 1 >> pure 0)
    exec conn "create table foo (bar)" >>= check
    exec conn "begin" >>= check
    exec conn "insert into foo (bar) values (1)" >>= check
    exec conn "rollback" >>= check
    readIORef ref >>= assertEqual "" 1
    sqlite3_rollback_hook conn (writeIORef ref 2 >> pure 0)
    exec conn "begin" >>= check
    exec conn "insert into foo (bar) values (1)" >>= check
    exec conn "rollback" >>= check
    readIORef ref >>= assertEqual "" 2

------------------------------------------------------------------------------------------------------------------------
-- Exception-safe acquire/release actions

withConnection :: Text -> (Sqlite3 -> IO a) -> IO a
withConnection name =
  brackety (open name) close

withBackup :: (Sqlite3, Text) -> (Sqlite3, Text) -> (Sqlite3_backup -> IO a) -> IO a
withBackup (conn1, name1) (conn2, name2) =
  brackety (backup_init conn2 name2 conn1 name1) backup_finish

withBlob :: Sqlite3 -> Text -> Text -> Text -> Int64 -> Bool -> (Sqlite3_blob -> IO a) -> IO a
withBlob conn database table column rowid mode =
  brackety (blob_open conn database table column rowid mode) blob_close

withBusyHandler :: Sqlite3 -> (Int -> IO Bool) -> IO a -> IO a
withBusyHandler conn callback =
  brackety2 (busy_handler conn callback)

withCollationNeeded :: Sqlite3 -> (Text -> IO ()) -> IO a -> IO a
withCollationNeeded conn callback =
  brackety2 (collation_needed conn callback)

withSqliteLibrary :: IO a -> IO a
withSqliteLibrary action =
  brackety initialize (\() -> shutdown) \() -> action

withStatement :: Sqlite3 -> Text -> ((Sqlite3_stmt, Text) -> IO a) -> IO a
withStatement conn sql =
  brackety (prepare_v2 conn sql) (\(statement, _) -> finalize statement)

brackety :: IO (Either Text a) -> (a -> IO (Either Text ())) -> (a -> IO b) -> IO b
brackety acquire release action =
  mask \restore -> do
    value <- restore acquire >>= check
    let cleanup = uninterruptibleMask_ (release value)
    result <-
      restore (action value) `catch` \(exception :: SomeException) -> do
        void cleanup
        throwIO exception
    cleanup >>= check
    pure result

brackety2 :: IO (Either Text (), IO ()) -> IO a -> IO a
brackety2 acquire action =
  bracket acquire (\(_, release) -> release) \(value, _) -> do
    check value
    action

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

bind_double :: Sqlite3_stmt -> Int -> Double -> IO (Either Text ())
bind_double statement index n = do
  code <- sqlite3_bind_double statement index n
  pure (inspect code ())

bind_int :: Sqlite3_stmt -> Int -> Int -> IO (Either Text ())
bind_int statement index n = do
  code <- sqlite3_bind_int statement index n
  pure (inspect code ())

bind_int64 :: Sqlite3_stmt -> Int -> Int64 -> IO (Either Text ())
bind_int64 statement index n = do
  code <- sqlite3_bind_int64 statement index n
  pure (inspect code ())

bind_null :: Sqlite3_stmt -> Int -> IO (Either Text ())
bind_null statement index = do
  code <- sqlite3_bind_null statement index
  pure (inspect code ())

bind_pointer :: Sqlite3_stmt -> Int -> a -> Text -> IO (Either Text ())
bind_pointer statement index pointer typ = do
  code <- sqlite3_bind_pointer statement index pointer typ
  pure (inspect code ())

bind_text :: Sqlite3_stmt -> Int -> Text -> IO (Either Text ())
bind_text statement index string = do
  code <- sqlite3_bind_text statement index string
  pure (inspect code ())

bind_zeroblob :: Sqlite3_stmt -> Int -> Int -> IO (Either Text ())
bind_zeroblob statement index n = do
  code <- sqlite3_bind_zeroblob statement index n
  pure (inspect code ())

blob_close :: Sqlite3_blob -> IO (Either Text ())
blob_close blob = do
  code <- sqlite3_blob_close blob
  pure (inspect code ())

blob_open :: Sqlite3 -> Text -> Text -> Text -> Int64 -> Bool -> IO (Either Text Sqlite3_blob)
blob_open conn database table column rowid mode = do
  (maybeBlob, code) <- sqlite3_blob_open conn database table column rowid mode
  case maybeBlob of
    Nothing -> pure (inspect code undefined)
    Just blob -> do
      let result = inspect code blob
      case result of
        Left _ -> void (blob_close blob)
        Right _ -> pure ()
      pure result

blob_read :: Sqlite3_blob -> Int -> Int -> IO (Either Text ByteString)
blob_read blob len offset =
  sqlite3_blob_read blob len offset <&> \case
    Left code -> inspect code undefined
    Right bytes -> Right bytes

blob_reopen :: Sqlite3_blob -> Int64 -> IO (Either Text ())
blob_reopen blob rowid = do
  code <- sqlite3_blob_reopen blob rowid
  pure (inspect code ())

blob_write :: Sqlite3_blob -> ByteString -> Int -> IO (Either Text ())
blob_write blob bytes offset = do
  code <- sqlite3_blob_write blob bytes offset
  pure (inspect code ())

busy_handler :: Sqlite3 -> (Int -> IO Bool) -> IO (Either Text (), IO ())
busy_handler conn callback = do
  (code, destructor) <- sqlite3_busy_handler conn callback
  pure (inspect code (), destructor)

busy_timeout :: Sqlite3 -> Int -> IO (Either Text ())
busy_timeout conn milliseconds = do
  code <- sqlite3_busy_timeout conn milliseconds
  pure (inspect code ())

clear_bindings :: Sqlite3_stmt -> IO (Either Text ())
clear_bindings statement = do
  code <- sqlite3_clear_bindings statement
  pure (inspect code ())

close :: Sqlite3 -> IO (Either Text ())
close conn = do
  code <- sqlite3_close conn
  pure (inspect code ())

collation_needed :: Sqlite3 -> (Text -> IO ()) -> IO (Either Text (), IO ())
collation_needed conn callback = do
  (code, destructor) <- sqlite3_collation_needed conn callback
  pure (inspect code (), destructor)

create_collation :: Sqlite3 -> Text -> Maybe (Text -> Text -> Ordering) -> IO (Either Text ())
create_collation conn name collation = do
  code <- sqlite3_create_collation conn name collation
  pure (inspect code ())

exec :: Sqlite3 -> Text -> IO (Either Text ())
exec conn sql =
  exec_ conn sql Nothing

-- A convenient wrapper around 'exec' that accumulates rows in a list.
execReturn :: Sqlite3 -> Text -> IO (Either Text [[Text]])
execReturn conn sql = do
  rowsRef <- newIORef []
  exec_ conn sql (Just \cols _ -> modifyIORef' rowsRef (cols :) >> pure 0) >>= \case
    Left err -> pure (Left err)
    Right () -> do
      rows <- readIORef rowsRef
      pure (Right (reverse (map Array.elems rows)))

exec_ :: Sqlite3 -> Text -> Maybe (Array Int Text -> Array Int Text -> IO Int) -> IO (Either Text ())
exec_ conn sql callback = do
  (maybeErrorMsg, code) <- sqlite3_exec conn sql callback
  pure case inspect code () of
    Left errorMsg -> Left (errorMsg <> maybe Text.empty ("; " <>) maybeErrorMsg)
    Right () -> Right ()

finalize :: Sqlite3_stmt -> IO (Either Text ())
finalize statement = do
  code <- sqlite3_finalize statement
  pure (inspect code ())

initialize :: IO (Either Text ())
initialize = do
  code <- sqlite3_initialize
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

shutdown :: IO (Either Text ())
shutdown = do
  code <- sqlite3_shutdown
  pure (inspect code ())

------------------------------------------------------------------------------------------------------------------------
-- Test helpers

check :: Either Text a -> IO a
check = \case
  Left msg -> assertFailure (Text.unpack msg)
  Right result -> pure result

inspect :: CInt -> a -> Either Text a
inspect code result =
  if code == _SQLITE_OK
    then Right result
    else Left (sqlite3_errstr code <> " (" <> Text.pack (show code) <> ")")
