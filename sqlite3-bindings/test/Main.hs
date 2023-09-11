module Main (main) where

import Control.Exception (SomeException, bracket, catch, mask, throwIO, uninterruptibleMask_)
import Control.Monad (when)
import Data.Array (Array)
import Data.Array qualified as Array
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign (FunPtr, Ptr)
import Foreign.C (CInt (..), CString)
import Sqlite3.Bindings
import System.IO.Temp qualified as Temporary
import Test.Tasty
import Test.Tasty.HUnit

-- TODO:
--
-- sqlite3_auto_extension,
-- sqlite3_backup_finish,
-- sqlite3_backup_init,
-- sqlite3_backup_pagecount,
-- sqlite3_backup_remaining,
-- sqlite3_backup_step,
-- sqlite3_bind_blob,
-- sqlite3_bind_double,
-- sqlite3_bind_int,
-- sqlite3_bind_int64,
-- sqlite3_bind_null,
-- sqlite3_bind_parameter_count,
-- sqlite3_bind_parameter_index,
-- sqlite3_bind_parameter_name,
-- sqlite3_bind_pointer,
-- sqlite3_bind_text,
-- sqlite3_bind_value,
-- sqlite3_bind_zeroblob,
-- sqlite3_blob_bytes,
-- sqlite3_blob_close,
-- sqlite3_blob_open,
-- sqlite3_blob_read,
-- sqlite3_blob_reopen,
-- sqlite3_blob_write,
-- sqlite3_busy_handler,
-- sqlite3_busy_timeout,
-- sqlite3_cancel_auto_extension,
-- sqlite3_changes,
-- sqlite3_changes64,
-- sqlite3_clear_bindings,
-- sqlite3_close,
-- sqlite3_close_v2,
-- sqlite3_collation_needed,
-- sqlite3_column_blob,
-- sqlite3_column_bytes,
-- sqlite3_column_count,
-- sqlite3_column_database_name,
-- sqlite3_column_decltype,
-- sqlite3_column_double,
-- sqlite3_column_int,
-- sqlite3_column_int64,
-- sqlite3_column_name,
-- sqlite3_column_origin_name,
-- sqlite3_column_table_name,
-- sqlite3_column_text,
-- sqlite3_column_type,
-- sqlite3_column_value,
-- sqlite3_commit_hook,
-- sqlite3_compileoption_get,
-- sqlite3_compileoption_used,
-- sqlite3_complete,
-- -- sqlite3_config_getpcache2,
-- sqlite3_config_heap,
-- sqlite3_config_log,
-- sqlite3_config_lookaside,
-- -- sqlite3_config_memdb_maxsize,
-- sqlite3_config_memstatus,
-- sqlite3_config_multithread,
-- -- sqlite3_config_nmap_size,
-- sqlite3_config_pagecache,
-- -- sqlite3_config_pcache2,
-- -- sqlite3_config_pcache_hdrsz,
-- -- sqlite3_config_pmasz,
-- -- sqlite3_config_serialized,
-- sqlite3_config_singlethread,
-- -- sqlite3_config_small_malloc,
-- -- sqlite3_config_sorterref_size,
-- -- sqlite3_config_sqllog,
-- -- sqlite3_config_stmtjrnl_spill,
-- -- sqlite3_config_uri,
-- -- sqlite3_config_win32_heapsize,
-- sqlite3_context_db_handle,
-- sqlite3_create_collation,
-- sqlite3_create_filename,
-- sqlite3_create_function,
-- sqlite3_create_function_v2,
-- sqlite3_create_module,
-- sqlite3_create_module_v2,
-- sqlite3_create_window_function,
-- sqlite3_data_count,
-- sqlite3_database_file_object,
-- sqlite3_db_cacheflush,
-- sqlite3_db_config__1,
-- sqlite3_db_config__2,
-- sqlite3_db_config__3,
-- sqlite3_db_filename,
-- sqlite3_db_handle,
-- sqlite3_db_mutex,
-- sqlite3_db_name,
-- sqlite3_db_readonly,
-- sqlite3_db_release_memory,
-- sqlite3_db_status,
-- sqlite3_declare_vtab,
-- sqlite3_deserialize,
-- sqlite3_drop_modules,
-- sqlite3_errcode,
-- sqlite3_errmsg,
-- sqlite3_error_offset,
-- sqlite3_errstr,
-- sqlite3_exec,
-- sqlite3_expanded_sql,
-- sqlite3_extended_errcode,
-- sqlite3_extended_result_codes,
-- sqlite3_file_control,
-- sqlite3_filename_database,
-- sqlite3_filename_journal,
-- sqlite3_filename_wal,
-- sqlite3_finalize,
-- sqlite3_free,
-- sqlite3_free_filename,
-- sqlite3_get_autocommit,
-- sqlite3_get_auxdata,
-- sqlite3_hard_heap_limit64,
-- sqlite3_initialize,
-- sqlite3_interrupt,
-- sqlite3_keyword_check,
-- sqlite3_keyword_count,
-- sqlite3_keyword_name,
-- sqlite3_last_insert_rowid,
-- sqlite3_libversion,
-- sqlite3_libversion_number,
-- sqlite3_limit,
-- sqlite3_load_extension,
-- sqlite3_log,
-- sqlite3_malloc,
-- sqlite3_malloc64,
-- sqlite3_memory_highwater,
-- sqlite3_memory_used,
-- sqlite3_msize,
-- -- sqlite3_mutex_held,
-- -- sqlite3_mutex_notheld,
-- sqlite3_next_stmt,
-- sqlite3_normalized_sql,
-- sqlite3_open,
-- sqlite3_open_v2,
-- sqlite3_overload_function,
-- sqlite3_prepare_v2,
-- sqlite3_prepare_v3,
-- sqlite3_preupdate_blobwrite,
-- sqlite3_preupdate_count,
-- sqlite3_preupdate_depth,
-- -- sqlite3_preupdate_hook,
-- -- sqlite3_preupdate_new,
-- -- sqlite3_preupdate_old,
-- sqlite3_progress_handler,
-- sqlite3_randomness,
-- sqlite3_realloc,
-- sqlite3_realloc64,
-- sqlite3_release_memory,
-- sqlite3_reset,
-- sqlite3_reset_auto_extension,
-- sqlite3_result_blob,
-- sqlite3_result_blob64,
-- sqlite3_result_double,
-- sqlite3_result_error,
-- sqlite3_result_error_code,
-- sqlite3_result_error_nomem,
-- sqlite3_result_error_toobig,
-- sqlite3_result_int,
-- sqlite3_result_int64,
-- sqlite3_result_null,
-- sqlite3_result_pointer,
-- sqlite3_result_subtype,
-- sqlite3_result_text,
-- sqlite3_result_text64,
-- sqlite3_result_value,
-- sqlite3_result_zeroblob,
-- sqlite3_result_zeroblob64,
-- sqlite3_rollback_hook,
-- sqlite3_serialize,
-- sqlite3_set_authorizer,
-- sqlite3_set_auxdata,
-- sqlite3_set_last_insert_rowid,
-- sqlite3_shutdown,
-- sqlite3_sleep,
-- sqlite3_snapshot_cmp,
-- sqlite3_snapshot_free,
-- sqlite3_snapshot_get,
-- sqlite3_snapshot_open,
-- sqlite3_snapshot_recover,
-- sqlite3_soft_heap_limit64,
-- sqlite3_sourceid,
-- sqlite3_sql,
-- sqlite3_status,
-- sqlite3_status64,
-- sqlite3_step,
-- sqlite3_stmt_busy,
-- sqlite3_stmt_isexplain,
-- sqlite3_stmt_readonly,
-- -- sqlite3_stmt_scanstatus,
-- -- sqlite3_stmt_scanstatus_reset,
-- sqlite3_stmt_status,
-- sqlite3_strglob,
-- sqlite3_stricmp,
-- sqlite3_strlike,
-- sqlite3_strnicmp,
-- sqlite3_system_errno,
-- sqlite3_table_column_metadata,
-- sqlite3_threadsafe,
-- sqlite3_total_changes,
-- sqlite3_total_changes64,
-- sqlite3_trace_v2,
-- sqlite3_txn_state,
-- sqlite3_unlock_notify,
-- sqlite3_update_hook,
-- sqlite3_uri_boolean,
-- sqlite3_uri_int64,
-- sqlite3_uri_key,
-- sqlite3_uri_parameter,
-- sqlite3_user_data,
-- sqlite3_value_blob,
-- sqlite3_value_bytes,
-- sqlite3_value_double,
-- sqlite3_value_dup,
-- sqlite3_value_free,
-- sqlite3_value_frombind,
-- sqlite3_value_int,
-- sqlite3_value_int64,
-- sqlite3_value_nochange,
-- sqlite3_value_numeric_type,
-- sqlite3_value_pointer,
-- sqlite3_value_subtype,
-- sqlite3_value_text,
-- sqlite3_value_type,
-- sqlite3_version,
-- sqlite3_vfs_find,
-- sqlite3_vfs_register,
-- sqlite3_vfs_unregister,
-- sqlite3_vtab_collation,
-- sqlite3_vtab_config__1,
-- sqlite3_vtab_config__2,
-- sqlite3_vtab_distinct,
-- sqlite3_vtab_in,
-- sqlite3_vtab_in_first,
-- sqlite3_vtab_in_next,
-- sqlite3_vtab_nochange,
-- sqlite3_vtab_on_conflict,
-- sqlite3_vtab_rhs_value,
-- sqlite3_wal_autocheckpoint,
-- sqlite3_wal_checkpoint,
-- sqlite3_wal_checkpoint_v2,
-- sqlite3_wal_hook,

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
        testCase "compileoption_used" test_compileoption_used,
        testCase "complete" test_complete,
        testCase "create_collation" test_create_collation,
        testCase "last_insert_rowid" test_last_insert_rowid,
        testCase "libversion / libversion_number" test_libversion,
        testCase "open / close" test_open,
        testCase "rollback_hook" test_rollback_hook
      ]

-- sqlite3_auto_extension
test_autovacuum_pages :: IO ()
test_autovacuum_pages = do
  withConnection ":memory:" \conn -> do
    countRef <- newIORef (0 :: Int)
    -- Cause an auto-vacuum to trigger after every commit.
    exec conn "pragma auto_vacuum = full" >>= check
    exec conn "create table foo(bar)" >>= check
    -- Register a callback that bumps `countRef` every time auto-vacuum occurs, and commit a transaction.
    sqlite3_autovacuum_pages conn (Just \_ _ n _ -> modifyIORef' countRef (+ 1) $> n) & assertOk
    exec conn "insert into foo values (1)" >>= check
    -- Clear the callback and commit another transaction.
    sqlite3_autovacuum_pages conn Nothing & assertOk
    exec conn "insert into foo values (1)" >>= check
    -- Assert that our callback was called once.
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

test_compileoption_used :: IO ()
test_compileoption_used = do
  assertEqual "" False (sqlite3_compileoption_used "foobar")

test_complete :: IO ()
test_complete = do
  assertEqual "" False (sqlite3_complete "select")
  assertEqual "" True (sqlite3_complete "select 1;")

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

test_libversion :: IO ()
test_libversion = do
  assertEqual "" "3.39.2" sqlite3_libversion
  assertEqual "" 3039002 sqlite3_libversion_number

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

assertOk :: IO CInt -> IO ()
assertOk action = do
  code <- action
  when (code /= _SQLITE_OK) do
    assertFailure (Text.unpack (sqlite3_errstr code <> " (" <> Text.pack (show code) <> ")"))

inspect :: CInt -> a -> Either Text a
inspect code result =
  if code == _SQLITE_OK
    then Right result
    else Left (sqlite3_errstr code <> " (" <> Text.pack (show code) <> ")")
