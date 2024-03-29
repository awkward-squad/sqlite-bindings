module Sqlite3.Bindings.C.Internal.Functions where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign (FunPtr, Ptr)
import Foreign.C (CChar (..), CDouble (..), CInt (..), CString, CUChar (..), CUInt (..))
import Sqlite3.Bindings.C.Internal.Constants
import Sqlite3.Bindings.C.Internal.Objects

-- | https://www.sqlite.org/c3ref/aggregate_context.html
--
-- Allocate memory for an aggregate function.
foreign import ccall unsafe
  sqlite3_aggregate_context ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Size, in bytes.
    CInt ->
    -- | Memory.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/auto_extension.html
--
-- Register an extension that is automatically loaded by every new connection.
foreign import ccall unsafe
  sqlite3_auto_extension ::
    -- | Extension entry point.
    FunPtr (Ptr Sqlite3 -> Ptr CString -> Ptr Sqlite3_api_routines -> IO CInt) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/autovacuum_pages.html
--
-- Register a callback that is invoked prior to each autovacuum.
foreign import ccall unsafe
  sqlite3_autovacuum_pages ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Callback.
    FunPtr (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt) ->
    -- | Application data.
    Ptr a ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Release a backup.
foreign import ccall unsafe
  sqlite3_backup_finish ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Initialize a backup.
foreign import ccall unsafe
  sqlite3_backup_init ::
    -- | Destination connection.
    Ptr Sqlite3 ->
    -- | Destination database name (UTF-8).
    CString ->
    -- | Source connection.
    Ptr Sqlite3 ->
    -- | Source database name (UTF-8).
    CString ->
    -- | Backup.
    IO (Ptr Sqlite3_backup)

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Get the number of pages in the source database of a backup.
foreign import ccall unsafe
  sqlite3_backup_pagecount ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Number of pages.
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Get the number of pages yet to be copied from the source database to the destination database of a backup.
foreign import ccall unsafe
  sqlite3_backup_remaining ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Number of pages.
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Copy pages from the source database to the destination database of a backup.
foreign import ccall safe
  sqlite3_backup_step ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Number of pages to copy.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
foreign import ccall safe
  sqlite3_bind_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    CInt ->
    -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
foreign import ccall safe
  sqlite3_bind_blob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    Word64 ->
    -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a double to a parameter.
foreign import ccall safe
  sqlite3_bind_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Double.
    CDouble ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
foreign import ccall safe
  sqlite3_bind_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Integer.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
foreign import ccall safe
  sqlite3_bind_int64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Integer.
    Int64 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter.
foreign import ccall safe
  sqlite3_bind_null ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_parameter_count.html
--
-- Get the index of the largest parameter.
foreign import ccall unsafe
  sqlite3_bind_parameter_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based), or 0 (no parameters).
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_parameter_index.html
--
-- Get the index of a named parameter.
foreign import ccall unsafe
  sqlite3_bind_parameter_index ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter name (UTF-8).
    CString ->
    -- | Parameter index (1-based), or 0 (not found).
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_parameter_name.html
--
-- Get the name of a named parameter.
foreign import ccall unsafe
  sqlite3_bind_parameter_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Parameter name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter, and associate it with a pointer.
foreign import ccall safe
  sqlite3_bind_pointer ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Pointer.
    Ptr a ->
    -- | Pointer type.
    CString ->
    -- | Pointer destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
foreign import ccall safe
  sqlite3_bind_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    CInt ->
    -- | String destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
foreign import ccall safe
  sqlite3_bind_text64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    Word64 ->
    -- | String destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Encoding.
    CUChar ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a value to a parameter.
foreign import ccall safe
  sqlite3_bind_value ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob of zeroes to a parameter.
foreign import ccall safe
  sqlite3_bind_zeroblob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Size of blob, in bytes.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob of zeroes to a parameter.
foreign import ccall safe
  sqlite3_bind_zeroblob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Size of blob, in bytes.
    Word64 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_bytes.html
--
-- Get the size of a blob, in bytes.
foreign import ccall unsafe
  sqlite3_blob_bytes ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Size of blob, in bytes.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_close.html
--
-- Close a blob.
foreign import ccall unsafe
  sqlite3_blob_close ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_open.html
--
-- Open a blob.
foreign import ccall unsafe
  sqlite3_blob_open ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Table name (UTF-8).
    CString ->
    -- | Column name (UTF-8).
    CString ->
    -- | Rowid.
    Int64 ->
    -- | Read-only if 0, else read-write.
    CInt ->
    -- | /Out/: blob.
    Ptr (Ptr Sqlite3_blob) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_read.html
--
-- Read data from a blob.
foreign import ccall unsafe
  sqlite3_blob_read ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Buffer to read into.
    Ptr a ->
    -- | Size of buffer to read into.
    CInt ->
    -- | Byte offset into blob to read from.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_reopen.html
--
-- Point an open blob at a different blob in the same table.
foreign import ccall unsafe
  sqlite3_blob_reopen ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Rowid.
    Int64 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_write.html
--
-- Write data to a blob.
foreign import ccall unsafe
  sqlite3_blob_write ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Buffer of data to write.
    Ptr a ->
    -- | Size of buffer to write.
    CInt ->
    -- | Byte offset into blob to write to.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/busy_handler.html
--
-- Register a callback that may be invoked when @SQLITE_BUSY@ would otherwise be returned from a function.
foreign import ccall unsafe
  sqlite3_busy_handler ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Callback.
    FunPtr (Ptr a -> CInt -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/busy_timeout.html
--
-- Register a 'sqlite3_busy_handler' callback that sleeps.
foreign import ccall unsafe
  sqlite3_busy_timeout ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of millseconds.
    CInt ->
    -- | Result code.
    IO CInt

-- -- | [__Cancel automatic extension loading__](https://www.sqlite.org/c3ref/cancel_auto_extension.html)
-- sqlite3_cancel_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/changes.html
--
-- Get the number of rows modified, inserted, or deleted by the most recent @UPDATE@, @INSERT@, or @DELETE@ statement on
-- a connection.
foreign import ccall unsafe
  sqlite3_changes ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of rows.
    IO CInt

-- | https://www.sqlite.org/c3ref/changes.html
--
-- Get the number of rows modified, inserted, or deleted by the most recent @UPDATE@, @INSERT@, or @DELETE@ statement on
-- a connection.
foreign import ccall unsafe
  sqlite3_changes64 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of rows.
    IO Int64

-- | https://www.sqlite.org/c3ref/clear_bindings.html
--
-- Clear parameter bindings.
foreign import ccall unsafe
  sqlite3_clear_bindings ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
--
-- Close a database connection.
foreign import ccall safe
  sqlite3_close ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
--
-- Attempt to close a database connection, but if it has any unfinalized statements, open blob handlers, or unfinished
-- backups, mark the connection as unusable and make arrangements to deallocate it after all statements are finalized,
-- blob handlers are closed, and backups are finished.
foreign import ccall safe
  sqlite3_close_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/collation_needed.html
--
-- Register a callback that is invoked when a collating sequence is needed.
foreign import ccall unsafe
  sqlite3_collation_needed ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Application data.
    Ptr a ->
    -- | Callback.
    FunPtr (Ptr a -> Ptr Sqlite3 -> CInt -> CString -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the blob of a result column.
foreign import ccall unsafe
  sqlite3_column_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Blob.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the size of a blob or string result column, in bytes.
foreign import ccall unsafe
  sqlite3_column_bytes ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Size, in bytes.
    IO CInt

-- | https://www.sqlite.org/c3ref/column_count.html
--
-- Get the number of columns in a result set.
foreign import ccall unsafe
  sqlite3_column_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Number of columns.
    IO CInt

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original database name for a result column.
foreign import ccall unsafe
  sqlite3_column_database_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Database name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/column_decltype.html
--
-- Get the declared type of a result column.
foreign import ccall unsafe
  sqlite3_column_decltype ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Type (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the double of a result column.
foreign import ccall unsafe
  sqlite3_column_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Double.
    IO CDouble

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
foreign import ccall unsafe
  sqlite3_column_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Integer.
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
foreign import ccall unsafe
  sqlite3_column_int64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Integer.
    IO Int64

-- | https://www.sqlite.org/c3ref/column_name.html
--
-- Get the column name of a result column.
foreign import ccall unsafe
  sqlite3_column_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Column name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original column name of a result column.
foreign import ccall unsafe
  sqlite3_column_origin_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Column name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original table name for a result column.
foreign import ccall unsafe
  sqlite3_column_table_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Table name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the string of a result column.
foreign import ccall unsafe
  sqlite3_column_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | String (UTF-8).
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the type of a result column.
foreign import ccall unsafe
  sqlite3_column_type ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Type.
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the unprotected value of a result column.
foreign import ccall unsafe
  sqlite3_column_value ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Value.
    IO (Ptr Sqlite3_value)

-- | https://www.sqlite.org/c3ref/commit_hook.html
--
-- Register a callback that is invoked whenever a transaction is committed.
foreign import ccall unsafe
  sqlite3_commit_hook ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Commit hook.
    FunPtr (Ptr a -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | Previous application data.
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get a compile-time option name.
foreign import ccall unsafe
  sqlite3_compileoption_get ::
    -- | Option index (0-based).
    CInt ->
    -- | Option name (UTF-8).
    CString

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get whether an option was specified at compile-time.
foreign import ccall unsafe
  sqlite3_compileoption_used ::
    -- | Option name (UTF-8).
    CString ->
    -- | @0@ or @1@.
    CInt

-- | https://www.sqlite.org/c3ref/complete.html
--
-- Get whether an SQL statement is complete.
foreign import ccall unsafe
  sqlite3_complete ::
    -- | SQL (UTF-8).
    CString ->
    -- | @0@ or @1@.
    CInt

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigheap
--
-- Specify memory that SQLite can use for dynamic allocations.
sqlite3_config_heap ::
  -- | Memory (8-byte aligned).
  Ptr a ->
  -- | Size of memory, in bytes.
  CInt ->
  -- | Minimum allocation size, in bytes.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_config_heap =
  sqlite3_config__2 _SQLITE_CONFIG_HEAP

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiglog
--
-- Register a callback that is invoked whenever an anomaly occurs.
sqlite3_config_log ::
  -- | Logging function.
  FunPtr (Ptr a -> CInt -> CString -> IO ()) ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_config_log =
  sqlite3_config__5 _SQLITE_CONFIG_LOG

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiglookaside
--
-- Set the default lookaside memory size.
sqlite3_config_lookaside ::
  -- | Size of each lookaside buffer slot, in bytes.
  CInt ->
  -- | Number of lookaside buffer slots allocated to each connection.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_config_lookaside =
  sqlite3_config__4 _SQLITE_CONFIG_LOOKASIDE

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmemstatus
--
-- Set whether memory allocation statistics are collected.
sqlite3_config_memstatus ::
  -- @0@ or @1@.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_config_memstatus =
  sqlite3_config__3 _SQLITE_CONFIG_MEMSTATUS

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmultithread
--
-- Set the threading mode to multi-thread.
sqlite3_config_multithread ::
  -- | Result code.
  IO CInt
sqlite3_config_multithread =
  sqlite3_config__1 _SQLITE_CONFIG_MULTITHREAD

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigpagecache
--
-- Specify memory that SQLite can use for the page cache.
sqlite3_config_pagecache ::
  -- | Memory (8-byte aligned).
  Ptr a ->
  -- | Size of each page cache line, in bytes.
  CInt ->
  -- | Number of cache lines.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_config_pagecache =
  sqlite3_config__2 _SQLITE_CONFIG_PAGECACHE

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigserialized
--
-- Set the threading mode to serialized.
sqlite3_config_serialized ::
  -- | Result code.
  IO CInt
sqlite3_config_serialized =
  sqlite3_config__1 _SQLITE_CONFIG_SERIALIZED

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiggetpcache2

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmemdbmaxsize

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmmapsize

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigpcache2

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigpcachehdrsz

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigpmasz

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigsmallmalloc

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigsorterrefsize

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigsqllog

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigstmtjrnlspill

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiguri

-- TODO
-- https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigwin32heapsize

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigsinglethread
--
-- Set the threading mode to single-thread.
sqlite3_config_singlethread ::
  -- | Result code.
  IO CInt
sqlite3_config_singlethread =
  sqlite3_config__1 _SQLITE_CONFIG_SINGLETHREAD

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__1 ::
    CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__2 ::
    CInt -> Ptr a -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__3 ::
    CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__4 ::
    CInt -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__5 ::
    CInt -> FunPtr (Ptr a -> CInt -> CString -> IO ()) -> Ptr a -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__6 ::
    CInt -> Ptr (Sqlite3_pcache_methods2 a) -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__7 ::
    CInt -> FunPtr (Ptr a -> Ptr Sqlite3 -> CString -> CInt -> IO ()) -> Ptr a -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__8 ::
    CInt -> Int64 -> Int64 -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__9 ::
    CInt -> Ptr CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__10 ::
    CInt -> CUInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config"
  sqlite3_config__11 ::
    CInt -> Int64 -> IO CInt

-- | https://www.sqlite.org/c3ref/context_db_handle.html
--
-- Get the connection for a function.
foreign import ccall unsafe
  sqlite3_context_db_handle ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Connection.
    IO (Ptr Sqlite3)

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
foreign import ccall safe
  sqlite3_create_collation ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Collating sequence name (UTF-8).
    CString ->
    -- | Encoding.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Collating sequence.
    FunPtr (Ptr a -> CInt -> Ptr b -> CInt -> Ptr b -> IO CInt) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
foreign import ccall safe
  sqlite3_create_collation_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Collating sequence name (UTF-8).
    CString ->
    -- | Encoding.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Collating sequence.
    FunPtr (Ptr a -> CInt -> Ptr b -> CInt -> Ptr b -> IO CInt) ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Create a VFS filename.
foreign import ccall unsafe
  sqlite3_create_filename ::
    -- | Database file.
    CString ->
    -- | Journal file.
    CString ->
    -- | WAL file.
    CString ->
    -- | Number of URI parameters.
    CInt ->
    -- | URI parameters (UTF-8).
    Ptr CString ->
    -- | Database file.
    IO CString

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create a function or aggregate function.
foreign import ccall unsafe
  sqlite3_create_function ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name (UTF-8).
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Encoding and flags.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Function.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Aggregate function step.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Aggregate function finalize.
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create a function or aggregate function.
foreign import ccall unsafe
  sqlite3_create_function_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name (UTF-8).
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Encoding and flags.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Function.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Aggregate function step.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Aggregate function finalize.
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
foreign import ccall unsafe
  sqlite3_create_module ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Module name (UTF-8).
    CString ->
    -- | Module.
    Ptr (Sqlite3_module a) ->
    -- | Application data.
    Ptr a ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
foreign import ccall unsafe
  sqlite3_create_module_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Module name (UTF-8).
    CString ->
    -- | Module.
    Ptr (Sqlite3_module a) ->
    -- | Application data.
    Ptr a ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create an aggregate window function.
foreign import ccall unsafe
  sqlite3_create_window_function ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name (UTF-8).
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Flags.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Aggregate function step.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Aggregate function finalize.
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    -- | Aggregate window function get current value.
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    -- | Aggregate window function remove value.
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/data_count.html
--
-- Get the number of columns in the next row of a result set.
foreign import ccall unsafe
  sqlite3_data_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Number of columns.
    IO CInt

-- | https://www.sqlite.org/c3ref/database_file_object.html
--
-- Get the database file object for a journal file.
foreign import ccall unsafe
  sqlite3_database_file_object ::
    -- | Journal file (UTF-8).
    CString ->
    -- | Database file object.
    IO (Ptr Sqlite3_file)

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
--
-- Flush all databases' dirty pager-cache pages to disk.
foreign import ccall safe
  sqlite3_db_cacheflush ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
--
-- TODO/FIXME
foreign import capi unsafe "sqlite3.h sqlite3_db_config"
  sqlite3_db_config__1 ::
    Ptr Sqlite3 -> CInt -> CString -> IO CInt

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
--
-- TODO/FIXME
foreign import capi unsafe "sqlite3.h sqlite3_db_config"
  sqlite3_db_config__2 ::
    Ptr Sqlite3 -> CInt -> Ptr a -> CInt -> CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
--
-- TODO/FIXME
foreign import capi unsafe "sqlite3.h sqlite3_db_config"
  sqlite3_db_config__3 ::
    Ptr Sqlite3 -> CInt -> CInt -> Ptr CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/db_filename.html
--
-- Get the filename for a database.
foreign import ccall unsafe
  sqlite3_db_filename ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Filename (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/db_handle.html
--
-- Get the connection for a statement.
foreign import ccall unsafe
  sqlite3_db_handle ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Connection.
    Ptr Sqlite3

-- | https://www.sqlite.org/c3ref/db_mutex.html
--
-- Get the mutex of a connection.
foreign import ccall unsafe
  sqlite3_db_mutex ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Mutex.
    IO (Ptr Sqlite3_mutex)

-- | https://www.sqlite.org/c3ref/db_name.html
--
-- Get the name of a database.
foreign import ccall unsafe
  sqlite3_db_name ::
    -- | Connection
    Ptr Sqlite3 ->
    -- | Database index (0-based; 0 is the main database file).
    CInt ->
    -- | Database name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/db_readonly.html
--
-- Get whether a database is read-only.
foreign import ccall unsafe
  sqlite3_db_readonly ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | @-1@ (not attached), @0@ (not read-only), or @1@ (read-only).
    IO CInt

-- | https://www.sqlite.org/c3ref/db_release_memory.html
--
-- Release as much memory as possible from a connection.
foreign import ccall unsafe
  sqlite3_db_release_memory ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/db_status.html
--
-- Get a connection status value.
foreign import ccall unsafe
  sqlite3_db_status ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Status option.
    CInt ->
    -- | /Out/: current value.
    Ptr CInt ->
    -- | /Out/: highest value.
    Ptr CInt ->
    -- | Reset the highest value to the current value?
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/declare_vtab.html
--
-- Declare the schema of a virtual table.
foreign import ccall unsafe
  sqlite3_declare_vtab ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Schema (UTF-8).
    CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/deserialize.html
--
-- Deserialize a database.
foreign import ccall unsafe
  sqlite3_deserialize ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Serialized database.
    Ptr CUChar ->
    -- | Size of serialized database, in bytes.
    Int64 ->
    -- | Size of serialized database buffer, in bytes. If this is larger than the previous argument, and
    -- `SQLITE_DESERIALIZE_READONLY` is not set, then SQLite may write to the unused memory.
    Int64 ->
    -- | Flags.
    CUInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/drop_modules.html
--
-- Remove virtual table modules.
foreign import ccall unsafe
  sqlite3_drop_modules ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Names of virtual table modules to keep (UTF-8).
    Ptr CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/exec.html
--
-- Execute zero or more SQL statements separated by semicolons.
foreign import ccall safe
  sqlite3_exec ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | SQL (UTF-8).
    CString ->
    -- | Callback.
    FunPtr (Ptr a -> CInt -> Ptr CString -> Ptr CString -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | /Out/: error message.
    Ptr CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
--
-- Set whether to return extended result codes.
foreign import ccall unsafe
  sqlite3_extended_result_codes ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | @0@ or @1@.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the result code of the most recent failure on a connection.
foreign import ccall unsafe
  sqlite3_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the error message of the most recent failure on a connection.
foreign import ccall unsafe
  sqlite3_errmsg ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Error message (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the byte offset into the SQL that the most recent failure on a connection refers to.
foreign import ccall unsafe
  sqlite3_error_offset ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Byte offset, or @-1@ if not applicable.
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the error message of a result code.
foreign import ccall unsafe
  sqlite3_errstr ::
    -- | Result code.
    CInt ->
    -- | Error message (UTF-8).
    CString

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the expanded SQL of a statement.
foreign import ccall unsafe
  sqlite3_expanded_sql ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | SQL (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the extended result code of the most recent failure on a connection.
foreign import ccall unsafe
  sqlite3_extended_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/file_control.html
--
-- Call @xFileControl@.
foreign import ccall unsafe
  sqlite3_file_control ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Opcode.
    CInt ->
    -- | Application data.
    Ptr a ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the database file for a database file, journal file, or WAL file.
foreign import ccall unsafe
  sqlite3_filename_database ::
    -- | Database file, journal file, or WAL file.
    CString ->
    -- | Database file.
    IO CString

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the journal file for a database file, journal file, or WAL file.
foreign import ccall unsafe
  sqlite3_filename_journal ::
    -- | Database file, journal file, or WAL file.
    CString ->
    -- | Journal file.
    IO CString

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the WAL file for a database file, journal file, or WAL file.
foreign import ccall unsafe
  sqlite3_filename_wal ::
    -- | Database file, journal file, or WAL file.
    CString ->
    -- | WAL file.
    IO CString

-- | https://www.sqlite.org/c3ref/finalize.html
--
-- Release a statement.
foreign import ccall safe
  sqlite3_finalize ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/free.html
--
-- Release memory acquired by 'sqlite3_malloc', 'sqlite3_malloc64', 'sqlite3_realloc', or 'sqlite3_realloc64'.
foreign import ccall unsafe
  sqlite3_free ::
    -- | Memory.
    Ptr a ->
    IO ()

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Release a VFS filename.
foreign import ccall unsafe
  sqlite3_free_filename ::
    -- | Filename (UTF-8).
    CString ->
    IO ()

-- | https://www.sqlite.org/c3ref/get_autocommit.html
--
-- Get whether a connection is in autocommit mode.
foreign import ccall unsafe
  sqlite3_get_autocommit ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/get_auxdata.html
--
-- Get the metadata of a function argument.
foreign import ccall unsafe
  sqlite3_get_auxdata ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Argument index (0-based).
    CInt ->
    -- | Metadata.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
foreign import ccall unsafe
  sqlite3_hard_heap_limit64 ::
    -- | Limit, in bytes, or a negative number to get the limit.
    Int64 ->
    -- | Previous limit, in bytes.
    IO Int64

-- | https://www.sqlite.org/c3ref/initialize.html
--
-- Initialize the library.
foreign import ccall unsafe
  sqlite3_initialize ::
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/interrupt.html
--
-- Cause all in-progress operations to return `SQLITE_INTERRUPT` at the earliest opportunity.
foreign import ccall unsafe
  sqlite3_interrupt ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO ()

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- Get whether a string is a keyword.
foreign import ccall unsafe
  sqlite3_keyword_check ::
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    CInt ->
    -- | @0@ or @1@.
    CInt

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- The number of distinct keywords.
foreign import ccall unsafe
  sqlite3_keyword_count :: CInt

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- Get a keyword by index.
foreign import ccall unsafe
  sqlite3_keyword_name ::
    -- | Keyword index (0-based).
    CInt ->
    -- | /Out/: keyword (UTF-8).
    Ptr (Ptr CChar) ->
    -- | /Out/: size of keyword, in bytes.
    Ptr CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/last_insert_rowid.html
--
-- Get the rowid of the most recent insert into a rowid table.
foreign import ccall unsafe
  sqlite3_last_insert_rowid ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Rowid.
    IO Int64

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
foreign import ccall unsafe
  sqlite3_libversion :: CString

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
foreign import ccall unsafe
  sqlite3_libversion_number :: CInt

-- | https://www.sqlite.org/c3ref/limit.html
--
-- Get or set a limit on a connection.
foreign import ccall unsafe
  sqlite3_limit ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Limit category.
    CInt ->
    -- | Limit, or a negative number to get the limit.
    CInt ->
    -- | Previous limit.
    IO CInt

-- | https://www.sqlite.org/c3ref/load_extension.html
foreign import ccall unsafe
  sqlite3_load_extension ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Extension shared library name.
    CString ->
    -- | Entry point.
    CString ->
    -- | /Out/: error message.
    Ptr CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/log.html
--
-- Write a message to the error log.
foreign import capi unsafe "sqlite3.h sqlite3_log"
  sqlite3_log ::
    -- | Result code.
    CInt ->
    -- | Error message.
    CString ->
    IO ()

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
foreign import ccall unsafe
  sqlite3_malloc ::
    -- | Size of memory, in bytes.
    CInt ->
    -- | Memory.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
foreign import ccall unsafe
  sqlite3_malloc64 ::
    -- | Size of memory, in bytes.
    Word64 ->
    -- | Memory.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the highest value of 'sqlite3_memory_used'.
foreign import ccall unsafe
  sqlite3_memory_highwater ::
    -- | Reset highest value? (@0@ or @1@).
    CInt ->
    -- | Highest value (prior to this reset, if this is a reset).
    IO Int64

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the size of live memory, in bytes.
foreign import ccall unsafe
  sqlite3_memory_used ::
    -- | Size of memory, in bytes.
    IO Int64

-- | https://www.sqlite.org/c3ref/free.html
--
-- Get the size of memory allocated with 'sqlite3_malloc', 'sqlite3_malloc64', 'sqlite3_realloc', or
-- 'sqlite3_realloc64', in bytes.
foreign import ccall unsafe
  sqlite3_msize ::
    -- | Memory.
    Ptr a ->
    -- | Size of memory, in bytes.
    IO Word64

-- sqlite3_mutex_held = undefined

-- sqlite3_mutex_notheld = undefined

-- | https://www.sqlite.org/c3ref/next_stmt.html
--
-- Get the next statement of a connection.
foreign import ccall unsafe
  sqlite3_next_stmt ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Next statement.
    IO (Ptr Sqlite3_stmt)

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the normalized SQL of a statement.
foreign import ccall unsafe
  sqlite3_normalized_sql ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | SQL (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/open.html
--
-- Open a new database connection.
foreign import ccall safe
  sqlite3_open ::
    -- | Database file (UTF-8).
    CString ->
    -- | /Out/: connection.
    Ptr (Ptr Sqlite3) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/open.html
--
-- Open a new database connection.
foreign import ccall safe
  sqlite3_open_v2 ::
    -- | Database file (UTF-8).
    CString ->
    -- | /Out/: connection.
    Ptr (Ptr Sqlite3) ->
    -- | Flags.
    CInt ->
    -- | Name of VFS to use.
    CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/overload_function.html
--
-- Ensure a placeholder function exists, to be overloaded by @xFindFunction@.
foreign import ccall unsafe
  sqlite3_overload_function ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name (UTF-8).
    CString ->
    -- | Number of arguments.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/prepare.html
--
-- Compile a statement.
foreign import ccall safe
  sqlite3_prepare_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | SQL (UTF-8).
    Ptr CChar ->
    -- | Size of SQL, in bytes.
    CInt ->
    -- | /Out/: statement.
    Ptr (Ptr Sqlite3_stmt) ->
    -- | /Out/: unused SQL.
    Ptr (Ptr CChar) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/prepare.html
--
-- Compile a statement.
foreign import ccall safe
  sqlite3_prepare_v3 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | SQL (UTF-8).
    Ptr CChar ->
    -- | Size of SQL, in bytes.
    CInt ->
    -- | Flags.
    CUInt ->
    -- | /Out/: statement.
    Ptr (Ptr Sqlite3_stmt) ->
    -- | /Out/: unused SQL.
    Ptr (Ptr CChar) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the index of the column being written with 'sqlite3_blob_write'.
foreign import ccall unsafe
  sqlite3_preupdate_blobwrite ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Column index, or @-1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the number of columns in the row being updated.
foreign import ccall unsafe
  sqlite3_preupdate_count ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of columns.
    IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the "trigger depth" of this update, i.e. @0@ for a direct update, or @n@ for an update caused by a trigger that
-- was invoked by a depth @n-1@ update.
foreign import ccall unsafe
  sqlite3_preupdate_depth ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Trigger depth.
    IO CInt

-- TODO
-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- foreign import ccall unsafe
--   sqlite3_preupdate_hook ::
--     Ptr Sqlite3 ->
--     FunPtr (Ptr a -> Ptr Sqlite3 -> CInt -> CString -> CString -> Int64 -> Int64 -> IO ()) ->
--     Ptr a ->
--     IO (Ptr b)

-- TODO
-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- foreign import ccall unsafe
--   sqlite3_preupdate_new ::
--     Ptr Sqlite3 ->
--     CInt ->
--     Ptr (Ptr Sqlite3_value) ->
--     IO CInt

-- TODO
-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- foreign import ccall unsafe
--   sqlite3_preupdate_old ::
--     Ptr Sqlite3 ->
--     CInt ->
--     Ptr (Ptr Sqlite3_value) ->
--     IO CInt

-- | https://www.sqlite.org/c3ref/progress_handler.html
--
-- Register a callback that is invoked periodically during long-running queries.
foreign import ccall unsafe
  sqlite3_progress_handler ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Approximate number of virtual machine instructions that are evaluated between successive invocations of the
    -- callback.
    CInt ->
    -- | Callback.
    FunPtr (Ptr a -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    IO ()

-- | https://www.sqlite.org/c3ref/randomness.html
--
-- Generate random bytes.
foreign import ccall unsafe
  sqlite3_randomness ::
    -- | Number of bytes to generate.
    CInt ->
    -- | /Out/: buffer.
    Ptr a ->
    IO ()

-- | https://www.sqlite.org/c3ref/free.html
--
-- Resize a memory allocation.
foreign import ccall unsafe
  sqlite3_realloc ::
    -- | Memory.
    Ptr a ->
    -- | Size of memory, in bytes.
    CInt ->
    -- | Memory.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/free.html
--
-- Resize a memory allocation.
foreign import ccall unsafe
  sqlite3_realloc64 ::
    -- | Memory.
    Ptr a ->
    -- | Size of memory, in bytes.
    Int64 ->
    -- | Memory.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/release_memory.html
--
-- Attempt to release memory by deallocating non-essential allocations, such as cache database pages used to improve
-- performance.
foreign import ccall unsafe
  sqlite3_release_memory ::
    -- | Number of bytes to release.
    CInt ->
    -- | Number of bytes actually released (may be more or less than the requested amount).
    IO CInt

-- | https://www.sqlite.org/c3ref/reset.html
--
-- Reset a statement to its initial state. Does not clear parameter bindings.
foreign import ccall safe
  sqlite3_reset ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- -- | https://www.sqlite.org/c3ref/reset_auto_extension.html
-- sqlite3_reset_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob from a function.
foreign import ccall unsafe
  sqlite3_result_blob ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    CInt ->
    -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob from a function.
foreign import ccall unsafe
  sqlite3_result_blob64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    Word64 ->
    -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a double from a function.
foreign import ccall unsafe
  sqlite3_result_double ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Double.
    CDouble ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw an exception from a function.
foreign import ccall unsafe
  sqlite3_result_error ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Error message (UTF-8).
    CString ->
    -- | Size of error message, in bytes, or @-1@ to use the entire message.
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw an exception from a function.
foreign import ccall unsafe
  sqlite3_result_error_code ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Result code.
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw a @SQLITE_NOMEM@ exception from a function.
foreign import ccall unsafe
  sqlite3_result_error_nomem ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw a @SQLITE_TOOBIG@ exception from a function.
foreign import ccall unsafe
  sqlite3_result_error_toobig ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return an integer from a function.
foreign import ccall unsafe
  sqlite3_result_int ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Integer.
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return an integer from a function.
foreign import ccall unsafe
  sqlite3_result_int64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Integer.
    Int64 ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return null from a function.
foreign import ccall unsafe
  sqlite3_result_null ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return null from a function, and associate it with a pointer.
foreign import ccall unsafe
  sqlite3_result_pointer ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Pointer.
    Ptr a ->
    -- | Pointer type.
    CString ->
    -- | Pointer destructor.
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_subtype.html
--
-- Set the subtype of the return value of a function.
foreign import ccall unsafe
  sqlite3_result_subtype ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Subtype.
    CUInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a string from a function.
foreign import ccall unsafe
  sqlite3_result_text ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    CInt ->
    -- | String destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a string from a function.
foreign import ccall unsafe
  sqlite3_result_text64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    Word64 ->
    -- | String destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Encoding.
    CUChar ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a value from a function.
foreign import ccall unsafe
  sqlite3_result_value ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Value.
    Ptr Sqlite3_value ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob of zeroes from a function.
foreign import ccall unsafe
  sqlite3_result_zeroblob ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Size of blob, in bytes.
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob of zeroes from a function.
foreign import ccall unsafe
  sqlite3_result_zeroblob64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Size of blob, in bytes.
    Word64 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/commit_hook.html
--
-- Register a callback that is invoked whenever a transaction is committed.
foreign import ccall unsafe
  sqlite3_rollback_hook ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Rollback hook.
    FunPtr (Ptr a -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | Previous application data.
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/serialize.html
--
-- Serialize a database.
foreign import ccall unsafe
  sqlite3_serialize ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | /Out/: size of database, in bytes.
    Ptr Int64 ->
    -- | Flags.
    CUInt ->
    -- | Serialized database.
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/set_authorizer.html
--
-- Register a callback that is invoked during statement preparation to authorize actions.
foreign import ccall unsafe
  sqlite3_set_authorizer ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Callback.
    FunPtr (Ptr a -> CInt -> CString -> CString -> CString -> CString -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/get_auxdata.html
--
-- Set the metadata of a function argument.
foreign import ccall unsafe
  sqlite3_set_auxdata ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Argument index (0-based).
    CInt ->
    -- | Metadata.
    Ptr a ->
    -- | Metadata destructor.
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/set_last_insert_rowid.html
--
-- Set the return value of the next 'sqlite3_last_insert_rowid'.
foreign import ccall unsafe
  sqlite3_set_last_insert_rowid ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Rowid.
    Int64 ->
    IO ()

-- | https://www.sqlite.org/c3ref/initialize.html
--
-- Deinitialize the library.
foreign import ccall unsafe
  sqlite3_shutdown ::
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/sleep.html
--
-- Suspend execution.
foreign import ccall safe
  sqlite3_sleep ::
    -- | Duration, in milliseconds.
    CInt ->
    -- | Duration actually suspended, in milliseconds.
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_cmp.html
--
-- Compare the ages of two snapshots of the same database.
foreign import ccall unsafe
  sqlite3_snapshot_cmp ::
    -- | First snapshot.
    Ptr Sqlite3_snapshot ->
    -- | Second snapshot.
    Ptr Sqlite3_snapshot ->
    -- | Negative if first snapshot is older, @0@ if the snapshots are equal, or positive if the first snapshot is newer.
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_free.html
--
-- Release a snapshot.
foreign import ccall unsafe
  sqlite3_snapshot_free ::
    -- | Snapshot.
    Ptr Sqlite3_snapshot ->
    IO ()

-- | https://www.sqlite.org/c3ref/snapshot_get.html
--
-- Create a snapshot.
foreign import ccall unsafe
  sqlite3_snapshot_get ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | /Out/: snapshot.
    Ptr (Ptr Sqlite3_snapshot) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_open.html
--
-- Begin a read transaction on a snapshot.
foreign import ccall unsafe
  sqlite3_snapshot_open ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Snapshot.
    Ptr Sqlite3_snapshot ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_recover.html
--
-- Recover snapshots from a WAL file.
foreign import ccall unsafe
  sqlite3_snapshot_recover ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
foreign import ccall unsafe
  sqlite3_soft_heap_limit64 ::
    -- | Limit, in bytes, or a negative number to get the limit.
    Int64 ->
    -- | Previous limit, in bytes.
    IO Int64

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The date, time, and hash of the library check-in.
foreign import ccall unsafe
  sqlite3_sourceid :: CString

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the SQL of a statement.
foreign import ccall unsafe
  sqlite3_sql ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | SQL (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/status.html
--
-- Get a library status value.
foreign import ccall unsafe
  sqlite3_status ::
    -- | Status code.
    CInt ->
    -- | /Out/: current value.
    Ptr CInt ->
    -- | /Out/: highest value.
    Ptr CInt ->
    -- | Reset highest value? (@0@ or @1@).
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/status.html
--
-- Get a library status value.
foreign import ccall unsafe
  sqlite3_status64 ::
    -- | Status code.
    CInt ->
    -- | /Out/: current value.
    Ptr Int64 ->
    -- | /Out/: highest value.
    Ptr Int64 ->
    -- | Reset highest value? (@0@ or @1@).
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
--
-- Produce the next row of a statement.
foreign import ccall safe
  sqlite3_step ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_busy.html
--
-- Get whether a statement is in-progress.
foreign import ccall unsafe
  sqlite3_stmt_busy ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_isexplain.html
--
-- Get whether a statement is an @EXPLAIN@.
foreign import ccall unsafe
  sqlite3_stmt_isexplain ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | @0@ (not @EXPLAIN@), @1@ (@EXPLAIN@), or @2@ (@EXPLAIN QUERY PLAN@).
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_readonly.html
--
-- Get whether a statement is read-only.
foreign import ccall unsafe
  sqlite3_stmt_readonly ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | @0@ or @1@.
    IO CInt

-- TODO
-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus.html
-- foreign import ccall unsafe
--   sqlite3_stmt_scanstatus ::
--     Ptr Sqlite3_stmt ->
--     CInt ->
--     CInt ->
--     Ptr a ->
--     IO CInt

-- TODO
-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus_reset.html
-- foreign import ccall unsafe
--   sqlite3_stmt_scanstatus_reset ::
--     Ptr Sqlite3_stmt ->
--     IO ()

-- | https://www.sqlite.org/c3ref/stmt_status.html
--
-- Get a statement status value.
foreign import ccall unsafe
  sqlite3_stmt_status ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Status code.
    CInt ->
    -- | Reset value? (@0@ or @1@).
    CInt ->
    -- | Value.
    IO CInt

-- | https://www.sqlite.org/c3ref/strglob.html
--
-- Get whether a string matches a glob pattern.
foreign import ccall unsafe
  sqlite3_strglob ::
    -- | Glob pattern (UTF-8).
    CString ->
    -- | String (UTF-8).
    CString ->
    -- | @0@ if matches.
    CInt

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding).
foreign import ccall unsafe
  sqlite3_stricmp ::
    -- | First string.
    CString ->
    -- | Second string.
    CString ->
    -- | Negative if first string is less than the second string, @0@ if the strings are equal, or positive if the first
    -- string is greater than the second string.
    CInt

-- | https://www.sqlite.org/c3ref/strlike.html
--
-- Get whether a string matches a like pattern.
foreign import ccall unsafe
  sqlite3_strlike ::
    -- | Like pattern (UTF-8).
    CString ->
    -- | String (UTF-8)
    CString ->
    -- | Escape character.
    CUInt ->
    -- | @0@ if matches.
    CInt

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding), up to a certain length.
foreign import ccall unsafe
  sqlite3_strnicmp ::
    -- | First string.
    CString ->
    -- | Second string.
    CString ->
    -- | Length, in bytes.
    CInt ->
    -- | Negative if first string is less than the second string, @0@ if the strings are equal, or positive if the first
    -- string is greater than the second string.
    CInt

-- | https://www.sqlite.org/c3ref/system_errno.html
--
-- Get the system result code of the most recent failure on a connection.
foreign import ccall unsafe
  sqlite3_system_errno ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/table_column_metadata.html
--
-- Get column metadata.
foreign import ccall unsafe
  sqlite3_table_column_metadata ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Table name (UTF-8).
    CString ->
    -- | Column name (UTF-8).
    CString ->
    -- | /Out/: type.
    Ptr CString ->
    -- | /Out/: default collating sequence name.
    Ptr CString ->
    -- | /Out/: @1@ if @NOT NULL@.
    Ptr CInt ->
    -- | /Out/: @1@ if part of the primary key.
    Ptr CInt ->
    -- | /Out/: @1@ if @AUTOINCREMENT@.
    Ptr CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/threadsafe.html
--
-- Get whether the library is thread-safe.
foreign import ccall unsafe
  sqlite3_threadsafe :: CInt

-- | https://www.sqlite.org/c3ref/total_changes.html
--
-- Get the total number of rows modified, inserted, and deleted by @UPDATE@, @INSERT@, and @DELETE@ statements made on a
-- connection.
foreign import ccall unsafe
  sqlite3_total_changes ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of rows.
    IO CInt

-- | https://www.sqlite.org/c3ref/total_changes.html
--
-- Get the total number of rows modified, inserted, and deleted by @UPDATE@, @INSERT@, and @DELETE@ statements made on a
-- connection.
foreign import ccall unsafe
  sqlite3_total_changes64 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of rows.
    IO Int64

-- | https://www.sqlite.org/c3ref/trace_v2.html
--
-- Register a callback that is invoked at various times.
foreign import ccall unsafe
  sqlite3_trace_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Property mask.
    CUInt ->
    -- | Callback.
    FunPtr (CUInt -> Ptr a -> Ptr b -> Ptr c -> IO CInt) ->
    -- | Context pointer.
    Ptr a ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/txn_state.html
--
-- Get the transaction state of a database.
foreign import ccall unsafe
  sqlite3_txn_state ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | @SQLITE_TXN_NONE@, @SQLITE_TXN_READ@, @SQLITE_TXN_WRITE@, or @-1@ if the database does not exist.
    IO CInt

-- | https://www.sqlite.org/c3ref/unlock_notify.html
foreign import ccall unsafe
  sqlite3_unlock_notify ::
    Ptr Sqlite3 ->
    FunPtr (Ptr (Ptr a) -> CInt -> IO ()) ->
    Ptr a ->
    IO CInt

-- | https://www.sqlite.org/c3ref/update_hook.html
foreign import ccall unsafe
  sqlite3_update_hook ::
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CInt -> CString -> CString -> Int64 -> IO ()) ->
    Ptr a ->
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a boolean query parameter of a database file.
foreign import ccall unsafe
  sqlite3_uri_boolean ::
    -- | Database file.
    CString ->
    -- | Query parameter name.
    CString ->
    -- | Default value.
    CInt ->
    -- | Query parameter value (@0@ or @1@).
    IO CInt

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get an integer query parameter of a database file.
foreign import ccall unsafe
  sqlite3_uri_int64 ::
    -- | Database file.
    CString ->
    -- | Query parameter name.
    CString ->
    -- | Default value.
    Int64 ->
    -- | Query parameter value.
    IO Int64

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a query parameter name of a database file.
foreign import ccall unsafe
  sqlite3_uri_key ::
    -- | Database file.
    CString ->
    -- | Query parameter index (0-based).
    CInt ->
    -- | Query parameter name.
    IO CString

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a query parameter of a database file.
foreign import ccall unsafe
  sqlite3_uri_parameter ::
    -- | Database file.
    CString ->
    -- | Query parameter name.
    CString ->
    -- | Query parameter value.
    IO CString

-- | https://www.sqlite.org/c3ref/user_data.html
foreign import ccall unsafe
  sqlite3_user_data ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the blob of a protected value.
foreign import ccall unsafe
  sqlite3_value_blob ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Blob.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the size of a protected blob or string value, in bytes.
foreign import ccall unsafe
  sqlite3_value_bytes ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Size, in bytes.
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the double of a protected value.
foreign import ccall unsafe
  sqlite3_value_double ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Double.
    IO CDouble

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Copy a value.
foreign import ccall unsafe
  sqlite3_value_dup ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Value copy (protected).
    IO (Ptr Sqlite3_value)

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Release memory acquired by 'sqlite3_value_dup'.
foreign import ccall unsafe
  sqlite3_value_free ::
    -- | Value.
    Ptr Sqlite3_value ->
    IO ()

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get whether a protected value is a bound parameter.
foreign import ccall unsafe
  sqlite3_value_frombind ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
foreign import ccall unsafe
  sqlite3_value_int ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Integer.
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
foreign import ccall unsafe
  sqlite3_value_int64 ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Integer.
    IO Int64

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Within @xUpdate@, get whether the column corresponding to a protected value is unchanged.
foreign import ccall unsafe
  sqlite3_value_nochange ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the numeric type of a protected value.
foreign import ccall unsafe
  sqlite3_value_numeric_type ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Type.
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the pointer of a protected value.
foreign import ccall unsafe
  sqlite3_value_pointer ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Pointer type.
    CString ->
    -- | Pointer.
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/value_subtype.html
--
-- Get the subtype of the return value of a function.
foreign import ccall unsafe
  sqlite3_value_subtype ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Subtype.
    IO CUInt

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the string of a protected value.
foreign import ccall unsafe
  sqlite3_value_text ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | String (UTF-8).
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the type of a protected value.
foreign import ccall unsafe
  sqlite3_value_type ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | Type.
    IO CInt

-- | https://www.sqlite.org/c3ref/libversion.html
foreign import capi unsafe "sqlite3.h value sqlite3_version"
  sqlite3_version :: CString

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Get a VFS.
foreign import ccall unsafe
  sqlite3_vfs_find ::
    -- | VFS name (UTF-8).
    CString ->
    -- | VFS.
    IO (Ptr Sqlite3_vfs)

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Register a VFS.
foreign import ccall unsafe
  sqlite3_vfs_register ::
    -- | VFS.
    Ptr Sqlite3_vfs ->
    -- | Make default? (@0@ or @1@).
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Unregister a VFS.
foreign import ccall unsafe
  sqlite3_vfs_unregister ::
    -- | VFS.
    Ptr Sqlite3_vfs ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_collation.html
--
-- Get the collating sequence of a virtual table constraint.
foreign import ccall unsafe
  sqlite3_vtab_collation ::
    -- | Index info (first argument to @xBestIndex@).
    Ptr Sqlite3_index_info ->
    -- | @aConstraint[]@ index.
    CInt ->
    -- | Collating sequence name (UTF-8).
    IO CString

-- | https://www.sqlite.org/c3ref/vtab_config.html
--
-- Configure a virtual table.
--
-- TODO/FIXME
foreign import capi unsafe "sqlite3.h sqlite3_vtab_config"
  sqlite3_vtab_config__1 ::
    Ptr Sqlite3 -> CInt -> CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/vtab_config.html
--
-- Configure a virtual table.
--
-- TODO/FIXME
foreign import capi unsafe "sqlite3.h sqlite3_vtab_config"
  sqlite3_vtab_config__2 ::
    Ptr Sqlite3 -> CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/vtab_distinct.html
--
-- Get information about how the query planner wants output to be ordered.
foreign import ccall unsafe
  sqlite3_vtab_distinct ::
    -- | Index info (first argument to @xBestIndex@).
    Ptr Sqlite3_index_info ->
    -- | @0@, @1@, @2@, or @3@.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_in.html
--
-- Get whether a virtual table constraint is an @IN@ operator that can be processed all at once.
foreign import ccall unsafe
  sqlite3_vtab_in ::
    -- | Index info (first argument to @xBestIndex@).
    Ptr Sqlite3_index_info ->
    -- | @aConstraint[]@ index.
    CInt ->
    -- | @-1@, @0@, or @1@.
    CInt ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the first value on the right-hand side of an @IN@ constraint.
foreign import ccall unsafe
  sqlite3_vtab_in_first ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | /Out/: Value.
    Ptr (Ptr Sqlite3_value) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the next value on the right-hand side of an @IN@ constraint.
foreign import ccall unsafe
  sqlite3_vtab_in_next ::
    -- | Value.
    Ptr Sqlite3_value ->
    -- | /Out/: Value.
    Ptr (Ptr Sqlite3_value) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_nochange.html
--
-- Within @xColumn@, get whether the column is being fetched as part of an @UPDATE@ in which its value will not change.
foreign import ccall unsafe
  sqlite3_vtab_nochange ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | @0@ or @1@.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_on_conflict.html
--
-- Within @xUpdate@ for an @INSERT@ or @UPDATE@, get the conflict resolution algorithm.
foreign import ccall unsafe
  sqlite3_vtab_on_conflict ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Conflict resolution algorithm.
    IO CInt

-- | https://www.sqlite.org/c3ref/vtab_rhs_value.html
--
-- Within @xBestIndex@, get the right-hand side of a virtual table constraint.
foreign import ccall unsafe
  sqlite3_vtab_rhs_value ::
    -- | Index info (first argument to @xBestIndex@).
    Ptr Sqlite3_index_info ->
    -- | @aConstraint[]@ index.
    CInt ->
    -- | /Out/: value.
    Ptr (Ptr Sqlite3_value) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_autocheckpoint.html
--
-- Register a callback that checkpoints the WAL after committing a transaction if there are more than a certain number
-- of frames in the WAL.
foreign import ccall unsafe
  sqlite3_wal_autocheckpoint ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of frames that will trigger a checkpoint.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_checkpoint.html
--
-- Checkpoint the WAL.
foreign import ccall unsafe
  sqlite3_wal_checkpoint ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_checkpoint_v2.html
--
-- Checkpoint the WAL.
foreign import ccall unsafe
  sqlite3_wal_checkpoint_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | Checkpoint mode.
    CInt ->
    -- | /Out/: number of frames in the WAL.
    Ptr CInt ->
    -- | /Out/: number of frames in the WAL that were checkpointed.
    Ptr CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_hook.html
--
-- Register a callback that is invoked each time data is committed to a database in WAL mode.
foreign import ccall unsafe
  sqlite3_wal_hook ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Callback.
    FunPtr (Ptr a -> Ptr Sqlite3 -> CString -> CInt -> IO CInt) ->
    -- | Application data.
    Ptr a ->
    -- | Previous application data.
    IO (Ptr b)
