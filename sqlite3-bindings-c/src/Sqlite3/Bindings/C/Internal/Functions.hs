module Sqlite3.Bindings.C.Internal.Functions where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign (FunPtr, Ptr)
import Foreign.C (CChar (..), CDouble (..), CInt (..), CString, CUChar (..), CUInt (..))
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
sqlite3_auto_extension = undefined

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
  -- safe because: sqlite3_autovacuum_pages, sqlite3_busy_handler
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
foreign import ccall unsafe
  sqlite3_bind_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    CInt ->
    -- | Destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
foreign import ccall unsafe
  sqlite3_bind_blob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Size of blob, in bytes.
    Word64 ->
    -- | Destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a double to a parameter.
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
  sqlite3_bind_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    CInt ->
    -- | Destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
foreign import ccall unsafe
  sqlite3_bind_text64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Size of string, in bytes.
    Word64 ->
    -- | Destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Encoding.
    CUChar ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a value to a parameter.
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
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
-- Point an open blob at a different row in the same table.
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

-- | [__Register a callback to handle `SQLITE_BUSY` errors__](https://www.sqlite.org/c3ref/busy_handler.html)
foreign import ccall unsafe
  sqlite3_busy_handler ::
    -- | Connection.
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CInt -> IO CInt) ->
    Ptr a ->
    IO CInt

-- | [__Set a busy timeout__](https://www.sqlite.org/c3ref/busy_timeout.html)
foreign import ccall unsafe
  sqlite3_busy_timeout ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Number of millseconds.
    CInt ->
    IO CInt

-- | [__Cancel automatic extension loading__](https://www.sqlite.org/c3ref/cancel_auto_extension.html)
sqlite3_cancel_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/changes.html
foreign import ccall unsafe
  sqlite3_changes ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/changes.html
foreign import ccall unsafe
  sqlite3_changes64 ::
    -- | Connection.
    Ptr Sqlite3 ->
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
foreign import ccall unsafe
  sqlite3_close ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
--
-- Close a database connection. If it has any unfinalized statements, open blob handlers, or unfinished backups, mark
-- the connection as unusable and make arrangements to deallocate it after all statements are finalized, blob handlers
-- are closed, and backups are finished.
foreign import ccall unsafe
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
foreign import ccall unsafe
  sqlite3_column_database_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
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
foreign import ccall unsafe
  sqlite3_column_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import ccall unsafe
  sqlite3_column_origin_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import ccall unsafe
  sqlite3_column_table_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
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
-- Get the value of a result column.
foreign import ccall unsafe
  sqlite3_column_value ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    -- | Value.
    IO (Ptr Sqlite3_value)

-- | https://www.sqlite.org/c3ref/commit_hook.html
foreign import ccall unsafe
  sqlite3_commit_hook ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Commit hook.
    FunPtr (Ptr a -> IO CInt) ->
    Ptr a ->
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get a compile-time option name.
foreign import ccall unsafe
  sqlite3_compileoption_get ::
    -- | Option index.
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
    -- | 0 or 1.
    CInt

-- | https://www.sqlite.org/c3ref/complete.html
foreign import ccall unsafe
  sqlite3_complete ::
    -- | SQL (UTF-8).
    CString ->
    CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__1 :: CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__2 :: CInt -> Ptr Sqlite3_mem_methods -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__3 :: CInt -> Ptr a -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__4 :: CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__5 :: CInt -> Ptr Sqlite3_mutex_methods -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__6 :: CInt -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__7 :: CInt -> FunPtr (Ptr a -> CInt -> CString -> IO ()) -> Ptr a -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__8 :: CInt -> Ptr Sqlite3_pcache_methods2 -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__9 :: CInt -> FunPtr (Ptr a -> Ptr Sqlite3 -> CString -> CInt -> IO ()) -> Ptr a -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__10 :: CInt -> Int64 -> Int64 -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__11 :: CInt -> Ptr CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__12 :: CInt -> CUInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_config" sqlite3_config__13 :: CInt -> Int64 -> IO CInt

-- | https://www.sqlite.org/c3ref/context_db_handle.html
foreign import ccall unsafe
  sqlite3_context_db_handle ::
    -- | Function context.
    Ptr Sqlite3_context ->
    -- | Connection.
    IO (Ptr Sqlite3)

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
foreign import ccall unsafe
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
foreign import ccall unsafe
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
    Ptr Sqlite3_module ->
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
    Ptr Sqlite3_module ->
    -- | Application data.
    Ptr a ->
    -- | Application data destructor.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create an aggregate function or an aggregate window function.
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
-- Get the file object for a journal.
foreign import ccall unsafe
  sqlite3_database_file_object ::
    CString ->
    IO (Ptr Sqlite3_file)

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
--
-- Flush all dirty pager-cache pages to disk, for all attached databases.
foreign import ccall safe
  -- safe because: sqlite3_busy_handler
  sqlite3_db_cacheflush ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_db_config" sqlite3_db_config__1 :: Ptr Sqlite3 -> CInt -> CString -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_db_config" sqlite3_db_config__2 :: Ptr Sqlite3 -> CInt -> Ptr a -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_db_config" sqlite3_db_config__3 :: Ptr Sqlite3 -> CInt -> CInt -> Ptr CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/db_filename.html
--
-- Get the filename for an attached database.
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
-- Get the name of an attached database.
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
-- Get whether an attached database is read-only.
foreign import ccall unsafe
  sqlite3_db_readonly ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name (UTF-8).
    CString ->
    -- | 0 or 1, or -1 if the database is not attached.
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
-- Get a status of a connection.
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

-- | [__Deserialize a database__](https://www.sqlite.org/c3ref/deserialize.html)
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
    IO CInt

-- | [__Remove unnecessary virtual table implementations__](https://www.sqlite.org/c3ref/drop_modules.html)
foreign import ccall unsafe
  sqlite3_drop_modules ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Names of virtual table modules to keep (UTF-8).
    Ptr CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/exec.html
--
-- Execute zero or more SQL statements separated by semicolons.
foreign import ccall safe
  -- safe because: sqlite3_autovacuum_pages,
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
foreign import ccall unsafe
  sqlite3_extended_result_codes ::
    -- | Connection.
    Ptr Sqlite3 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the error code of the most recent failure on a connection.
foreign import ccall unsafe
  sqlite3_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Error code.
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
foreign import ccall unsafe
  sqlite3_error_offset ::
    -- | Connection.
    Ptr Sqlite3 ->
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
foreign import ccall unsafe
  sqlite3_extended_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

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
-- Finalize a statement.
foreign import ccall unsafe
  sqlite3_finalize ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/free.html
foreign import ccall unsafe
  sqlite3_free :: Ptr a -> IO ()

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Release memory acquired by 'sqlite3_create_filename'.
foreign import ccall unsafe
  sqlite3_free_filename ::
    -- | Filename.
    CString ->
    IO ()

-- | https://www.sqlite.org/c3ref/get_autocommit.html
--
-- Get whether a connection is in autocommit mode.
foreign import ccall unsafe
  sqlite3_get_autocommit ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | 0 or 1.
    IO CInt

-- | https://www.sqlite.org/c3ref/get_auxdata.html
foreign import ccall unsafe
  sqlite3_get_auxdata ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CInt ->
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
-- Cause all in-progress operations to abort and return `SQLITE_INTERRUPT` at the earliest opportunity.
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
    -- | 0 or 1.
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
-- Get the rowid of the most recent insert into a table with a rowid.
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
    -- | Error code.
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
    -- | Reset highest value? (0 or 1).
    CInt ->
    -- | Highest value (prior to reset, if reset).
    IO Int64

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the size of live memory, in bytes.
foreign import ccall unsafe
  sqlite3_memory_used :: IO Int64

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

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Create a mutex.
foreign import ccall unsafe
  sqlite3_mutex_alloc ::
    -- | Mutex type.
    CInt ->
    -- | Mutex.
    IO (Ptr Sqlite3_mutex)

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Acquire a mutex (blocking).
foreign import ccall safe
  sqlite3_mutex_enter ::
    -- | Mutex.
    Ptr Sqlite3_mutex ->
    IO ()

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Destroy a mutex.
foreign import ccall unsafe
  sqlite3_mutex_free ::
    -- | Mutex.
    Ptr Sqlite3_mutex ->
    IO ()

sqlite3_mutex_held = undefined

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Release a mutex. The mutex must have been acquired on the same OS thread.
foreign import ccall unsafe
  sqlite3_mutex_leave ::
    -- | Mutex.
    Ptr Sqlite3_mutex ->
    IO ()

sqlite3_mutex_notheld = undefined

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Acquire a mutex (non-blocking).
foreign import ccall unsafe
  sqlite3_mutex_try ::
    -- | Mutex.
    Ptr Sqlite3_mutex ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/next_stmt.html
--
-- Get the first or next statement of a connection.
foreign import ccall unsafe
  sqlite3_next_stmt ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Statement, or null to get the first statement.
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
  -- safe because: sqlite3_auto_extension
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
  -- safe because: sqlite3_auto_extension
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
foreign import ccall unsafe
  sqlite3_overload_function :: Ptr Sqlite3 -> CString -> CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/prepare.html
foreign import ccall safe
  -- safe because: sqlite3_collation_needed
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
foreign import ccall unsafe
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
foreign import ccall unsafe
  sqlite3_preupdate_blobwrite :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import ccall unsafe
  sqlite3_preupdate_count :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import ccall unsafe
  sqlite3_preupdate_depth :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import ccall unsafe
  sqlite3_preupdate_hook ::
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> Ptr Sqlite3 -> CInt -> CString -> CString -> Int64 -> Int64 -> IO ()) ->
    Ptr a ->
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import ccall unsafe
  sqlite3_preupdate_new ::
    Ptr Sqlite3 ->
    CInt ->
    Ptr (Ptr Sqlite3_value) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import ccall unsafe
  sqlite3_preupdate_old ::
    Ptr Sqlite3 ->
    CInt ->
    Ptr (Ptr Sqlite3_value) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/progress_handler.html
foreign import ccall unsafe
  sqlite3_progress_handler ::
    Ptr Sqlite3 ->
    CInt ->
    FunPtr (Ptr a -> IO CInt) ->
    Ptr a ->
    IO ()

-- | https://www.sqlite.org/c3ref/randomness.html
foreign import ccall unsafe
  sqlite3_randomness :: CInt -> Ptr a -> IO ()

-- | https://www.sqlite.org/c3ref/free.html
foreign import ccall unsafe
  sqlite3_realloc :: Ptr a -> CInt -> IO (Ptr a)

-- | https://www.sqlite.org/c3ref/free.html
foreign import ccall unsafe
  sqlite3_realloc64 :: Ptr a -> Int64 -> IO (Ptr a)

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
foreign import ccall unsafe
  sqlite3_reset ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/reset_auto_extension.html
sqlite3_reset_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_blob ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr a ->
    CInt ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_blob64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr a ->
    Word64 ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_double ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CDouble ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CString ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_code ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_nomem ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_toobig ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_int ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_int64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Int64 ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_null ::
    -- | Function context.
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_pointer ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr a ->
    CString ->
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
foreign import ccall unsafe
  sqlite3_result_text ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr CChar ->
    CInt ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_text64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr CChar ->
    Word64 ->
    FunPtr (Ptr a -> IO ()) ->
    CUChar ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_value ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Ptr Sqlite3_value ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_zeroblob ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_zeroblob64 ::
    -- | Function context.
    Ptr Sqlite3_context ->
    Word64 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/commit_hook.html
foreign import ccall unsafe
  sqlite3_rollback_hook ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Rollback hook.
    FunPtr (Ptr a -> IO CInt) ->
    Ptr a ->
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/serialize.html
foreign import ccall unsafe
  sqlite3_serialize ::
    Ptr Sqlite3 ->
    CString ->
    Ptr Int64 ->
    CUInt ->
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/set_authorizer.html
foreign import ccall unsafe
  sqlite3_set_authorizer ::
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CInt -> CString -> CString -> CString -> CString -> IO CInt) ->
    Ptr a ->
    IO CInt

-- | https://www.sqlite.org/c3ref/get_auxdata.html
foreign import ccall unsafe
  sqlite3_set_auxdata ::
    -- | Function context.
    Ptr Sqlite3_context ->
    CInt ->
    Ptr a ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/set_last_insert_rowid.html
foreign import ccall unsafe
  sqlite3_set_last_insert_rowid :: Ptr Sqlite3 -> Int64 -> IO ()

-- | https://www.sqlite.org/c3ref/initialize.html
foreign import ccall unsafe
  sqlite3_shutdown :: IO CInt

-- | https://www.sqlite.org/c3ref/sleep.html
foreign import ccall safe
  sqlite3_sleep :: CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_cmp.html
foreign import ccall unsafe
  sqlite3_snapshot_cmp ::
    Ptr Sqlite3_snapshot ->
    Ptr Sqlite3_snapshot ->
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_free.html
foreign import ccall unsafe
  sqlite3_snapshot_free :: Ptr Sqlite3_snapshot -> IO ()

-- | https://www.sqlite.org/c3ref/snapshot_get.html
foreign import ccall unsafe
  sqlite3_snapshot_get ::
    Ptr Sqlite3 ->
    CString ->
    Ptr (Ptr Sqlite3_snapshot) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_open.html
foreign import ccall unsafe
  sqlite3_snapshot_open ::
    Ptr Sqlite3 ->
    CString ->
    Ptr Sqlite3_snapshot ->
    IO CInt

-- | https://www.sqlite.org/c3ref/snapshot_recover.html
foreign import ccall unsafe
  sqlite3_snapshot_recover ::
    Ptr Sqlite3 ->
    CString ->
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
foreign import ccall unsafe
  sqlite3_status ::
    CInt ->
    Ptr CInt ->
    Ptr CInt ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/status.html
foreign import ccall unsafe
  sqlite3_status64 ::
    CInt ->
    Ptr Int64 ->
    Ptr Int64 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
--
-- Produce the next row of a statement.
foreign import ccall safe
  -- safe because: sqlite3_autovacuum_pages, sqlite3_create_function
  sqlite3_step ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_busy.html
foreign import ccall unsafe
  sqlite3_stmt_busy ::
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_isexplain.html
foreign import ccall unsafe
  sqlite3_stmt_isexplain ::
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_readonly.html
foreign import ccall unsafe
  sqlite3_stmt_readonly ::
    Ptr Sqlite3_stmt ->
    IO CInt

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus.html
-- foreign import ccall unsafe
--   sqlite3_stmt_scanstatus ::
--     Ptr Sqlite3_stmt ->
--     CInt ->
--     CInt ->
--     Ptr a ->
--     IO CInt

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus_reset.html
-- foreign import ccall unsafe
--   sqlite3_stmt_scanstatus_reset ::
--     Ptr Sqlite3_stmt ->
--     IO ()

-- | https://www.sqlite.org/c3ref/stmt_status.html
foreign import ccall unsafe
  sqlite3_stmt_status ::
    Ptr Sqlite3_stmt ->
    CInt ->
    CInt ->
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
    -- | 0 if matches.
    CInt

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding).
foreign import ccall unsafe
  sqlite3_stricmp ::
    CString ->
    CString ->
    IO CInt

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
    -- | 0 if matches.
    CInt

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding), up to a certain length.
foreign import ccall unsafe
  sqlite3_strnicmp ::
    CString ->
    CString ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/system_errno.html
foreign import ccall unsafe
  sqlite3_system_errno :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/table_column_metadata.html
foreign import ccall unsafe
  sqlite3_table_column_metadata ::
    Ptr Sqlite3 ->
    CString ->
    CString ->
    CString ->
    Ptr CString ->
    Ptr CString ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/threadsafe.html
foreign import ccall unsafe
  sqlite3_threadsafe :: CInt

-- | https://www.sqlite.org/c3ref/total_changes.html
foreign import ccall unsafe
  sqlite3_total_changes :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/total_changes.html
foreign import ccall unsafe
  sqlite3_total_changes64 :: Ptr Sqlite3 -> IO Int64

-- | https://www.sqlite.org/c3ref/trace_v2.html
foreign import ccall unsafe
  sqlite3_trace_v2 ::
    Ptr Sqlite3 ->
    CUInt ->
    FunPtr (CUInt -> Ptr a -> Ptr b -> Ptr c -> IO CInt) ->
    Ptr a ->
    IO CInt

-- | https://www.sqlite.org/c3ref/txn_state.html
foreign import ccall unsafe
  sqlite3_txn_state ::
    Ptr Sqlite3 ->
    CString ->
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
    -- | Query parameter value (0 or 1).
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
    -- | Value copy.
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
    -- | 0 or 1.
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
    -- 0 or 1.
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
    -- | String (UTF-8)
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
    -- | Make default? (0 or 1).
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

foreign import capi unsafe "sqlite3.h sqlite3_vtab_config" sqlite3_vtab_config__1 :: Ptr Sqlite3 -> CInt -> CInt -> IO CInt

foreign import capi unsafe "sqlite3.h sqlite3_vtab_config" sqlite3_vtab_config__2 :: Ptr Sqlite3 -> CInt -> IO CInt

-- | https://www.sqlite.org/c3ref/vtab_distinct.html
--
-- Get information about how the query planner wants output to be ordered.
foreign import ccall unsafe
  sqlite3_vtab_distinct ::
    -- | Index info (first argument to @xBestIndex@).
    Ptr Sqlite3_index_info ->
    -- | 0, 1, 2, or 3.
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
    -- | -1, 0, or 1.
    CInt ->
    -- | 0 or 1.
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
    -- | 0 or 1.
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
