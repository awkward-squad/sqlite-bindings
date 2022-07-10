module Sqlite.Bindings.Internal.Functions where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CDouble (..), CInt (..), CUChar (..), CUInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import Sqlite.Bindings.Internal.Objects

-- TODO look over all FunPtr and decide which functions need safe variants
--
-- TODO call out all "optional" params (whether caller can provide null)

-- | https://www.sqlite.org/c3ref/aggregate_context.html
foreign import ccall unsafe
  sqlite3_aggregate_context ::
    -- | Context.
    Ptr Sqlite3_context ->
    -- | Number of bytes.
    CInt ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/auto_extension.html
sqlite3_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/autovacuum_pages.html
foreign import ccall unsafe
  sqlite3_autovacuum_pages ::
    -- | Connection.
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt) ->
    Ptr a ->
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import ccall unsafe
  sqlite3_backup_finish ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import ccall unsafe
  sqlite3_backup_init ::
    -- | Destination connection.
    Ptr Sqlite3 ->
    -- | Destination database name.
    CString ->
    -- | Source connection.
    Ptr Sqlite3 ->
    -- | Source database name.
    CString ->
    IO (Ptr Sqlite3_backup)

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import ccall unsafe
  sqlite3_backup_pagecount ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import ccall unsafe
  sqlite3_backup_remaining ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import ccall safe
  sqlite3_backup_step ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Number of pages to copy.
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a BLOB to a parameter.
foreign import ccall unsafe
  sqlite3_bind_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Length of blob in bytes.
    CInt ->
    -- | Optional destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
    FunPtr (Ptr a -> IO ()) ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a BLOB to a parameter.
foreign import ccall unsafe
  sqlite3_bind_blob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Length of blob in bytes.
    Int64 ->
    -- | Optional destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
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
-- Get the index of the largest parameter in a statement.
--
-- This is normally the number of unique parameters in a statement. For example, the following statement has two
-- parameters, and @sqlite3_bind_parameter_count@ would return 2:
--
-- @
-- select "foo"
-- from "bar"
-- where "baz" = ? and "qux" = ?
-- @
--
-- However, a statement may include manually-numbered parameters. For example, on this statement,
-- @sqlite3_bind_parameter_count@ would return 6:
--
-- @
-- select "foo"
-- from "bar"
-- where "baz" = ?2 and "qux" = ?6
-- @
foreign import ccall unsafe
  sqlite3_bind_parameter_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based; 0 means no parameters).
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_parameter_index.html
foreign import ccall unsafe
  sqlite3_bind_parameter_index ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter name.
    CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_parameter_name.html
foreign import ccall unsafe
  sqlite3_bind_parameter_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/bind_blob.html
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
    -- | Optional destructor.
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
    -- | Length of string in bytes.
    CInt ->
    -- | Optional destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
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
    -- | Length of string in bytes.
    Int64 ->
    -- | Optional destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
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
-- Bind a BLOB of zeroes to a parameter.
foreign import ccall unsafe
  sqlite3_bind_zeroblob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Length of blob in bytes.
    CInt ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a BLOB of zeroes to a parameter.
foreign import ccall unsafe
  sqlite3_bind_zeroblob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Length of blob in bytes.
    Int64 ->
    -- | Result code.
    IO CInt

-- | [__Return the size of an open BLOB__](https://www.sqlite.org/c3ref/blob_bytes.html)
foreign import ccall unsafe
  sqlite3_blob_bytes ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    IO CInt

-- | [__Close a BLOB handle__](https://www.sqlite.org/c3ref/blob_close.html)
foreign import ccall unsafe
  sqlite3_blob_close ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    IO CInt

-- | [__Open a BLOB for incremental I/O__](https://www.sqlite.org/c3ref/blob_open.html)
foreign import ccall unsafe
  sqlite3_blob_open ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name.
    CString ->
    -- | Table name.
    CString ->
    -- | Column name
    CString ->
    -- | Row id.
    Int64 ->
    -- | Flags.
    CInt ->
    -- | /Out/: blob.
    Ptr (Ptr Sqlite3_blob) ->
    IO CInt

-- | [__Read data from a BLOB incrementally__](https://www.sqlite.org/c3ref/blob_read.html)
foreign import ccall unsafe
  sqlite3_blob_read ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Buffer to read into.
    Ptr a ->
    -- | Length of buffer to read into.
    CInt ->
    -- | Byte offset into blob to read from.
    CInt ->
    IO CInt

-- | [__Move a BLOB handle to a new row__](https://www.sqlite.org/c3ref/blob_reopen.html)
foreign import ccall unsafe
  sqlite3_blob_reopen ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Rowid.
    Int64 ->
    IO CInt

-- | [__Write data into a BLOB incrementally__](https://www.sqlite.org/c3ref/blob_write.html)
foreign import ccall unsafe
  sqlite3_blob_write ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    -- | Buffer of data to write.
    Ptr a ->
    -- | Length of buffer to write.
    CInt ->
    -- | Byte offset into blob to write to.
    CInt ->
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
foreign import ccall unsafe
  sqlite3_clear_bindings ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
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
-- Close a database connection. If it has any unfinalized statements, open BLOB handlers, or unfinished backups, mark
-- the connection as unusable and make arrangements to deallocate it after all statements are finalized, BLOB handlers
-- are closed, and backups are finished.
foreign import ccall unsafe
  sqlite3_close_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | [__Collation needed callbacks__](https://www.sqlite.org/c3ref/collation_needed.html)
foreign import ccall unsafe
  sqlite3_collation_needed ::
    Ptr Sqlite3 ->
    Ptr a ->
    FunPtr (Ptr a -> Ptr Sqlite3 -> CInt -> CString -> IO ()) ->
    IO CInt

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr a)

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_bytes ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | [__Number of columns in a result set__](https://www.sqlite.org/c3ref/column_count.html)
foreign import ccall unsafe
  sqlite3_column_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import ccall unsafe
  sqlite3_column_database_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | [__Declared datatype of a query result__](https://www.sqlite.org/c3ref/column_decltype.html)
foreign import ccall unsafe
  sqlite3_column_decltype ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CDouble

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_int64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
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

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr CUChar)

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_type ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | [__Result values from a query__](https://www.sqlite.org/c3ref/column_blob.html)
foreign import ccall unsafe
  sqlite3_column_value ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
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

-- | [__Runtime library compilation options diagnostics__](https://www.sqlite.org/c3ref/compileoption_get.html)
foreign import ccall unsafe
  sqlite3_compileoption_get ::
    -- | Option index.
    CInt ->
    CString

-- | [__Runtime library compilation options diagnostics__](https://www.sqlite.org/c3ref/compileoption_get.html)
foreign import ccall unsafe
  sqlite3_compileoption_used ::
    -- | Option name.
    CString ->
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

-- | [__Database connection for functions__](https://www.sqlite.org/c3ref/context_db_handle.html)
foreign import ccall unsafe
  sqlite3_context_db_handle ::
    Ptr Sqlite3_context ->
    IO (Ptr Sqlite3)

sqlite3_create_collation = undefined

sqlite3_create_collation16 = undefined

sqlite3_create_collation_v2 = undefined

sqlite3_create_filename = undefined

-- | [__Create or redefine SQL functions__](https://www.sqlite.org/c3ref/create_function.html)
foreign import ccall unsafe
  sqlite3_create_function ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name.
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Flags.
    CInt ->
    -- | User data.
    Ptr a ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    IO CInt

-- | [__Create or redefine SQL functions__](https://www.sqlite.org/c3ref/create_function.html)
foreign import ccall unsafe
  sqlite3_create_function_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name.
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Flags.
    CInt ->
    -- | User data.
    Ptr a ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    -- | User data destructor.
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

sqlite3_create_module = undefined

sqlite3_create_module_v2 = undefined

-- | [__Create or redefine SQL functions__](https://www.sqlite.org/c3ref/create_function.html)
foreign import ccall unsafe
  sqlite3_create_window_function ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Function name.
    CString ->
    -- | Number of function arguments.
    CInt ->
    -- | Flags.
    CInt ->
    -- | User data.
    Ptr a ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> IO ()) ->
    FunPtr (Ptr Sqlite3_context -> CInt -> Ptr (Ptr Sqlite3_value) -> IO ()) ->
    -- | User data destructor.
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

-- | [__Number of columns in a result set__](https://www.sqlite.org/c3ref/data_count.html)
foreign import ccall unsafe
  sqlite3_data_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

sqlite3_database_file_object = undefined

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
foreign import ccall safe "sqlite3_db_cacheflush"
  sqlite3_db_cacheflush__safe ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
foreign import ccall unsafe "sqlite3_db_cacheflush"
  sqlite3_db_cacheflush__unsafe ::
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
    -- | Database name.
    CString ->
    -- | Filename.
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
    -- | Database name.
    IO CString

-- | https://www.sqlite.org/c3ref/db_readonly.html
--
-- Get whether an attached database is read-only.
foreign import ccall unsafe
  sqlite3_db_readonly ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name.
    CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/db_release_memory.html
--
-- Free as much memory as possible from a connection.
foreign import ccall unsafe
  sqlite3_db_release_memory ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Result code.
    IO CInt

-- | https://www.sqlite.org/c3ref/db_status.html
--
-- Get a status of a connection.
--
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | Status option                          | Meaning                                                                                                                              |
-- +========================================+======================================================================================================================================+
-- | @SQLITE_DBSTATUS_CACHE_HIT@₁           | Pager cache hits.                                                                                                                    |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_CACHE_MISS@₁          | Pager cache misses.                                                                                                                  |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_CACHE_SPILL@          | Dirty cache entries written to disk in the middle of a transaction due to the page cache overflowing.                                |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_CACHE_USED@₁          | Approximate bytes of heap memory used by all pager caches.                                                                           |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_CACHE_USED_SHARED@₁   | @DBSTATUS_CACHE_USED@, but evenly divide bytes of shared pager caches among all associated connections.                              |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_CACHE_WRITE@₁         | Dirty cache entries written to disk.                                                                                                 |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_DEFERRED_FKS@₁        | Zero if and only if all foreign key constraints (deferred or immediate) have been resolved.                                          |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_LOOKASIDE_HIT@₂       | Malloc attempts satisfied using lookaside memory.                                                                                    |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL@₂ | Malloc attempts not satisfied using lookaside memory because all lookaside memory was in use.                                        |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE@₂ | Malloc attempts not satisfied using lookaside memory because the amount of memory requested was larger than the lookaside slot size. |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_LOOKASIDE_USED@       | Lookaside memory slots currently checked out.                                                                                        |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_SCHEMA_USED@₁         | Approximate bytes of heap memory used to store the schema for all attached databases.                                                |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
-- | @SQLITE_DBSTATUS_STMT_USED@₁           | Approximate bytes of heap and lookaside memory used by all statements                                                                |
-- +----------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------+
--
-- /₁ only has current value/
--
-- /₂ only has highest value/
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

sqlite3_declare_vtab = undefined

-- | [__Deserialize a database__](https://www.sqlite.org/c3ref/deserialize.html)
foreign import ccall unsafe
  sqlite3_deserialize ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | Database name.
    CString ->
    -- | Serialized database.
    Ptr CUChar ->
    -- | Number of bytes in the serialized database.
    Int64 ->
    -- | Number of bytes in the serialized database buffer. If this is larger than the previous argument, and
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
    -- | Names of virtual table modules to keep.
    Ptr CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
foreign import ccall unsafe
  sqlite3_extended_result_codes ::
    -- | Connection.
    Ptr Sqlite3 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import ccall unsafe
  sqlite3_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import ccall unsafe
  sqlite3_errmsg ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CString

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import ccall unsafe
  sqlite3_error_offset ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import ccall unsafe
  sqlite3_errstr :: CInt -> CString

-- | [__Retrieving statement SQL__](https://www.sqlite.org/c3ref/expanded_sql.html)
foreign import ccall unsafe
  sqlite3_expanded_sql ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CString

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import ccall unsafe
  sqlite3_extended_errcode ::
    -- | Connection.
    Ptr Sqlite3 ->
    IO CInt

sqlite3_file_control = undefined

sqlite3_filename_database = undefined

sqlite3_filename_journal = undefined

sqlite3_filename_wal = undefined

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
  sqlite3_free_filename :: CString -> IO ()

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
    Ptr Sqlite3_context -> CInt -> IO (Ptr a)

sqlite3_hard_heap_limit64 = undefined

sqlite3_initialize = undefined

sqlite3_interrupt = undefined

sqlite3_keyword_check = undefined

sqlite3_keyword_count = undefined

sqlite3_keyword_name = undefined

sqlite3_last_insert_rowid = undefined

sqlite3_libversion = undefined

sqlite3_libversion_number = undefined

sqlite3_limit = undefined

sqlite3_load_extension = undefined

sqlite3_log = undefined

sqlite3_malloc = undefined

sqlite3_malloc64 = undefined

sqlite3_memory_highwater = undefined

sqlite3_memory_used = undefined

sqlite3_mprintf = undefined

sqlite3_msize = undefined

sqlite3_mutex_alloc = undefined

sqlite3_mutex_enter = undefined

sqlite3_mutex_free = undefined

sqlite3_mutex_held = undefined

sqlite3_mutex_leave = undefined

sqlite3_mutex_notheld = undefined

sqlite3_mutex_try = undefined

sqlite3_next_stmt = undefined

sqlite3_normalized_sql = undefined

-- | https://www.sqlite.org/c3ref/open.html
--
-- Open a new database connection.
foreign import ccall unsafe
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
foreign import ccall unsafe
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
foreign import ccall unsafe
  sqlite3_prepare_v2 ::
    -- | Connection.
    Ptr Sqlite3 ->
    -- | SQL (UTF-8).
    Ptr CChar ->
    -- | Length of SQL in bytes.
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
    -- | Length of SQL in bytes.
    CInt ->
    -- | Flags.
    CUInt ->
    -- | /Out/: statement.
    Ptr (Ptr Sqlite3_stmt) ->
    -- | /Out/: unused SQL.
    Ptr (Ptr CChar) ->
    -- | Result code.
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
    -- | Number of bytes to free.
    CInt ->
    -- | Number of bytes actually freed (may be more or less than the requested amount).
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

-- | [__Reset automatic extension loading__](https://www.sqlite.org/c3ref/reset_auto_extension.html)
sqlite3_reset_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_blob ::
    Ptr Sqlite3_context ->
    Ptr a ->
    CInt ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_blob64 ::
    Ptr Sqlite3_context ->
    Ptr a ->
    Int64 ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_double ::
    Ptr Sqlite3_context ->
    CDouble ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error ::
    Ptr Sqlite3_context ->
    CString ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_code ::
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_nomem ::
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_error_toobig ::
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_int ::
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_int64 ::
    Ptr Sqlite3_context ->
    Int64 ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_null ::
    Ptr Sqlite3_context ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_pointer ::
    Ptr Sqlite3_context ->
    Ptr a ->
    CString ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_subtype.html
foreign import ccall unsafe
  sqlite3_result_subtype ::
    Ptr Sqlite3_context ->
    CUInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_text ::
    Ptr Sqlite3_context ->
    Ptr CChar ->
    CInt ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_text64 ::
    Ptr Sqlite3_context ->
    Ptr CChar ->
    Int64 ->
    FunPtr (Ptr a -> IO ()) ->
    CUChar ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_value ::
    Ptr Sqlite3_context ->
    Ptr Sqlite3_value ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_zeroblob ::
    Ptr Sqlite3_context ->
    CInt ->
    IO ()

-- | https://www.sqlite.org/c3ref/result_blob.html
foreign import ccall unsafe
  sqlite3_result_zeroblob64 ::
    Ptr Sqlite3_context ->
    Int64 ->
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
  sqlite3_set_auxdata :: Ptr Sqlite3_context -> CInt -> Ptr a -> FunPtr (Ptr a -> IO ()) -> IO ()

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

sqlite3_snprintf = undefined

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
foreign import ccall unsafe
  sqlite3_soft_heap_limit64 :: Int64 -> IO Int64

-- | https://www.sqlite.org/c3ref/libversion.html
foreign import ccall unsafe
  sqlite3_sourceid :: CString

-- | https://www.sqlite.org/c3ref/expanded_sql.html
foreign import ccall unsafe
  sqlite3_sql :: Ptr Sqlite3_stmt -> IO CString

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
foreign import ccall safe
  sqlite3_step ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
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

-- | https://www.sqlite.org/c3ref/stmt_scanstatus.html
foreign import ccall unsafe
  sqlite3_stmt_scanstatus ::
    Ptr Sqlite3_stmt ->
    CInt ->
    CInt ->
    Ptr a ->
    IO CInt

-- | https://www.sqlite.org/c3ref/stmt_scanstatus_reset.html
foreign import ccall unsafe
  sqlite3_stmt_scanstatus_reset ::
    Ptr Sqlite3_stmt ->
    IO ()

-- | https://www.sqlite.org/c3ref/stmt_status.html
foreign import ccall unsafe
  sqlite3_stmt_status ::
    Ptr Sqlite3_stmt ->
    CInt ->
    CInt ->
    IO CInt

sqlite3_str_append = undefined

sqlite3_str_appendall = undefined

sqlite3_str_appendchar = undefined

sqlite3_str_appendf = undefined

sqlite3_str_errcode = undefined

sqlite3_str_finish = undefined

sqlite3_str_length = undefined

sqlite3_str_new = undefined

sqlite3_str_reset = undefined

sqlite3_str_value = undefined

sqlite3_str_vappendf = undefined

sqlite3_strglob = undefined

sqlite3_stricmp = undefined

sqlite3_strlike = undefined

sqlite3_strnicmp = undefined

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

sqlite3_uri_boolean = undefined

sqlite3_uri_int64 = undefined

sqlite3_uri_key = undefined

sqlite3_uri_parameter = undefined

-- | https://www.sqlite.org/c3ref/user_data.html
foreign import ccall unsafe
  sqlite3_user_data ::
    Ptr Sqlite3_context ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_blob ::
    Ptr Sqlite3_value ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_bytes ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_double ::
    Ptr Sqlite3_value ->
    IO CDouble

sqlite3_value_dup = undefined

sqlite3_value_free = undefined

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_frombind ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_int ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_int64 ::
    Ptr Sqlite3_value ->
    IO Int64

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_nochange ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_numeric_type ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_pointer ::
    Ptr Sqlite3_value ->
    CString ->
    IO (Ptr a)

sqlite3_value_subtype = undefined

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_text ::
    Ptr Sqlite3_value ->
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/value_blob.html
foreign import ccall unsafe
  sqlite3_value_type ::
    Ptr Sqlite3_value ->
    IO CInt

-- | https://www.sqlite.org/c3ref/libversion.html
foreign import capi unsafe "sqlite3.h value sqlite3_version"
  sqlite3_version :: CString

sqlite3_vfs_find = undefined

sqlite3_vfs_register = undefined

sqlite3_vfs_unregister = undefined

sqlite3_vmprintf = undefined

sqlite3_vsnprintf = undefined

sqlite3_vtab_collation = undefined

sqlite3_vtab_config = undefined

sqlite3_vtab_distinct = undefined

sqlite3_vtab_in = undefined

sqlite3_vtab_in_first = undefined

sqlite3_vtab_in_next = undefined

sqlite3_vtab_nochange = undefined

sqlite3_vtab_on_conflict = undefined

sqlite3_vtab_rhs_value = undefined

-- | https://www.sqlite.org/c3ref/wal_autocheckpoint.html
foreign import ccall unsafe
  sqlite3_wal_autocheckpoint ::
    Ptr Sqlite3 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_checkpoint.html
foreign import ccall unsafe
  sqlite3_wal_checkpoint ::
    Ptr Sqlite3 ->
    CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_checkpoint_v2.html
foreign import ccall unsafe
  sqlite3_wal_checkpoint_v2 ::
    Ptr Sqlite3 ->
    CString ->
    CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/wal_hook.html
foreign import ccall unsafe
  sqlite3_wal_hook ::
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> Ptr Sqlite3 -> CString -> CInt -> IO CInt) ->
    Ptr a ->
    IO (Ptr b)
