module Sqlite3.Bindings.Internal.Functions where

import Data.Bits ((.|.))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Foreign (FunPtr, Ptr)
import Foreign.C (CChar (..), CDouble (..), CInt (..), CString, CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (peek))
import qualified Sqlite.Bindings
import Sqlite3.Bindings.Internal.Constants
import Sqlite3.Bindings.Internal.Objects
import Sqlite3.Bindings.Internal.Utils (cstringLenToText, textToCString, textToCStringLen)

sqlite3_aggregate_context = undefined

sqlite3_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/autovacuum_pages.html
--
-- Register a callback that is invoked prior to each autovacuum.
sqlite3_autovacuum_pages ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Callback.
  FunPtr (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt) ->
  -- | Application data.
  Ptr a ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_autovacuum_pages =
  Sqlite.Bindings.sqlite3_autovacuum_pages

-- | https://www.sqlite.org/c3ref/backup_finish.html
sqlite3_backup_finish ::
  -- | Backup.
  Ptr Sqlite.Bindings.Sqlite3_backup ->
  IO CInt
sqlite3_backup_finish =
  Sqlite.Bindings.sqlite3_backup_finish

-- | https://www.sqlite.org/c3ref/backup_finish.html
sqlite3_backup_init ::
  -- | Destination connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Destination database name (UTF-8).
  CString ->
  -- | Source connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Source database name (UTF-8).
  CString ->
  IO (Ptr Sqlite.Bindings.Sqlite3_backup)
sqlite3_backup_init =
  Sqlite.Bindings.sqlite3_backup_init

-- | https://www.sqlite.org/c3ref/backup_finish.html
sqlite3_backup_pagecount ::
  -- | Backup.
  Ptr Sqlite.Bindings.Sqlite3_backup ->
  IO CInt
sqlite3_backup_pagecount =
  Sqlite.Bindings.sqlite3_backup_pagecount

-- | https://www.sqlite.org/c3ref/backup_finish.html
sqlite3_backup_remaining ::
  -- | Backup.
  Ptr Sqlite.Bindings.Sqlite3_backup ->
  IO CInt
sqlite3_backup_remaining =
  Sqlite.Bindings.sqlite3_backup_remaining

-- | https://www.sqlite.org/c3ref/backup_finish.html
sqlite3_backup_step ::
  -- | Backup.
  Ptr Sqlite.Bindings.Sqlite3_backup ->
  -- | Number of pages to copy.
  CInt ->
  IO CInt
sqlite3_backup_step =
  Sqlite.Bindings.sqlite3_backup_step

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
sqlite3_bind_blob ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
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
sqlite3_bind_blob =
  Sqlite.Bindings.sqlite3_bind_blob

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
sqlite3_bind_blob64 ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
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
sqlite3_bind_blob64 =
  Sqlite.Bindings.sqlite3_bind_blob64

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a double to a parameter.
sqlite3_bind_double ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Double.
  CDouble ->
  -- | Result code.
  IO CInt
sqlite3_bind_double =
  Sqlite.Bindings.sqlite3_bind_double

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
sqlite3_bind_int ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Integer.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_bind_int =
  Sqlite.Bindings.sqlite3_bind_int

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
sqlite3_bind_int64 ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Integer.
  Int64 ->
  -- | Result code.
  IO CInt
sqlite3_bind_int64 =
  Sqlite.Bindings.sqlite3_bind_int64

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter.
sqlite3_bind_null ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_bind_null =
  Sqlite.Bindings.sqlite3_bind_null

-- | https://www.sqlite.org/c3ref/bind_parameter_count.html
--
-- Get the index of the largest parameter.
sqlite3_bind_parameter_count ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based), or 0 (no parameters).
  IO CInt
sqlite3_bind_parameter_count =
  Sqlite.Bindings.sqlite3_bind_parameter_count

-- | https://www.sqlite.org/c3ref/bind_parameter_index.html
--
-- Get the index of a named parameter.
sqlite3_bind_parameter_index ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter name (UTF-8).
  CString ->
  -- | Parameter index (1-based), or 0 (not found).
  IO CInt
sqlite3_bind_parameter_index =
  Sqlite.Bindings.sqlite3_bind_parameter_index

-- | https://www.sqlite.org/c3ref/bind_parameter_name.html
--
-- Get the name of a named parameter.
sqlite3_bind_parameter_name ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Parameter name (UTF-8).
  IO CString
sqlite3_bind_parameter_name =
  Sqlite.Bindings.sqlite3_bind_parameter_name

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter, and associate it with a pointer.
sqlite3_bind_pointer ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
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
sqlite3_bind_pointer =
  Sqlite.Bindings.sqlite3_bind_pointer

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
sqlite3_bind_text ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
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
sqlite3_bind_text =
  Sqlite.Bindings.sqlite3_bind_text

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
sqlite3_bind_text64 ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
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
sqlite3_bind_text64 =
  Sqlite.Bindings.sqlite3_bind_text64

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a value to a parameter.
sqlite3_bind_value ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Result code.
  IO CInt
sqlite3_bind_value =
  Sqlite.Bindings.sqlite3_bind_value

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob of zeroes to a parameter.
sqlite3_bind_zeroblob ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Size of blob, in bytes.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_bind_zeroblob =
  Sqlite.Bindings.sqlite3_bind_zeroblob

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob of zeroes to a parameter.
sqlite3_bind_zeroblob64 ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Parameter index (1-based).
  CInt ->
  -- | Size of blob, in bytes.
  Word64 ->
  -- | Result code.
  IO CInt
sqlite3_bind_zeroblob64 =
  Sqlite.Bindings.sqlite3_bind_zeroblob64

-- | https://www.sqlite.org/c3ref/blob_bytes.html
--
-- Get the size of a blob, in bytes.
sqlite3_blob_bytes ::
  -- | Blob.
  Ptr Sqlite.Bindings.Sqlite3_blob ->
  -- | Size of blob, in bytes.
  IO CInt
sqlite3_blob_bytes =
  Sqlite.Bindings.sqlite3_blob_bytes

-- | https://www.sqlite.org/c3ref/blob_close.html
--
-- Close a blob.
sqlite3_blob_close ::
  -- | Blob.
  Ptr Sqlite.Bindings.Sqlite3_blob ->
  -- | Result code.
  IO CInt
sqlite3_blob_close =
  Sqlite.Bindings.sqlite3_blob_close

-- | https://www.sqlite.org/c3ref/blob_open.html
--
-- Open a blob.
sqlite3_blob_open ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
  Ptr (Ptr Sqlite.Bindings.Sqlite3_blob) ->
  -- | Result code.
  IO CInt
sqlite3_blob_open =
  Sqlite.Bindings.sqlite3_blob_open

-- | https://www.sqlite.org/c3ref/blob_read.html
--
-- Read data from a blob.
sqlite3_blob_read ::
  -- | Blob.
  Ptr Sqlite.Bindings.Sqlite3_blob ->
  -- | Buffer to read into.
  Ptr a ->
  -- | Size of buffer to read into.
  CInt ->
  -- | Byte offset into blob to read from.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_blob_read =
  Sqlite.Bindings.sqlite3_blob_read

-- | https://www.sqlite.org/c3ref/blob_reopen.html
--
-- Point an open blob at a different row in the same table.
sqlite3_blob_reopen ::
  -- | Blob.
  Ptr Sqlite.Bindings.Sqlite3_blob ->
  -- | Rowid.
  Int64 ->
  -- | Result code.
  IO CInt
sqlite3_blob_reopen =
  Sqlite.Bindings.sqlite3_blob_reopen

-- | https://www.sqlite.org/c3ref/blob_write.html
--
-- Write data to a blob.
sqlite3_blob_write ::
  -- | Blob.
  Ptr Sqlite.Bindings.Sqlite3_blob ->
  -- | Buffer of data to write.
  Ptr a ->
  -- | Size of buffer to write.
  CInt ->
  -- | Byte offset into blob to write to.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_blob_write =
  Sqlite.Bindings.sqlite3_blob_write

-- | [__Register a callback to handle `SQLITE_BUSY` errors__](https://www.sqlite.org/c3ref/busy_handler.html)
sqlite3_busy_handler ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  FunPtr (Ptr a -> CInt -> IO CInt) ->
  Ptr a ->
  IO CInt
sqlite3_busy_handler =
  Sqlite.Bindings.sqlite3_busy_handler

-- | [__Set a busy timeout__](https://www.sqlite.org/c3ref/busy_timeout.html)
sqlite3_busy_timeout ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Number of millseconds.
  CInt ->
  IO CInt
sqlite3_busy_timeout =
  Sqlite.Bindings.sqlite3_busy_timeout

-- | [__Cancel automatic extension loading__](https://www.sqlite.org/c3ref/cancel_auto_extension.html)
sqlite3_cancel_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/changes.html
sqlite3_changes ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_changes =
  Sqlite.Bindings.sqlite3_changes

-- | https://www.sqlite.org/c3ref/changes.html
sqlite3_changes64 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO Int64
sqlite3_changes64 =
  Sqlite.Bindings.sqlite3_changes64

-- | https://www.sqlite.org/c3ref/clear_bindings.html
--
-- Clear parameter bindings.
sqlite3_clear_bindings ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_clear_bindings =
  Sqlite.Bindings.sqlite3_clear_bindings

-- | https://www.sqlite.org/c3ref/close.html
--
-- Close a database connection.
sqlite3_close ::
  -- | Connection.
  Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_close (Sqlite3 connection) =
  Sqlite.Bindings.sqlite3_close connection

-- | https://www.sqlite.org/c3ref/close.html
--
-- Close a database connection. If it has any unfinalized statements, open blob handlers, or unfinished backups, mark
-- the connection as unusable and make arrangements to deallocate it after all statements are finalized, blob handlers
-- are closed, and backups are finished.
sqlite3_close_v2 ::
  -- | Connection.
  Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_close_v2 (Sqlite3 connection) =
  Sqlite.Bindings.sqlite3_close_v2 connection

-- | https://www.sqlite.org/c3ref/collation_needed.html
--
-- Register a callback that is invoked when a collating sequence is needed.
sqlite3_collation_needed ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Application data.
  Ptr a ->
  -- | Callback.
  FunPtr (Ptr a -> Ptr Sqlite.Bindings.Sqlite3 -> CInt -> CString -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_collation_needed =
  Sqlite.Bindings.sqlite3_collation_needed

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the blob of a result column.
sqlite3_column_blob ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Blob.
  IO (Ptr a)
sqlite3_column_blob =
  Sqlite.Bindings.sqlite3_column_blob

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the size of a blob or string result column, in bytes.
sqlite3_column_bytes ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Size, in bytes.
  IO CInt
sqlite3_column_bytes =
  Sqlite.Bindings.sqlite3_column_bytes

-- | https://www.sqlite.org/c3ref/column_count.html
--
-- Get the number of columns in a result set.
sqlite3_column_count ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Number of columns.
  IO CInt
sqlite3_column_count =
  Sqlite.Bindings.sqlite3_column_count

-- | https://www.sqlite.org/c3ref/column_database_name.html
sqlite3_column_database_name ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  IO CString
sqlite3_column_database_name =
  Sqlite.Bindings.sqlite3_column_database_name

-- | https://www.sqlite.org/c3ref/column_decltype.html
--
-- Get the declared type of a result column.
sqlite3_column_decltype ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Type (UTF-8).
  IO CString
sqlite3_column_decltype =
  Sqlite.Bindings.sqlite3_column_decltype

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the double of a result column.
sqlite3_column_double ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Double.
  IO CDouble
sqlite3_column_double =
  Sqlite.Bindings.sqlite3_column_double

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
sqlite3_column_int ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Integer.
  IO CInt
sqlite3_column_int =
  Sqlite.Bindings.sqlite3_column_int

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
sqlite3_column_int64 ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Integer.
  IO Int64
sqlite3_column_int64 =
  Sqlite.Bindings.sqlite3_column_int64

-- | https://www.sqlite.org/c3ref/column_name.html
sqlite3_column_name ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  IO CString
sqlite3_column_name =
  Sqlite.Bindings.sqlite3_column_name

-- | https://www.sqlite.org/c3ref/column_database_name.html
sqlite3_column_origin_name ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  IO CString
sqlite3_column_origin_name =
  Sqlite.Bindings.sqlite3_column_origin_name

-- | https://www.sqlite.org/c3ref/column_database_name.html
sqlite3_column_table_name ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  IO CString
sqlite3_column_table_name =
  Sqlite.Bindings.sqlite3_column_table_name

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the string of a result column.
sqlite3_column_text ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | String (UTF-8).
  IO (Ptr CUChar)
sqlite3_column_text =
  Sqlite.Bindings.sqlite3_column_text

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the type of a result column.
sqlite3_column_type ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Type.
  IO CInt
sqlite3_column_type =
  Sqlite.Bindings.sqlite3_column_type

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the value of a result column.
sqlite3_column_value ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Column index (0-based).
  CInt ->
  -- | Value.
  IO (Ptr Sqlite.Bindings.Sqlite3_value)
sqlite3_column_value =
  Sqlite.Bindings.sqlite3_column_value

-- | https://www.sqlite.org/c3ref/commit_hook.html
sqlite3_commit_hook ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Commit hook.
  FunPtr (Ptr a -> IO CInt) ->
  Ptr a ->
  IO (Ptr b)
sqlite3_commit_hook =
  Sqlite.Bindings.sqlite3_commit_hook

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get a compile-time option name.
sqlite3_compileoption_get ::
  -- | Option index.
  CInt ->
  -- | Option name (UTF-8).
  CString
sqlite3_compileoption_get =
  Sqlite.Bindings.sqlite3_compileoption_get

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get whether an option was specified at compile-time.
sqlite3_compileoption_used ::
  -- | Option name (UTF-8).
  CString ->
  -- | 0 or 1.
  CInt
sqlite3_compileoption_used =
  Sqlite.Bindings.sqlite3_compileoption_used

-- | https://www.sqlite.org/c3ref/complete.html
sqlite3_complete ::
  -- | SQL (UTF-8).
  CString ->
  CInt
sqlite3_complete =
  Sqlite.Bindings.sqlite3_complete

sqlite3_config__1 :: CInt -> IO CInt
sqlite3_config__1 =
  Sqlite.Bindings.sqlite3_config__1

sqlite3_config__2 :: CInt -> Ptr Sqlite.Bindings.Sqlite3_mem_methods -> IO CInt
sqlite3_config__2 =
  Sqlite.Bindings.sqlite3_config__2

sqlite3_config__3 :: CInt -> Ptr a -> CInt -> CInt -> IO CInt
sqlite3_config__3 =
  Sqlite.Bindings.sqlite3_config__3

sqlite3_config__4 :: CInt -> CInt -> IO CInt
sqlite3_config__4 =
  Sqlite.Bindings.sqlite3_config__4

sqlite3_config__5 :: CInt -> Ptr Sqlite.Bindings.Sqlite3_mutex_methods -> IO CInt
sqlite3_config__5 =
  Sqlite.Bindings.sqlite3_config__5

sqlite3_config__6 :: CInt -> CInt -> CInt -> IO CInt
sqlite3_config__6 =
  Sqlite.Bindings.sqlite3_config__6

sqlite3_config__7 :: CInt -> FunPtr (Ptr a -> CInt -> CString -> IO ()) -> Ptr a -> IO CInt
sqlite3_config__7 =
  Sqlite.Bindings.sqlite3_config__7

sqlite3_config__8 :: CInt -> Ptr Sqlite.Bindings.Sqlite3_pcache_methods2 -> IO CInt
sqlite3_config__8 =
  Sqlite.Bindings.sqlite3_config__8

sqlite3_config__9 :: CInt -> FunPtr (Ptr a -> Ptr Sqlite.Bindings.Sqlite3 -> CString -> CInt -> IO ()) -> Ptr a -> IO CInt
sqlite3_config__9 =
  Sqlite.Bindings.sqlite3_config__9

sqlite3_config__10 :: CInt -> Int64 -> Int64 -> IO CInt
sqlite3_config__10 =
  Sqlite.Bindings.sqlite3_config__10

sqlite3_config__11 :: CInt -> Ptr CInt -> IO CInt
sqlite3_config__11 =
  Sqlite.Bindings.sqlite3_config__11

sqlite3_config__12 :: CInt -> CUInt -> IO CInt
sqlite3_config__12 =
  Sqlite.Bindings.sqlite3_config__12

sqlite3_config__13 :: CInt -> Int64 -> IO CInt
sqlite3_config__13 =
  Sqlite.Bindings.sqlite3_config__13

-- | https://www.sqlite.org/c3ref/context_db_handle.html
sqlite3_context_db_handle ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  -- | Connection.
  IO (Ptr Sqlite.Bindings.Sqlite3)
sqlite3_context_db_handle =
  Sqlite.Bindings.sqlite3_context_db_handle

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
sqlite3_create_collation ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
sqlite3_create_collation =
  Sqlite.Bindings.sqlite3_create_collation

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
sqlite3_create_collation_v2 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
sqlite3_create_collation_v2 =
  Sqlite.Bindings.sqlite3_create_collation_v2

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Create a VFS filename.
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
sqlite3_create_filename =
  Sqlite.Bindings.sqlite3_create_filename

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create a function or aggregate function.
sqlite3_create_function ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of function arguments.
  CInt ->
  -- | Encoding and flags.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Function.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function step.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function finalize.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_function =
  Sqlite.Bindings.sqlite3_create_function

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create a function or aggregate function.
sqlite3_create_function_v2 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of function arguments.
  CInt ->
  -- | Encoding and flags.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Function.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function step.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function finalize.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> IO ()) ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  IO CInt
sqlite3_create_function_v2 =
  Sqlite.Bindings.sqlite3_create_function_v2

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
sqlite3_create_module ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Module name (UTF-8).
  CString ->
  -- | Module.
  Ptr Sqlite.Bindings.Sqlite3_module ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_create_module =
  Sqlite.Bindings.sqlite3_create_module

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
sqlite3_create_module_v2 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Module name (UTF-8).
  CString ->
  -- | Module.
  Ptr Sqlite.Bindings.Sqlite3_module ->
  -- | Application data.
  Ptr a ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_module_v2 =
  Sqlite.Bindings.sqlite3_create_module_v2

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create an aggregate function or an aggregate window function.
sqlite3_create_window_function ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of function arguments.
  CInt ->
  -- | Flags.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Aggregate function step.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function finalize.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> IO ()) ->
  -- | Aggregate window function get current value.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> IO ()) ->
  -- | Aggregate window function remove value.
  FunPtr (Ptr Sqlite.Bindings.Sqlite3_context -> CInt -> Ptr (Ptr Sqlite.Bindings.Sqlite3_value) -> IO ()) ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  IO CInt
sqlite3_create_window_function =
  Sqlite.Bindings.sqlite3_create_window_function

-- | https://www.sqlite.org/c3ref/data_count.html
--
-- Get the number of columns in the next row of a result set.
sqlite3_data_count ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Number of columns.
  IO CInt
sqlite3_data_count =
  Sqlite.Bindings.sqlite3_data_count

-- | https://www.sqlite.org/c3ref/database_file_object.html
--
-- Get the file object for a journal.
sqlite3_database_file_object ::
  CString ->
  IO (Ptr Sqlite.Bindings.Sqlite3_file)
sqlite3_database_file_object =
  Sqlite.Bindings.sqlite3_database_file_object

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
--
-- Flush all dirty pager-cache pages to disk, for all attached databases.
sqlite3_db_cacheflush ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_db_cacheflush =
  Sqlite.Bindings.sqlite3_db_cacheflush

sqlite3_db_config__1 :: Ptr Sqlite.Bindings.Sqlite3 -> CInt -> CString -> IO CInt
sqlite3_db_config__1 =
  Sqlite.Bindings.sqlite3_db_config__1

sqlite3_db_config__2 :: Ptr Sqlite.Bindings.Sqlite3 -> CInt -> Ptr a -> CInt -> CInt -> IO CInt
sqlite3_db_config__2 =
  Sqlite.Bindings.sqlite3_db_config__2

sqlite3_db_config__3 :: Ptr Sqlite.Bindings.Sqlite3 -> CInt -> CInt -> Ptr CInt -> IO CInt
sqlite3_db_config__3 =
  Sqlite.Bindings.sqlite3_db_config__3

-- | https://www.sqlite.org/c3ref/db_filename.html
--
-- Get the filename for an attached database.
sqlite3_db_filename ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Filename (UTF-8).
  IO CString
sqlite3_db_filename =
  Sqlite.Bindings.sqlite3_db_filename

-- | https://www.sqlite.org/c3ref/db_handle.html
--
-- Get the connection for a statement.
sqlite3_db_handle ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3
sqlite3_db_handle =
  Sqlite.Bindings.sqlite3_db_handle

-- | https://www.sqlite.org/c3ref/db_mutex.html
--
-- Get the mutex of a connection.
sqlite3_db_mutex ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Mutex.
  IO (Ptr Sqlite.Bindings.Sqlite3_mutex)
sqlite3_db_mutex =
  Sqlite.Bindings.sqlite3_db_mutex

-- | https://www.sqlite.org/c3ref/db_name.html
--
-- Get the name of an attached database.
sqlite3_db_name ::
  -- | Connection
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Database index (0-based; 0 is the main database file).
  CInt ->
  -- | Database name (UTF-8).
  IO CString
sqlite3_db_name =
  Sqlite.Bindings.sqlite3_db_name

-- | https://www.sqlite.org/c3ref/db_readonly.html
--
-- Get whether an attached database is read-only.
sqlite3_db_readonly ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | 0 or 1, or -1 if the database is not attached.
  IO CInt
sqlite3_db_readonly =
  Sqlite.Bindings.sqlite3_db_readonly

-- | https://www.sqlite.org/c3ref/db_release_memory.html
--
-- Release as much memory as possible from a connection.
sqlite3_db_release_memory ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_db_release_memory =
  Sqlite.Bindings.sqlite3_db_release_memory

-- | https://www.sqlite.org/c3ref/db_status.html
--
-- Get a status of a connection.
sqlite3_db_status ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
sqlite3_db_status =
  Sqlite.Bindings.sqlite3_db_status

-- | https://www.sqlite.org/c3ref/declare_vtab.html
--
-- Declare the schema of a virtual table.
sqlite3_declare_vtab ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Schema (UTF-8).
  CString ->
  -- | Result code.
  IO CInt
sqlite3_declare_vtab =
  Sqlite.Bindings.sqlite3_declare_vtab

-- | [__Deserialize a database__](https://www.sqlite.org/c3ref/deserialize.html)
sqlite3_deserialize ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
sqlite3_deserialize =
  Sqlite.Bindings.sqlite3_deserialize

-- | [__Remove unnecessary virtual table implementations__](https://www.sqlite.org/c3ref/drop_modules.html)
sqlite3_drop_modules ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Names of virtual table modules to keep (UTF-8).
  Ptr CString ->
  IO CInt
sqlite3_drop_modules =
  Sqlite.Bindings.sqlite3_drop_modules

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
sqlite3_extended_result_codes ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  CInt ->
  IO CInt
sqlite3_extended_result_codes =
  Sqlite.Bindings.sqlite3_extended_result_codes

-- | https://www.sqlite.org/c3ref/errcode.html
sqlite3_errcode ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_errcode =
  Sqlite.Bindings.sqlite3_errcode

-- | https://www.sqlite.org/c3ref/errcode.html
sqlite3_errmsg ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Error message (UTF-8).
  IO CString
sqlite3_errmsg =
  Sqlite.Bindings.sqlite3_errmsg

-- | https://www.sqlite.org/c3ref/errcode.html
sqlite3_error_offset ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_error_offset =
  Sqlite.Bindings.sqlite3_error_offset

-- | https://www.sqlite.org/c3ref/errcode.html
sqlite3_errstr ::
  CInt ->
  CString
sqlite3_errstr =
  Sqlite.Bindings.sqlite3_errstr

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the expanded SQL of a statement.
sqlite3_expanded_sql ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_expanded_sql =
  Sqlite.Bindings.sqlite3_expanded_sql

-- | https://www.sqlite.org/c3ref/errcode.html
sqlite3_extended_errcode ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_extended_errcode =
  Sqlite.Bindings.sqlite3_extended_errcode

sqlite3_file_control ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Opcode.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_file_control =
  Sqlite.Bindings.sqlite3_file_control

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the database file for a database file, journal file, or WAL file.
sqlite3_filename_database ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | Database file.
  IO CString
sqlite3_filename_database =
  Sqlite.Bindings.sqlite3_filename_database

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the journal file for a database file, journal file, or WAL file.
sqlite3_filename_journal ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | Journal file.
  IO CString
sqlite3_filename_journal =
  Sqlite.Bindings.sqlite3_filename_journal

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the WAL file for a database file, journal file, or WAL file.
sqlite3_filename_wal ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | WAL file.
  IO CString
sqlite3_filename_wal =
  Sqlite.Bindings.sqlite3_filename_wal

-- | https://www.sqlite.org/c3ref/finalize.html
--
-- Finalize a statement.
sqlite3_finalize ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_finalize (Sqlite3_stmt statement) =
  Sqlite.Bindings.sqlite3_finalize statement

-- | https://www.sqlite.org/c3ref/free.html
sqlite3_free ::
  Ptr a ->
  IO ()
sqlite3_free =
  Sqlite.Bindings.sqlite3_free

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Release memory acquired by 'sqlite3_create_filename'.
sqlite3_free_filename ::
  -- | Filename.
  CString ->
  IO ()
sqlite3_free_filename =
  Sqlite.Bindings.sqlite3_free_filename

-- | https://www.sqlite.org/c3ref/get_autocommit.html
--
-- Get whether a connection is in autocommit mode.
sqlite3_get_autocommit ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | 0 or 1.
  IO CInt
sqlite3_get_autocommit =
  Sqlite.Bindings.sqlite3_get_autocommit

-- | https://www.sqlite.org/c3ref/get_auxdata.html
sqlite3_get_auxdata ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CInt ->
  IO (Ptr a)
sqlite3_get_auxdata =
  Sqlite.Bindings.sqlite3_get_auxdata

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
sqlite3_hard_heap_limit64 ::
  -- | Limit, in bytes, or a negative number to get the limit.
  Int64 ->
  -- | Previous limit, in bytes.
  IO Int64
sqlite3_hard_heap_limit64 =
  Sqlite.Bindings.sqlite3_hard_heap_limit64

-- | https://www.sqlite.org/c3ref/initialize.html
--
-- Initialize the library.
sqlite3_initialize ::
  -- | Result code.
  IO CInt
sqlite3_initialize =
  Sqlite.Bindings.sqlite3_initialize

-- | https://www.sqlite.org/c3ref/interrupt.html
--
-- Cause all in-progress operations to abort and return `SQLITE_INTERRUPT` at the earliest opportunity.
sqlite3_interrupt ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO ()
sqlite3_interrupt =
  Sqlite.Bindings.sqlite3_interrupt

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- Get whether a string is a keyword.
sqlite3_keyword_check ::
  -- | String (UTF-8).
  Ptr CChar ->
  -- | Size of string, in bytes.
  CInt ->
  -- | 0 or 1.
  CInt
sqlite3_keyword_check =
  Sqlite.Bindings.sqlite3_keyword_check

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- The number of distinct keywords.
sqlite3_keyword_count :: CInt
sqlite3_keyword_count =
  Sqlite.Bindings.sqlite3_keyword_count

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- Get a keyword by index.
sqlite3_keyword_name ::
  -- | Keyword index (0-based).
  CInt ->
  -- | /Out/: keyword (UTF-8).
  Ptr (Ptr CChar) ->
  -- | /Out/: size of keyword, in bytes.
  Ptr CInt ->
  -- | Result code.
  IO CInt
sqlite3_keyword_name =
  Sqlite.Bindings.sqlite3_keyword_name

-- | https://www.sqlite.org/c3ref/last_insert_rowid.html
--
-- Get the rowid of the most recent insert into a table with a rowid.
sqlite3_last_insert_rowid ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Rowid.
  IO Int64
sqlite3_last_insert_rowid =
  Sqlite.Bindings.sqlite3_last_insert_rowid

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
sqlite3_libversion :: CString
sqlite3_libversion =
  Sqlite.Bindings.sqlite3_libversion

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
sqlite3_libversion_number :: CInt
sqlite3_libversion_number =
  Sqlite.Bindings.sqlite3_libversion_number

-- | https://www.sqlite.org/c3ref/limit.html
--
-- Get or set a limit on a connection.
sqlite3_limit ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Limit category.
  CInt ->
  -- | Limit, or a negative number to get the limit.
  CInt ->
  -- | Previous limit.
  IO CInt
sqlite3_limit =
  Sqlite.Bindings.sqlite3_limit

-- | https://www.sqlite.org/c3ref/load_extension.html
sqlite3_load_extension ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Extension shared library name.
  CString ->
  -- | Entry point.
  CString ->
  -- | /Out/: error message.
  Ptr CString ->
  -- | Result code.
  IO CInt
sqlite3_load_extension =
  Sqlite.Bindings.sqlite3_load_extension

-- | https://www.sqlite.org/c3ref/log.html
--
-- Write a message to the error log.
sqlite3_log ::
  -- | Error code.
  CInt ->
  -- | Error message.
  CString ->
  IO ()
sqlite3_log =
  Sqlite.Bindings.sqlite3_log

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
sqlite3_malloc ::
  -- | Size of memory, in bytes.
  CInt ->
  -- | Memory.
  IO (Ptr a)
sqlite3_malloc =
  Sqlite.Bindings.sqlite3_malloc

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
sqlite3_malloc64 ::
  -- | Size of memory, in bytes.
  Word64 ->
  -- | Memory.
  IO (Ptr a)
sqlite3_malloc64 =
  Sqlite.Bindings.sqlite3_malloc64

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the highest value of 'sqlite3_memory_used'.
sqlite3_memory_highwater ::
  -- | Reset highest value? (0 or 1).
  CInt ->
  -- | Highest value (prior to reset, if reset).
  IO Int64
sqlite3_memory_highwater =
  Sqlite.Bindings.sqlite3_memory_highwater

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the size of live memory, in bytes.
sqlite3_memory_used :: IO Int64
sqlite3_memory_used =
  Sqlite.Bindings.sqlite3_memory_used

-- | https://www.sqlite.org/c3ref/free.html
--
-- Get the size of memory allocated with 'sqlite3_malloc', 'sqlite3_malloc64', 'sqlite3_realloc', or
-- 'sqlite3_realloc64', in bytes.
sqlite3_msize ::
  -- | Memory.
  Ptr a ->
  -- | Size of memory, in bytes.
  IO Word64
sqlite3_msize =
  Sqlite.Bindings.sqlite3_msize

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Create a mutex.
sqlite3_mutex_alloc ::
  -- | Mutex type.
  CInt ->
  -- | Mutex.
  IO (Ptr Sqlite.Bindings.Sqlite3_mutex)
sqlite3_mutex_alloc =
  Sqlite.Bindings.sqlite3_mutex_alloc

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Acquire a mutex (blocking).
sqlite3_mutex_enter ::
  -- | Mutex.
  Ptr Sqlite.Bindings.Sqlite3_mutex ->
  IO ()
sqlite3_mutex_enter =
  Sqlite.Bindings.sqlite3_mutex_enter

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Destroy a mutex.
sqlite3_mutex_free ::
  -- | Mutex.
  Ptr Sqlite.Bindings.Sqlite3_mutex ->
  IO ()
sqlite3_mutex_free =
  Sqlite.Bindings.sqlite3_mutex_free

sqlite3_mutex_held = undefined

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Release a mutex. The mutex must have been acquired on the same OS thread.
sqlite3_mutex_leave ::
  -- | Mutex.
  Ptr Sqlite.Bindings.Sqlite3_mutex ->
  IO ()
sqlite3_mutex_leave =
  Sqlite.Bindings.sqlite3_mutex_leave

sqlite3_mutex_notheld = undefined

-- | https://www.sqlite.org/c3ref/mutex_alloc.html
--
-- Acquire a mutex (non-blocking).
sqlite3_mutex_try ::
  -- | Mutex.
  Ptr Sqlite.Bindings.Sqlite3_mutex ->
  -- | Result code.
  IO CInt
sqlite3_mutex_try =
  Sqlite.Bindings.sqlite3_mutex_try

-- | https://www.sqlite.org/c3ref/next_stmt.html
--
-- Get the first or next statement of a connection.
sqlite3_next_stmt ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Statement, or null to get the first statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Next statement.
  IO (Ptr Sqlite.Bindings.Sqlite3_stmt)
sqlite3_next_stmt =
  Sqlite.Bindings.sqlite3_next_stmt

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the normalized SQL of a statement.
sqlite3_normalized_sql ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_normalized_sql =
  Sqlite.Bindings.sqlite3_normalized_sql

-- | https://www.sqlite.org/c3ref/open.html
--
-- Open a new database connection.
sqlite3_open ::
  -- | Database file.
  Text ->
  -- | Connection, result code.
  IO (Maybe Sqlite3, CInt)
sqlite3_open database =
  textToCString database \c_database ->
    alloca \connectionPtr -> do
      code <- Sqlite.Bindings.sqlite3_open c_database connectionPtr
      connection <- peek connectionPtr
      pure (if connection == nullPtr then Nothing else Just (Sqlite3 connection), code)

-- | https://www.sqlite.org/c3ref/open.html
--
-- Open a new database connection.
sqlite3_open_v2 ::
  -- | Database file.
  Text ->
  -- | Mode.
  SQLITE_OPEN_MODE ->
  -- | Flags.
  SQLITE_OPEN_FLAGS ->
  -- | Name of VFS to use.
  Maybe Text ->
  -- | Connection, result code.
  IO (Maybe Sqlite3, CInt)
sqlite3_open_v2 database (SQLITE_OPEN_MODE mode) (SQLITE_OPEN_FLAGS flags) maybeVfs =
  textToCString database \c_database ->
    alloca \connectionPtr ->
      withVfs \c_vfs -> do
        code <- Sqlite.Bindings.sqlite3_open_v2 c_database connectionPtr (mode .|. flags) c_vfs
        connection <- peek connectionPtr
        pure (if connection == nullPtr then Nothing else Just (Sqlite3 connection), code)
  where
    withVfs :: (CString -> IO a) -> IO a
    withVfs k =
      case maybeVfs of
        Nothing -> k nullPtr
        Just vfs -> textToCString vfs k

-- | https://www.sqlite.org/c3ref/overload_function.html
sqlite3_overload_function ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  CInt ->
  IO CInt
sqlite3_overload_function =
  Sqlite.Bindings.sqlite3_overload_function

-- | https://www.sqlite.org/c3ref/prepare.html
sqlite3_prepare_v2 ::
  -- | Connection.
  Sqlite3 ->
  -- | SQL.
  Text ->
  -- | Statement, unused SQL, result code.
  IO (Maybe Sqlite3_stmt, Text, CInt)
sqlite3_prepare_v2 (Sqlite3 connection) sql =
  textToCStringLen sql \c_sql c_sql_len ->
    alloca \statementPtr ->
      alloca \unusedSqlPtr -> do
        code <-
          Sqlite.Bindings.sqlite3_prepare_v2
            connection
            c_sql
            (fromIntegral @Int @CInt c_sql_len)
            statementPtr
            unusedSqlPtr
        statement <- peek statementPtr
        c_unused_sql <- peek unusedSqlPtr
        let unusedSqlLen = (c_sql `plusPtr` c_sql_len) `minusPtr` c_unused_sql
        unusedSql <-
          if unusedSqlLen > 0
            then cstringLenToText c_unused_sql unusedSqlLen
            else pure Text.empty
        pure (if statement == nullPtr then Nothing else Just (Sqlite3_stmt statement), unusedSql, code)

-- | https://www.sqlite.org/c3ref/prepare.html
sqlite3_prepare_v3 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | SQL (UTF-8).
  Ptr CChar ->
  -- | Size of SQL, in bytes.
  CInt ->
  -- | Flags.
  CUInt ->
  -- | /Out/: statement.
  Ptr (Ptr Sqlite.Bindings.Sqlite3_stmt) ->
  -- | /Out/: unused SQL.
  Ptr (Ptr CChar) ->
  -- | Result code.
  IO CInt
sqlite3_prepare_v3 =
  Sqlite.Bindings.sqlite3_prepare_v3

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_blobwrite ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_preupdate_blobwrite =
  Sqlite.Bindings.sqlite3_preupdate_blobwrite

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_count ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_preupdate_count =
  Sqlite.Bindings.sqlite3_preupdate_count

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_depth ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_preupdate_depth =
  Sqlite.Bindings.sqlite3_preupdate_depth

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_hook ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  FunPtr (Ptr a -> Ptr Sqlite.Bindings.Sqlite3 -> CInt -> CString -> CString -> Int64 -> Int64 -> IO ()) ->
  Ptr a ->
  IO (Ptr b)
sqlite3_preupdate_hook =
  Sqlite.Bindings.sqlite3_preupdate_hook

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_new ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CInt ->
  Ptr (Ptr Sqlite.Bindings.Sqlite3_value) ->
  IO CInt
sqlite3_preupdate_new =
  Sqlite.Bindings.sqlite3_preupdate_new

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
sqlite3_preupdate_old ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CInt ->
  Ptr (Ptr Sqlite.Bindings.Sqlite3_value) ->
  IO CInt
sqlite3_preupdate_old =
  Sqlite.Bindings.sqlite3_preupdate_old

-- | https://www.sqlite.org/c3ref/progress_handler.html
sqlite3_progress_handler ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CInt ->
  FunPtr (Ptr a -> IO CInt) ->
  Ptr a ->
  IO ()
sqlite3_progress_handler =
  Sqlite.Bindings.sqlite3_progress_handler

-- | https://www.sqlite.org/c3ref/randomness.html
sqlite3_randomness ::
  CInt ->
  Ptr a ->
  IO ()
sqlite3_randomness =
  Sqlite.Bindings.sqlite3_randomness

-- | https://www.sqlite.org/c3ref/free.html
sqlite3_realloc ::
  Ptr a ->
  CInt ->
  IO (Ptr a)
sqlite3_realloc =
  Sqlite.Bindings.sqlite3_realloc

-- | https://www.sqlite.org/c3ref/free.html
sqlite3_realloc64 ::
  Ptr a ->
  Int64 ->
  IO (Ptr a)
sqlite3_realloc64 =
  Sqlite.Bindings.sqlite3_realloc64

-- | https://www.sqlite.org/c3ref/release_memory.html
--
-- Attempt to release memory by deallocating non-essential allocations, such as cache database pages used to improve
-- performance.
sqlite3_release_memory ::
  -- | Number of bytes to release.
  CInt ->
  -- | Number of bytes actually released (may be more or less than the requested amount).
  IO CInt
sqlite3_release_memory =
  Sqlite.Bindings.sqlite3_release_memory

-- | https://www.sqlite.org/c3ref/reset.html
--
-- Reset a statement to its initial state. Does not clear parameter bindings.
sqlite3_reset ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_reset =
  Sqlite.Bindings.sqlite3_reset

-- | https://www.sqlite.org/c3ref/reset_auto_extension.html
sqlite3_reset_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_blob ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr a ->
  CInt ->
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_blob =
  Sqlite.Bindings.sqlite3_result_blob

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_blob64 ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr a ->
  Word64 ->
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_blob64 =
  Sqlite.Bindings.sqlite3_result_blob64

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_double ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CDouble ->
  IO ()
sqlite3_result_double =
  Sqlite.Bindings.sqlite3_result_double

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_error ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CString ->
  CInt ->
  IO ()
sqlite3_result_error =
  Sqlite.Bindings.sqlite3_result_error

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_error_code ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CInt ->
  IO ()
sqlite3_result_error_code =
  Sqlite.Bindings.sqlite3_result_error_code

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_error_nomem ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  IO ()
sqlite3_result_error_nomem =
  Sqlite.Bindings.sqlite3_result_error_nomem

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_error_toobig ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  IO ()
sqlite3_result_error_toobig =
  Sqlite.Bindings.sqlite3_result_error_toobig

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_int ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CInt ->
  IO ()
sqlite3_result_int =
  Sqlite.Bindings.sqlite3_result_int

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_int64 ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Int64 ->
  IO ()
sqlite3_result_int64 =
  Sqlite.Bindings.sqlite3_result_int64

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_null ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  IO ()
sqlite3_result_null =
  Sqlite.Bindings.sqlite3_result_null

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_pointer ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr a ->
  CString ->
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_pointer =
  Sqlite.Bindings.sqlite3_result_pointer

-- | https://www.sqlite.org/c3ref/result_subtype.html
--
-- Set the subtype of the return value of a function.
sqlite3_result_subtype ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  -- | Subtype.
  CUInt ->
  IO ()
sqlite3_result_subtype =
  Sqlite.Bindings.sqlite3_result_subtype

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_text ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr CChar ->
  CInt ->
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_text =
  Sqlite.Bindings.sqlite3_result_text

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_text64 ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr CChar ->
  Word64 ->
  FunPtr (Ptr a -> IO ()) ->
  CUChar ->
  IO ()
sqlite3_result_text64 =
  Sqlite.Bindings.sqlite3_result_text64

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_value ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Ptr Sqlite.Bindings.Sqlite3_value ->
  IO ()
sqlite3_result_value =
  Sqlite.Bindings.sqlite3_result_value

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_zeroblob ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CInt ->
  IO ()
sqlite3_result_zeroblob =
  Sqlite.Bindings.sqlite3_result_zeroblob

-- | https://www.sqlite.org/c3ref/result_blob.html
sqlite3_result_zeroblob64 ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  Word64 ->
  IO CInt
sqlite3_result_zeroblob64 =
  Sqlite.Bindings.sqlite3_result_zeroblob64

-- | https://www.sqlite.org/c3ref/commit_hook.html
sqlite3_rollback_hook ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Rollback hook.
  FunPtr (Ptr a -> IO CInt) ->
  Ptr a ->
  IO (Ptr b)
sqlite3_rollback_hook =
  Sqlite.Bindings.sqlite3_rollback_hook

-- | https://www.sqlite.org/c3ref/serialize.html
sqlite3_serialize ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  Ptr Int64 ->
  CUInt ->
  IO (Ptr CUChar)
sqlite3_serialize =
  Sqlite.Bindings.sqlite3_serialize

-- | https://www.sqlite.org/c3ref/set_authorizer.html
sqlite3_set_authorizer ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  FunPtr (Ptr a -> CInt -> CString -> CString -> CString -> CString -> IO CInt) ->
  Ptr a ->
  IO CInt
sqlite3_set_authorizer =
  Sqlite.Bindings.sqlite3_set_authorizer

-- | https://www.sqlite.org/c3ref/get_auxdata.html
sqlite3_set_auxdata ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  CInt ->
  Ptr a ->
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_set_auxdata =
  Sqlite.Bindings.sqlite3_set_auxdata

-- | https://www.sqlite.org/c3ref/set_last_insert_rowid.html
sqlite3_set_last_insert_rowid ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  Int64 ->
  IO ()
sqlite3_set_last_insert_rowid =
  Sqlite.Bindings.sqlite3_set_last_insert_rowid

-- | https://www.sqlite.org/c3ref/initialize.html
sqlite3_shutdown :: IO CInt
sqlite3_shutdown =
  Sqlite.Bindings.sqlite3_shutdown

-- | https://www.sqlite.org/c3ref/sleep.html
sqlite3_sleep ::
  CInt ->
  IO CInt
sqlite3_sleep =
  Sqlite.Bindings.sqlite3_sleep

-- | https://www.sqlite.org/c3ref/snapshot_cmp.html
sqlite3_snapshot_cmp ::
  Ptr Sqlite.Bindings.Sqlite3_snapshot ->
  Ptr Sqlite.Bindings.Sqlite3_snapshot ->
  IO CInt
sqlite3_snapshot_cmp =
  Sqlite.Bindings.sqlite3_snapshot_cmp

-- | https://www.sqlite.org/c3ref/snapshot_free.html
sqlite3_snapshot_free ::
  Ptr Sqlite.Bindings.Sqlite3_snapshot ->
  IO ()
sqlite3_snapshot_free =
  Sqlite.Bindings.sqlite3_snapshot_free

-- | https://www.sqlite.org/c3ref/snapshot_get.html
sqlite3_snapshot_get ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  Ptr (Ptr Sqlite.Bindings.Sqlite3_snapshot) ->
  IO CInt
sqlite3_snapshot_get =
  Sqlite.Bindings.sqlite3_snapshot_get

-- | https://www.sqlite.org/c3ref/snapshot_open.html
sqlite3_snapshot_open ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  Ptr Sqlite.Bindings.Sqlite3_snapshot ->
  IO CInt
sqlite3_snapshot_open =
  Sqlite.Bindings.sqlite3_snapshot_open

-- | https://www.sqlite.org/c3ref/snapshot_recover.html
sqlite3_snapshot_recover ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  IO CInt
sqlite3_snapshot_recover =
  Sqlite.Bindings.sqlite3_snapshot_recover

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
sqlite3_soft_heap_limit64 ::
  -- | Limit, in bytes, or a negative number to get the limit.
  Int64 ->
  -- | Previous limit, in bytes.
  IO Int64
sqlite3_soft_heap_limit64 =
  Sqlite.Bindings.sqlite3_soft_heap_limit64

-- | https://www.sqlite.org/c3ref/libversion.html
sqlite3_sourceid :: CString
sqlite3_sourceid =
  Sqlite.Bindings.sqlite3_sourceid

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the SQL of a statement.
sqlite3_sql ::
  -- | Statement.
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_sql =
  Sqlite.Bindings.sqlite3_sql

-- | https://www.sqlite.org/c3ref/status.html
sqlite3_status ::
  CInt ->
  Ptr CInt ->
  Ptr CInt ->
  CInt ->
  IO CInt
sqlite3_status =
  Sqlite.Bindings.sqlite3_status

-- | https://www.sqlite.org/c3ref/status.html
sqlite3_status64 ::
  CInt ->
  Ptr Int64 ->
  Ptr Int64 ->
  CInt ->
  IO CInt
sqlite3_status64 =
  Sqlite.Bindings.sqlite3_status64

-- | https://www.sqlite.org/c3ref/step.html
--
-- Produce the next row of a statement.
sqlite3_step ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_step (Sqlite3_stmt statement) =
  Sqlite.Bindings.sqlite3_step statement

-- | https://www.sqlite.org/c3ref/stmt_busy.html
sqlite3_stmt_busy ::
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  IO CInt
sqlite3_stmt_busy =
  Sqlite.Bindings.sqlite3_stmt_busy

-- | https://www.sqlite.org/c3ref/stmt_isexplain.html
sqlite3_stmt_isexplain ::
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  IO CInt
sqlite3_stmt_isexplain =
  Sqlite.Bindings.sqlite3_stmt_isexplain

-- | https://www.sqlite.org/c3ref/stmt_readonly.html
sqlite3_stmt_readonly ::
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  IO CInt
sqlite3_stmt_readonly =
  Sqlite.Bindings.sqlite3_stmt_readonly

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus.html
-- sqlite3_stmt_scanstatus ::
--   Ptr Sqlite.Bindings.Sqlite3_stmt ->
--   CInt ->
--   CInt ->
--   Ptr a ->
--   IO CInt
-- sqlite3_stmt_scanstatus =
--   Sqlite.Bindings.sqlite3_stmt_scanstatus

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus_reset.html
-- sqlite3_stmt_scanstatus_reset ::
--   Ptr Sqlite.Bindings.Sqlite3_stmt ->
--   IO ()
-- sqlite3_stmt_scanstatus_reset =
--   Sqlite.Bindings.sqlite3_stmt_scanstatus_reset

-- | https://www.sqlite.org/c3ref/stmt_status.html
sqlite3_stmt_status ::
  Ptr Sqlite.Bindings.Sqlite3_stmt ->
  CInt ->
  CInt ->
  IO CInt
sqlite3_stmt_status =
  Sqlite.Bindings.sqlite3_stmt_status

-- | https://www.sqlite.org/c3ref/strglob.html
--
-- Get whether a string matches a glob pattern.
sqlite3_strglob ::
  -- | Glob pattern (UTF-8).
  CString ->
  -- | String (UTF-8).
  CString ->
  -- | 0 if matches.
  CInt
sqlite3_strglob =
  Sqlite.Bindings.sqlite3_strglob

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding).
sqlite3_stricmp ::
  CString ->
  CString ->
  IO CInt
sqlite3_stricmp =
  Sqlite.Bindings.sqlite3_stricmp

-- | https://www.sqlite.org/c3ref/strlike.html
--
-- Get whether a string matches a like pattern.
sqlite3_strlike ::
  -- | Like pattern (UTF-8).
  CString ->
  -- | String (UTF-8)
  CString ->
  -- | Escape character.
  CUInt ->
  -- | 0 if matches.
  CInt
sqlite3_strlike =
  Sqlite.Bindings.sqlite3_strlike

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding), up to a certain length.
sqlite3_strnicmp ::
  CString ->
  CString ->
  CInt ->
  IO CInt
sqlite3_strnicmp =
  Sqlite.Bindings.sqlite3_strnicmp

-- | https://www.sqlite.org/c3ref/system_errno.html
sqlite3_system_errno ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_system_errno =
  Sqlite.Bindings.sqlite3_system_errno

-- | https://www.sqlite.org/c3ref/table_column_metadata.html
sqlite3_table_column_metadata ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  CString ->
  CString ->
  Ptr CString ->
  Ptr CString ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr CInt ->
  IO CInt
sqlite3_table_column_metadata =
  Sqlite.Bindings.sqlite3_table_column_metadata

-- | https://www.sqlite.org/c3ref/threadsafe.html
sqlite3_threadsafe :: CInt
sqlite3_threadsafe =
  Sqlite.Bindings.sqlite3_threadsafe

-- | https://www.sqlite.org/c3ref/total_changes.html
sqlite3_total_changes ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO CInt
sqlite3_total_changes =
  Sqlite.Bindings.sqlite3_total_changes

-- | https://www.sqlite.org/c3ref/total_changes.html
sqlite3_total_changes64 ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  IO Int64
sqlite3_total_changes64 =
  Sqlite.Bindings.sqlite3_total_changes64

-- | https://www.sqlite.org/c3ref/trace_v2.html
sqlite3_trace_v2 ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CUInt ->
  FunPtr (CUInt -> Ptr a -> Ptr b -> Ptr c -> IO CInt) ->
  Ptr a ->
  IO CInt
sqlite3_trace_v2 =
  Sqlite.Bindings.sqlite3_trace_v2

-- | https://www.sqlite.org/c3ref/txn_state.html
sqlite3_txn_state ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  CString ->
  IO CInt
sqlite3_txn_state =
  Sqlite.Bindings.sqlite3_txn_state

-- | https://www.sqlite.org/c3ref/unlock_notify.html
sqlite3_unlock_notify ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  FunPtr (Ptr (Ptr a) -> CInt -> IO ()) ->
  Ptr a ->
  IO CInt
sqlite3_unlock_notify =
  Sqlite.Bindings.sqlite3_unlock_notify

-- | https://www.sqlite.org/c3ref/update_hook.html
sqlite3_update_hook ::
  Ptr Sqlite.Bindings.Sqlite3 ->
  FunPtr (Ptr a -> CInt -> CString -> CString -> Int64 -> IO ()) ->
  Ptr a ->
  IO (Ptr b)
sqlite3_update_hook =
  Sqlite.Bindings.sqlite3_update_hook

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a boolean query parameter of a database file.
sqlite3_uri_boolean ::
  -- | Database file.
  CString ->
  -- | Query parameter name.
  CString ->
  -- | Default value.
  CInt ->
  -- | Query parameter value (0 or 1).
  IO CInt
sqlite3_uri_boolean =
  Sqlite.Bindings.sqlite3_uri_boolean

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get an integer query parameter of a database file.
sqlite3_uri_int64 ::
  -- | Database file.
  CString ->
  -- | Query parameter name.
  CString ->
  -- | Default value.
  Int64 ->
  -- | Query parameter value.
  IO Int64
sqlite3_uri_int64 =
  Sqlite.Bindings.sqlite3_uri_int64

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a query parameter name of a database file.
sqlite3_uri_key ::
  -- | Database file.
  CString ->
  -- | Query parameter index (0-based).
  CInt ->
  -- | Query parameter name.
  IO CString
sqlite3_uri_key =
  Sqlite.Bindings.sqlite3_uri_key

-- | https://www.sqlite.org/c3ref/uri_boolean.html
--
-- Get a query parameter of a database file.
sqlite3_uri_parameter ::
  -- | Database file.
  CString ->
  -- | Query parameter name.
  CString ->
  -- | Query parameter value.
  IO CString
sqlite3_uri_parameter =
  Sqlite.Bindings.sqlite3_uri_parameter

-- | https://www.sqlite.org/c3ref/user_data.html
sqlite3_user_data ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  IO (Ptr a)
sqlite3_user_data =
  Sqlite.Bindings.sqlite3_user_data

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the blob of a protected value.
sqlite3_value_blob ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Blob.
  IO (Ptr a)
sqlite3_value_blob =
  Sqlite.Bindings.sqlite3_value_blob

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the size of a protected blob or string value, in bytes.
sqlite3_value_bytes ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Size, in bytes.
  IO CInt
sqlite3_value_bytes =
  Sqlite.Bindings.sqlite3_value_bytes

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the double of a protected value.
sqlite3_value_double ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Double.
  IO CDouble
sqlite3_value_double =
  Sqlite.Bindings.sqlite3_value_double

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Copy a value.
sqlite3_value_dup ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Value copy.
  IO (Ptr Sqlite.Bindings.Sqlite3_value)
sqlite3_value_dup =
  Sqlite.Bindings.sqlite3_value_dup

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Release memory acquired by 'sqlite3_value_dup'.
sqlite3_value_free ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  IO ()
sqlite3_value_free =
  Sqlite.Bindings.sqlite3_value_free

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get whether a protected value is a bound parameter.
sqlite3_value_frombind ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | 0 or 1.
  IO CInt
sqlite3_value_frombind =
  Sqlite.Bindings.sqlite3_value_frombind

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
sqlite3_value_int ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Integer.
  IO CInt
sqlite3_value_int =
  Sqlite.Bindings.sqlite3_value_int

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
sqlite3_value_int64 ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Integer.
  IO Int64
sqlite3_value_int64 =
  Sqlite.Bindings.sqlite3_value_int64

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Within @xUpdate@, get whether the column corresponding to a protected value is unchanged.
sqlite3_value_nochange ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- 0 or 1.
  IO CInt
sqlite3_value_nochange =
  Sqlite.Bindings.sqlite3_value_nochange

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the numeric type of a protected value.
sqlite3_value_numeric_type ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Type.
  IO CInt
sqlite3_value_numeric_type =
  Sqlite.Bindings.sqlite3_value_numeric_type

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the pointer of a protected value.
sqlite3_value_pointer ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Pointer type.
  CString ->
  -- | Pointer.
  IO (Ptr a)
sqlite3_value_pointer =
  Sqlite.Bindings.sqlite3_value_pointer

-- | https://www.sqlite.org/c3ref/value_subtype.html
--
-- Get the subtype of the return value of a function.
sqlite3_value_subtype ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Subtype.
  IO CUInt
sqlite3_value_subtype =
  Sqlite.Bindings.sqlite3_value_subtype

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the string of a protected value.
sqlite3_value_text ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | String (UTF-8)
  IO (Ptr CUChar)
sqlite3_value_text =
  Sqlite.Bindings.sqlite3_value_text

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the type of a protected value.
sqlite3_value_type ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | Type.
  IO CInt
sqlite3_value_type =
  Sqlite.Bindings.sqlite3_value_type

-- | https://www.sqlite.org/c3ref/libversion.html
sqlite3_version :: CString
sqlite3_version =
  Sqlite.Bindings.sqlite3_version

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Get a VFS.
sqlite3_vfs_find ::
  -- | VFS name (UTF-8).
  CString ->
  -- | VFS.
  IO (Ptr Sqlite.Bindings.Sqlite3_vfs)
sqlite3_vfs_find =
  Sqlite.Bindings.sqlite3_vfs_find

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Register a VFS.
sqlite3_vfs_register ::
  -- | VFS.
  Ptr Sqlite.Bindings.Sqlite3_vfs ->
  -- | Make default? (0 or 1).
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_vfs_register =
  Sqlite.Bindings.sqlite3_vfs_register

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Unregister a VFS.
sqlite3_vfs_unregister ::
  -- | VFS.
  Ptr Sqlite.Bindings.Sqlite3_vfs ->
  -- | Result code.
  IO CInt
sqlite3_vfs_unregister =
  Sqlite.Bindings.sqlite3_vfs_unregister

-- | https://www.sqlite.org/c3ref/vtab_collation.html
--
-- Get the collating sequence of a virtual table constraint.
sqlite3_vtab_collation ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr Sqlite.Bindings.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | Collating sequence name (UTF-8).
  IO CString
sqlite3_vtab_collation =
  Sqlite.Bindings.sqlite3_vtab_collation

sqlite3_vtab_config__1 :: Ptr Sqlite.Bindings.Sqlite3 -> CInt -> CInt -> IO CInt
sqlite3_vtab_config__1 =
  Sqlite.Bindings.sqlite3_vtab_config__1

sqlite3_vtab_config__2 :: Ptr Sqlite.Bindings.Sqlite3 -> CInt -> IO CInt
sqlite3_vtab_config__2 =
  Sqlite.Bindings.sqlite3_vtab_config__2

-- | https://www.sqlite.org/c3ref/vtab_distinct.html
--
-- Get information about how the query planner wants output to be ordered.
sqlite3_vtab_distinct ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr Sqlite.Bindings.Sqlite3_index_info ->
  -- | 0, 1, 2, or 3.
  IO CInt
sqlite3_vtab_distinct =
  Sqlite.Bindings.sqlite3_vtab_distinct

-- | https://www.sqlite.org/c3ref/vtab_in.html
--
-- Get whether a virtual table constraint is an @IN@ operator that can be processed all at once.
sqlite3_vtab_in ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr Sqlite.Bindings.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | -1, 0, or 1.
  CInt ->
  -- | 0 or 1.
  IO CInt
sqlite3_vtab_in =
  Sqlite.Bindings.sqlite3_vtab_in

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the first value on the right-hand side of an @IN@ constraint.
sqlite3_vtab_in_first ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | /Out/: Value.
  Ptr (Ptr Sqlite.Bindings.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_in_first =
  Sqlite.Bindings.sqlite3_vtab_in_first

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the next value on the right-hand side of an @IN@ constraint.
sqlite3_vtab_in_next ::
  -- | Value.
  Ptr Sqlite.Bindings.Sqlite3_value ->
  -- | /Out/: Value.
  Ptr (Ptr Sqlite.Bindings.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_in_next =
  Sqlite.Bindings.sqlite3_vtab_in_next

-- | https://www.sqlite.org/c3ref/vtab_nochange.html
--
-- Within @xColumn@, get whether the column is being fetched as part of an @UPDATE@ in which its value will not change.
sqlite3_vtab_nochange ::
  -- | Function context.
  Ptr Sqlite.Bindings.Sqlite3_context ->
  -- | 0 or 1.
  IO CInt
sqlite3_vtab_nochange =
  Sqlite.Bindings.sqlite3_vtab_nochange

-- | https://www.sqlite.org/c3ref/vtab_on_conflict.html
--
-- Within @xUpdate@ for an @INSERT@ or @UPDATE@, get the conflict resolution algorithm.
sqlite3_vtab_on_conflict ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Conflict resolution algorithm.
  IO CInt
sqlite3_vtab_on_conflict =
  Sqlite.Bindings.sqlite3_vtab_on_conflict

-- | https://www.sqlite.org/c3ref/vtab_rhs_value.html
--
-- Within @xBestIndex@, get the right-hand side of a virtual table constraint.
sqlite3_vtab_rhs_value ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr Sqlite.Bindings.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | /Out/: value.
  Ptr (Ptr Sqlite.Bindings.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_rhs_value =
  Sqlite.Bindings.sqlite3_vtab_rhs_value

-- | https://www.sqlite.org/c3ref/wal_autocheckpoint.html
--
-- Register a callback that checkpoints the WAL after committing a transaction if there are more than a certain number
-- of frames in the WAL.
sqlite3_wal_autocheckpoint ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Number of frames that will trigger a checkpoint.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_wal_autocheckpoint =
  Sqlite.Bindings.sqlite3_wal_autocheckpoint

-- | https://www.sqlite.org/c3ref/wal_checkpoint.html
--
-- Checkpoint the WAL.
sqlite3_wal_checkpoint ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Result code.
  IO CInt
sqlite3_wal_checkpoint =
  Sqlite.Bindings.sqlite3_wal_checkpoint

-- | https://www.sqlite.org/c3ref/wal_checkpoint_v2.html
--
-- Checkpoint the WAL.
sqlite3_wal_checkpoint_v2 ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
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
sqlite3_wal_checkpoint_v2 =
  Sqlite.Bindings.sqlite3_wal_checkpoint_v2

-- | https://www.sqlite.org/c3ref/wal_hook.html
--
-- Register a callback that is invoked each time data is committed to a database in WAL mode.
sqlite3_wal_hook ::
  -- | Connection.
  Ptr Sqlite.Bindings.Sqlite3 ->
  -- | Callback.
  FunPtr (Ptr a -> Ptr Sqlite.Bindings.Sqlite3 -> CString -> CInt -> IO CInt) ->
  -- | Application data.
  Ptr a ->
  -- | Previous application data.
  IO (Ptr b)
sqlite3_wal_hook =
  Sqlite.Bindings.sqlite3_wal_hook
