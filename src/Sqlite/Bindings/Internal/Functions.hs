module Sqlite.Bindings.Internal.Functions where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CDouble (..), CInt (..), CUChar (..))
import Foreign.Ptr (FunPtr, Ptr)
import Sqlite.Bindings.Internal.Objects

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_blob"
  sqlite3_bind_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Length of blob in bytes.
    CInt ->
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_double"
  sqlite3_bind_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Double.
    CDouble ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_int"
  sqlite3_bind_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Integer.
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_null"
  sqlite3_bind_null ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_text"
  sqlite3_bind_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Length of string in bytes.
    CInt ->
    FunPtr (Ptr CChar -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_zeroblob"
  sqlite3_bind_zeroblob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Length of blob in bytes.
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_bytes.html
foreign import capi unsafe "sqlite3.h sqlite3_blob_bytes"
  sqlite3_blob_bytes ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_close.html
foreign import capi unsafe "sqlite3.h sqlite3_blob_close"
  sqlite3_blob_close ::
    -- | Blob.
    Ptr Sqlite3_blob ->
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_open.html
foreign import capi unsafe "sqlite3.h sqlite3_blob_open"
  sqlite3_blob_open ::
    -- | Database.
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
    -- | Out: blob.
    Ptr (Ptr Sqlite3_blob) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/blob_read.html
foreign import capi unsafe "sqlite3.h sqlite3_blob_read"
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

-- | https://www.sqlite.org/c3ref/blob_write.html
foreign import capi unsafe "sqlite3.h sqlite3_blob_write"
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

-- | https://www.sqlite.org/c3ref/busy_handler.html
foreign import capi unsafe "sqlite3.h sqlite3_busy_handler"
  sqlite3_busy_handler ::
    -- | Database.
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CInt -> IO CInt) ->
    Ptr a ->
    IO CInt

-- | https://www.sqlite.org/c3ref/changes.html
foreign import capi unsafe "sqlite3.h sqlite3_changes"
  sqlite3_changes ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/clear_bindings.html
foreign import capi unsafe "sqlite3.h sqlite3_clear_bindings"
  sqlite3_clear_bindings ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
foreign import capi unsafe "sqlite3.h sqlite3_close"
  sqlite3_close ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
foreign import capi unsafe "sqlite3.h sqlite3_close_v2"
  sqlite3_close_v2 ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_blob"
  sqlite3_column_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_bytes"
  sqlite3_column_bytes ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_count.html
foreign import capi unsafe "sqlite3.h sqlite3_column_count"
  sqlite3_column_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_double"
  sqlite3_column_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CDouble

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_int"
  sqlite3_column_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_text"
  sqlite3_column_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
foreign import capi unsafe "sqlite3.h sqlite3_extended_result_codes"
  sqlite3_extended_result_codes ::
    -- | Database.
    Ptr Sqlite3 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import capi unsafe "sqlite3.h sqlite3_errcode"
  sqlite3_errcode ::
    -- | Database
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import capi unsafe "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg ::
    -- | Database
    Ptr Sqlite3 ->
    IO CString

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import capi unsafe "sqlite3.h sqlite3_error_offset"
  sqlite3_error_offset ::
    -- | Database
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import capi unsafe "sqlite3.h sqlite3_extended_errcode"
  sqlite3_extended_errcode ::
    -- | Database
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/finalize.html
foreign import capi unsafe "sqlite3.h sqlite3_finalize"
  sqlite3_finalize ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/open.html
foreign import capi unsafe "sqlite3.h sqlite3_open_v2"
  sqlite3_open_v2 ::
    -- | Database file (UTF-8).
    CString ->
    -- | Out: database.
    Ptr (Ptr Sqlite3) ->
    -- | Flags.
    CInt ->
    -- | VFS module name.
    CString ->
    IO CInt

-- | https://www.sqlite.org/c3ref/prepare.html
foreign import capi unsafe "sqlite3.h sqlite3_prepare_v2"
  sqlite3_prepare_v2 ::
    -- | Database.
    Ptr Sqlite3 ->
    -- | SQL (UTF-8).
    Ptr CChar ->
    -- | Length of SQL in bytes.
    CInt ->
    -- | Out: statement.
    Ptr (Ptr Sqlite3_stmt) ->
    -- | Out: unused SQL.
    Ptr (Ptr CChar) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/reset.html
foreign import capi unsafe "sqlite3.h sqlite3_reset"
  sqlite3_reset ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
foreign import capi safe "sqlite3.h sqlite3_step"
  sqlite3_step__safe ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
foreign import capi unsafe "sqlite3.h sqlite3_step"
  sqlite3_step__unsafe ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

------------------------------------------------------------------------------------------------------------------------

foreign import ccall "wrapper"
  createBusyHandler :: (Ptr a -> CInt -> IO CInt) -> IO (FunPtr (Ptr a -> CInt -> IO CInt))
