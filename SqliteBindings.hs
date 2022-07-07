-- | https://www.sqlite.org/cintro.html
module SqliteBindings where

import Foreign.C.Types
import Foreign.Ptr (Ptr)

-- todo - annotate safe/unsafe

data Sqlite3

data Sqlite3_stmt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import ccall unsafe "sqlite3_bind_blob"
  sqlite3_bind_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Parameter value.
    Ptr a ->
    -- | Length of parameter value in bytes.
    CInt ->
    Ptr (Ptr () -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import ccall unsafe "sqlite3_bind_double"
  sqlite3_bind_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Parameter value.
    CDouble ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import ccall unsafe "sqlite3_bind_int"
  sqlite3_bind_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Parameter value.
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import ccall unsafe "sqlite3_bind_null"
  sqlite3_bind_null ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import ccall unsafe "sqlite3_bind_text"
  sqlite3_bind_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Parameter value (UTF-8).
    Ptr CChar ->
    -- | Length of parameter value in bytes.
    CInt ->
    Ptr (Ptr () -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/clear_bindings.html
foreign import ccall unsafe "sqlite3_clear_bindings"
  sqlite3_clear_bindings ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/close.html
foreign import ccall unsafe "sqlite3_close_v2"
  sqlite3_close_v2 :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import ccall unsafe "sqlite3_column_blob"
  sqlite3_column_blob ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr a)

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import ccall unsafe "sqlite3_column_bytes"
  sqlite3_column_bytes ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_count.html
foreign import ccall unsafe "sqlite3_column_count"
  sqlite3_column_count ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import ccall unsafe "sqlite3_column_double"
  sqlite3_column_double ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CDouble

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import ccall unsafe "sqlite3_column_int"
  sqlite3_column_int ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import ccall unsafe "sqlite3_column_text"
  sqlite3_column_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr CUChar)

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
foreign import ccall unsafe "sqlite3_extended_result_codes"
  sqlite3_extended_result_codes ::
    -- | Database.
    Ptr Sqlite3 ->
    CInt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/finalize.html
foreign import ccall unsafe "sqlite3_finalize"
  sqlite3_finalize ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/open.html
foreign import ccall unsafe "sqlite3_open_v2"
  sqlite3_open_v2 ::
    -- | Database file (UTF-8).
    Ptr CChar ->
    -- | Out: database.
    Ptr (Ptr Sqlite3) ->
    -- | Flags.
    CInt ->
    -- | VFS module name.
    Ptr CChar ->
    IO CInt

-- | https://www.sqlite.org/c3ref/prepare.html
foreign import ccall unsafe "sqlite3_prepare_v2"
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
foreign import ccall unsafe "sqlite3_reset"
  sqlite3_reset ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
foreign import ccall safe "sqlite3_step"
  sqlite3_step__safe ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

-- | https://www.sqlite.org/c3ref/step.html
foreign import ccall unsafe "sqlite3_step"
  sqlite3_step__unsafe ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt
