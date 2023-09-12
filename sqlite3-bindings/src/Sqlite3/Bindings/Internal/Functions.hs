{-# LANGUAGE CApiFFI #-}

module Sqlite3.Bindings.Internal.Functions where

import Control.Exception (mask_)
import Control.Monad (when)
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text (I8)
import Data.Traversable (for)
import Data.Word (Word64)
import Foreign.C (CChar (..), CDouble (..), CInt (..), CString, CUChar (..), CUInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtr, castPtrToFunPtr, freeHaskellFunPtr, minusPtr, nullFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr
import Foreign.Storable (Storable (peek), peekElemOff)
import Sqlite3.Bindings.C qualified as C
import Sqlite3.Bindings.Internal.Constants
import Sqlite3.Bindings.Internal.Objects
import Sqlite3.Bindings.Internal.Utils
  ( boolToCInt,
    carrayToArray,
    cdoubleToDouble,
    cintToBool,
    cintToInt,
    cstringLenToText,
    cstringToText,
    cuintToWord,
    doubleToCDouble,
    intToCInt,
    textToCString,
    textToCStringLen,
    wordToCUInt,
  )
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- | https://www.sqlite.org/c3ref/auto_extension.html
--
-- Register an extension that is automatically loaded by every new connection.
sqlite3_auto_extension ::
  -- | Extension entry point.
  FunPtr (Sqlite3 -> Ptr CString -> Sqlite3_api_routines -> IO CInt) ->
  -- | Result code.
  IO CInt
sqlite3_auto_extension callback =
  C.sqlite3_auto_extension (coerce @ExtensionEntryPoint @CExtensionEntryPoint callback)

type ExtensionEntryPoint = FunPtr (Sqlite3 -> Ptr CString -> Sqlite3_api_routines -> IO CInt)

type CExtensionEntryPoint = FunPtr (Ptr C.Sqlite3 -> Ptr CString -> Ptr C.Sqlite3_api_routines -> IO CInt)

-- | https://www.sqlite.org/c3ref/autovacuum_pages.html
--
-- Register a callback that is invoked prior to each autovacuum.
sqlite3_autovacuum_pages ::
  -- | Connection.
  Sqlite3 ->
  -- | Callback.
  (Maybe (Text -> Word -> Word -> Word -> IO Word)) ->
  -- | Result code.
  IO CInt
sqlite3_autovacuum_pages (Sqlite3 connection) = \case
  Nothing -> C.sqlite3_autovacuum_pages connection nullFunPtr nullPtr nullFunPtr
  Just callback ->
    mask_ do
      c_callback <-
        makeCallback6 \_ c_name numPages numFreePages pageSize -> do
          name <- cstringToText c_name
          wordToCUInt <$> callback name (cuintToWord numPages) (cuintToWord numFreePages) (cuintToWord pageSize)
      C.sqlite3_autovacuum_pages connection c_callback (castFunPtrToPtr c_callback) hs_free_fun_ptr

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Release a backup.
sqlite3_backup_finish ::
  -- | Backup.
  Sqlite3_backup ->
  -- | Result code.
  IO CInt
sqlite3_backup_finish =
  coerce C.sqlite3_backup_finish

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Initialize a backup.
sqlite3_backup_init ::
  -- | Destination connection.
  Sqlite3 ->
  -- | Destination database name.
  Text ->
  -- | Source connection.
  Sqlite3 ->
  -- | Source database name.
  Text ->
  -- | Backup, or error code on failure.
  IO (Either CInt Sqlite3_backup)
sqlite3_backup_init (Sqlite3 dstConnection) dstName (Sqlite3 srcConnection) srcName = do
  c_backup <-
    textToCString dstName \c_dstName ->
      textToCString srcName \c_srcName ->
        C.sqlite3_backup_init dstConnection c_dstName srcConnection c_srcName
  if c_backup == nullPtr
    then Left <$> sqlite3_errcode (Sqlite3 dstConnection)
    else pure (Right (Sqlite3_backup c_backup))

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Get the number of pages in the source database of a backup.
sqlite3_backup_pagecount ::
  -- | Backup.
  Sqlite3_backup ->
  -- | Number of pages.
  IO Int
sqlite3_backup_pagecount (Sqlite3_backup backup) =
  cintToInt <$> C.sqlite3_backup_pagecount backup

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Get the number of pages yet to be copied from the source database to the destination database of a backup.
sqlite3_backup_remaining ::
  -- | Backup.
  Sqlite3_backup ->
  -- | Number of pages.
  IO Int
sqlite3_backup_remaining (Sqlite3_backup backup) =
  cintToInt <$> C.sqlite3_backup_remaining backup

-- | https://www.sqlite.org/c3ref/backup_finish.html
--
-- Copy pages from the source database to the destination database of a backup.
sqlite3_backup_step ::
  -- | Backup.
  Sqlite3_backup ->
  -- | Number of pages to copy.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_backup_step (Sqlite3_backup backup) n =
  C.sqlite3_backup_step backup (intToCInt n)

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob to a parameter.
sqlite3_bind_blob ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Blob.
  ByteString ->
  -- | Result code.
  IO CInt
sqlite3_bind_blob (Sqlite3_stmt statement) index blob =
  ByteString.unsafeUseAsCStringLen blob \(c_blob, c_blob_len) ->
    C.sqlite3_bind_blob statement (intToCInt index) c_blob (intToCInt c_blob_len) C._SQLITE_TRANSIENT

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a double to a parameter.
sqlite3_bind_double ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Double.
  Double ->
  -- | Result code.
  IO CInt
sqlite3_bind_double (Sqlite3_stmt statement) index n =
  C.sqlite3_bind_double statement (intToCInt index) (doubleToCDouble n)

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
sqlite3_bind_int ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Integer.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_bind_int (Sqlite3_stmt statement) index n =
  C.sqlite3_bind_int statement (intToCInt index) (intToCInt n)

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind an integer to a parameter.
sqlite3_bind_int64 ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Integer.
  Int64 ->
  -- | Result code.
  IO CInt
sqlite3_bind_int64 (Sqlite3_stmt statement) index n =
  C.sqlite3_bind_int64 statement (intToCInt index) n

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter.
sqlite3_bind_null ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Result code.
  IO CInt
sqlite3_bind_null (Sqlite3_stmt statement) index =
  C.sqlite3_bind_null statement (intToCInt index)

-- | https://www.sqlite.org/c3ref/bind_parameter_count.html
--
-- Get the index of the largest parameter.
sqlite3_bind_parameter_count ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based), or 0 (no parameters).
  IO Int
sqlite3_bind_parameter_count (Sqlite3_stmt statement) =
  cintToInt <$> C.sqlite3_bind_parameter_count statement

-- | https://www.sqlite.org/c3ref/bind_parameter_index.html
--
-- Get the index of a named parameter.
sqlite3_bind_parameter_index ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter name.
  Text ->
  -- | Parameter index (1-based).
  IO (Maybe Int)
sqlite3_bind_parameter_index (Sqlite3_stmt statement) name = do
  index <-
    textToCString name \c_name -> do
      C.sqlite3_bind_parameter_index statement c_name
  pure if index == 0 then Nothing else Just (cintToInt index)

-- | https://www.sqlite.org/c3ref/bind_parameter_name.html
--
-- Get the name of a named parameter.
sqlite3_bind_parameter_name ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Parameter name.
  IO (Maybe Text)
sqlite3_bind_parameter_name (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_bind_parameter_name statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind null to a parameter, and associate it with a pointer.
sqlite3_bind_pointer ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Pointer.
  a ->
  -- | Pointer type.
  Text ->
  -- | Result code.
  IO CInt
sqlite3_bind_pointer (Sqlite3_stmt statement) index value typ =
  textToCString typ \c_typ ->
    mask_ do
      pointer <- newStablePtr value
      C.sqlite3_bind_pointer statement (intToCInt index) (castStablePtrToPtr pointer) c_typ freeStablePtrFunPtr

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a string to a parameter.
sqlite3_bind_text ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | String.
  Text ->
  -- | Result code.
  IO CInt
sqlite3_bind_text (Sqlite3_stmt statement) index string =
  textToCStringLen string \c_string c_string_len ->
    C.sqlite3_bind_text statement (intToCInt index) c_string (intToCInt c_string_len) C._SQLITE_TRANSIENT

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a value to a parameter.
sqlite3_bind_value ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Value.
  Sqlite3_value ->
  -- | Result code.
  IO CInt
sqlite3_bind_value (Sqlite3_stmt statement) index (Sqlite3_value value) =
  C.sqlite3_bind_value statement (intToCInt index) value

-- | https://www.sqlite.org/c3ref/bind_blob.html
--
-- Bind a blob of zeroes to a parameter.
sqlite3_bind_zeroblob ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Parameter index (1-based).
  Int ->
  -- | Size of blob, in bytes.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_bind_zeroblob (Sqlite3_stmt statement) index n =
  C.sqlite3_bind_zeroblob statement (intToCInt index) (intToCInt n)

-- | https://www.sqlite.org/c3ref/blob_bytes.html
--
-- Get the size of a blob, in bytes.
sqlite3_blob_bytes ::
  -- | Blob.
  Sqlite3_blob ->
  -- | Size of blob, in bytes.
  IO CInt
sqlite3_blob_bytes =
  coerce C.sqlite3_blob_bytes

-- | https://www.sqlite.org/c3ref/blob_close.html
--
-- Close a blob.
sqlite3_blob_close ::
  -- | Blob.
  Sqlite3_blob ->
  -- | Result code.
  IO CInt
sqlite3_blob_close =
  coerce C.sqlite3_blob_close

-- | https://www.sqlite.org/c3ref/blob_open.html
--
-- Open a blob.
sqlite3_blob_open ::
  -- | Connection.
  Sqlite3 ->
  -- | Database name.
  Text ->
  -- | Table name.
  Text ->
  -- | Column name.
  Text ->
  -- | Rowid.
  Int64 ->
  -- | Read-only (false) or read-write (true)
  Bool ->
  -- | Blob, result code.
  IO (Maybe Sqlite3_blob, CInt)
sqlite3_blob_open (Sqlite3 connection) database table column rowid mode = do
  (blob, code) <-
    textToCString database \c_database ->
      textToCString table \c_table ->
        textToCString column \c_column ->
          alloca \blobPtr -> do
            code <- C.sqlite3_blob_open connection c_database c_table c_column rowid (boolToCInt mode) blobPtr
            blob <- peek blobPtr
            pure (blob, code)
  pure (if blob == nullPtr then Nothing else Just (Sqlite3_blob blob), code)

-- | https://www.sqlite.org/c3ref/blob_read.html
--
-- Read data from a blob.
sqlite3_blob_read ::
  -- | Blob.
  Sqlite3_blob ->
  -- | Number of blob bytes to read.
  Int ->
  -- | Byte offset into blob to read from.
  Int ->
  -- | Bytes, or error code on failure.
  IO (Either CInt ByteString)
sqlite3_blob_read (Sqlite3_blob blob) len offset = do
  foreignPtr <- ByteString.mallocByteString len
  code <-
    withForeignPtr foreignPtr \ptr ->
      C.sqlite3_blob_read blob ptr (intToCInt len) (intToCInt offset)
  pure (if code == C._SQLITE_OK then Right (ByteString.BS foreignPtr len) else Left code)

-- | https://www.sqlite.org/c3ref/blob_reopen.html
--
-- Point an open blob at a different blob in the same table.
sqlite3_blob_reopen ::
  -- | Blob.
  Sqlite3_blob ->
  -- | Rowid.
  Int64 ->
  -- | Result code.
  IO CInt
sqlite3_blob_reopen =
  coerce C.sqlite3_blob_reopen

-- | https://www.sqlite.org/c3ref/blob_write.html
--
-- Write data to a blob.
sqlite3_blob_write ::
  -- | Blob.
  Sqlite3_blob ->
  -- | Data.
  ByteString ->
  -- | Byte offset into blob to write to.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_blob_write (Sqlite3_blob blob) bytes offset =
  ByteString.unsafeUseAsCStringLen bytes \(ptr, len) ->
    C.sqlite3_blob_write blob ptr (intToCInt len) (intToCInt offset)

-- | https://www.sqlite.org/c3ref/busy_handler.html
--
-- Register a callback that may be invoked when @SQLITE_BUSY@ would otherwise be returned from a function.
sqlite3_busy_handler ::
  -- | Connection.
  Sqlite3 ->
  -- | Callback.
  (Int -> IO Bool) ->
  -- | Result code, callback destructor.
  IO (CInt, IO ())
sqlite3_busy_handler (Sqlite3 connection) callback = do
  c_callback <- makeCallback2 \_ n -> boolToCInt <$> callback (cintToInt n)
  code <- C.sqlite3_busy_handler connection c_callback nullPtr
  pure (code, freeHaskellFunPtr c_callback)

-- | https://www.sqlite.org/c3ref/busy_timeout.html
--
-- Register a 'sqlite3_busy_handler' callback that sleeps.
sqlite3_busy_timeout ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of millseconds.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_busy_timeout (Sqlite3 connection) milliseconds =
  C.sqlite3_busy_timeout connection (intToCInt milliseconds)

-- | [__Cancel automatic extension loading__](https://www.sqlite.org/c3ref/cancel_auto_extension.html)
sqlite3_cancel_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/changes.html
--
-- Get the number of rows modified, inserted, or deleted by the most recent @UPDATE@, @INSERT@, or @DELETE@ statement on
-- a connection.
sqlite3_changes ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of rows.
  IO Int
sqlite3_changes (Sqlite3 connection) =
  cintToInt <$> C.sqlite3_changes connection

-- | https://www.sqlite.org/c3ref/changes.html
--
-- Get the number of rows modified, inserted, or deleted by the most recent @UPDATE@, @INSERT@, or @DELETE@ statement on
-- a connection.
sqlite3_changes64 ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of rows.
  IO Int64
sqlite3_changes64 =
  coerce C.sqlite3_changes64

-- | https://www.sqlite.org/c3ref/clear_bindings.html
--
-- Clear parameter bindings.
sqlite3_clear_bindings ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_clear_bindings =
  coerce C.sqlite3_clear_bindings

-- | https://www.sqlite.org/c3ref/close.html
--
-- Close a database connection.
sqlite3_close ::
  -- | Connection.
  Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_close (Sqlite3 connection) = do
  freeCommitHook connection
  freeRollbackHook connection
  C.sqlite3_close connection

-- | https://www.sqlite.org/c3ref/close.html
--
-- Attempt to close a database connection, but if it has any unfinalized statements, open blob handlers, or unfinished
-- backups, mark the connection as unusable and make arrangements to deallocate it after all statements are finalized,
-- blob handlers are closed, and backups are finished.
sqlite3_close_v2 ::
  -- | Connection.
  Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_close_v2 (Sqlite3 connection) = do
  freeCommitHook connection
  freeRollbackHook connection
  C.sqlite3_close_v2 connection

-- | https://www.sqlite.org/c3ref/collation_needed.html
--
-- Register a callback that is invoked when a collating sequence is needed.
sqlite3_collation_needed ::
  -- | Connection.
  Sqlite3 ->
  -- | Callback.
  (Text -> IO ()) ->
  -- | Result code, callback destructor.
  IO (CInt, IO ())
sqlite3_collation_needed (Sqlite3 connection) callback = do
  c_callback <-
    makeCallback7 \_ _ _ c_name -> do
      name <- cstringToText c_name
      callback name
  code <- C.sqlite3_collation_needed connection nullPtr c_callback
  pure (code, freeHaskellFunPtr c_callback)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the blob of a result column.
sqlite3_column_blob ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Blob.
  IO ByteString
sqlite3_column_blob statement@(Sqlite3_stmt c_statement) index = do
  -- Documentation recommends calling blob first, then bytes
  blob <- C.sqlite3_column_blob c_statement (intToCInt index)
  sqlite3_column_bytes statement index >>= \case
    0 -> pure ByteString.empty
    len -> ByteString.unsafePackCStringLen (blob, len)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the size of a blob or string result column, in bytes.
sqlite3_column_bytes ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Size, in bytes.
  IO Int
sqlite3_column_bytes (Sqlite3_stmt statement) index =
  cintToInt <$> C.sqlite3_column_bytes statement (intToCInt index)

-- | https://www.sqlite.org/c3ref/column_count.html
--
-- Get the number of columns in a result set.
sqlite3_column_count ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Number of columns.
  IO Int
sqlite3_column_count (Sqlite3_stmt statement) =
  cintToInt <$> C.sqlite3_column_count statement

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original database name for a result column.
sqlite3_column_database_name ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Database name.
  IO (Maybe Text)
sqlite3_column_database_name (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_column_database_name statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/column_decltype.html
--
-- Get the declared type of a result column.
sqlite3_column_decltype ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Type.
  IO (Maybe Text)
sqlite3_column_decltype (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_column_decltype statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the double of a result column.
sqlite3_column_double ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Double.
  IO Double
sqlite3_column_double (Sqlite3_stmt statement) index =
  cdoubleToDouble <$> C.sqlite3_column_double statement (intToCInt index)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
sqlite3_column_int ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Integer.
  IO Int
sqlite3_column_int (Sqlite3_stmt statement) index =
  cintToInt <$> C.sqlite3_column_int statement (intToCInt index)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the integer of a result column.
sqlite3_column_int64 ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Integer.
  IO Int64
sqlite3_column_int64 (Sqlite3_stmt statement) index =
  C.sqlite3_column_int64 statement (intToCInt index)

-- | https://www.sqlite.org/c3ref/column_name.html
--
-- Get the column name of a result column.
sqlite3_column_name ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Column name.
  IO (Maybe Text)
sqlite3_column_name (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_column_name statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original column name of a result column.
sqlite3_column_origin_name ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Column name.
  IO (Maybe Text)
sqlite3_column_origin_name (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_column_origin_name statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/column_database_name.html
--
-- Get the original table name for a result column.
sqlite3_column_table_name ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Table name.
  IO (Maybe Text)
sqlite3_column_table_name (Sqlite3_stmt statement) index = do
  c_name <- C.sqlite3_column_table_name statement (intToCInt index)
  if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the string of a result column.
sqlite3_column_text ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | String.
  IO Text
sqlite3_column_text statement@(Sqlite3_stmt c_statement) index = do
  -- Documentation recommends calling text first, then bytes
  string <- C.sqlite3_column_text c_statement (intToCInt index)
  sqlite3_column_bytes statement index >>= \case
    0 -> pure Text.empty
    len -> cstringLenToText (castPtr @CUChar @CChar string) (fromIntegral @Int @Text.I8 len)

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the type of a result column.
sqlite3_column_type ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Type.
  IO SQLITE_DATATYPE
sqlite3_column_type (Sqlite3_stmt statement) index =
  coerce (C.sqlite3_column_type statement (intToCInt index))

-- | https://www.sqlite.org/c3ref/column_blob.html
--
-- Get the unprotected value of a result column.
sqlite3_column_value ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Column index (0-based).
  Int ->
  -- | Value.
  IO Sqlite3_value
sqlite3_column_value (Sqlite3_stmt statement) index =
  coerce (C.sqlite3_column_value statement (intToCInt index))

-- | https://www.sqlite.org/c3ref/commit_hook.html
--
-- Register a callback that is invoked whenever a transaction is committed.
sqlite3_commit_hook ::
  -- | Connection.
  Sqlite3 ->
  -- | Commit hook.
  IO Int ->
  IO ()
sqlite3_commit_hook (Sqlite3 connection) hook = do
  mask_ do
    c_hook <- makeCallback1 \_ -> intToCInt <$> hook
    oldHook <- C.sqlite3_commit_hook connection c_hook (castFunPtrToPtr c_hook)
    freeAsFunPtrIfNonNull oldHook

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get a compile-time option name.
sqlite3_compileoption_get ::
  -- | Option index (0-based).
  Int ->
  -- | Option name.
  Maybe Text
sqlite3_compileoption_get index =
  unsafeDupablePerformIO (if c_name == nullPtr then pure Nothing else Just <$> cstringToText c_name)
  where
    c_name =
      C.sqlite3_compileoption_get (intToCInt index)

-- | https://www.sqlite.org/c3ref/compileoption_get.html
--
-- Get whether an option was specified at compile-time.
sqlite3_compileoption_used ::
  -- | Option name.
  Text ->
  Bool
sqlite3_compileoption_used name =
  unsafeDupablePerformIO do
    textToCString name \c_name ->
      pure (cintToBool (C.sqlite3_compileoption_used c_name))

-- | https://www.sqlite.org/c3ref/complete.html
--
-- Get whether an SQL statement is complete.
sqlite3_complete ::
  -- | SQL.
  Text ->
  Bool
sqlite3_complete sql =
  unsafeDupablePerformIO do
    textToCString sql \c_sql ->
      pure (cintToBool (C.sqlite3_complete c_sql))

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigheap
--
-- Specify memory that SQLite can use for dynamic allocations.
sqlite3_config_heap ::
  -- | Size of memory, in bytes.
  Int ->
  -- | Minimum allocation size, in bytes.
  Int ->
  -- | Result code.
  (CInt -> IO a) ->
  IO a
sqlite3_config_heap n1 n2 action =
  allocaBytesAligned n1 8 \ptr -> do
    code <- C.sqlite3_config_heap ptr (intToCInt n1) (intToCInt n2)
    action code

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiglog
--
-- Register a callback that is invoked whenever an anomaly occurs.
sqlite3_config_log ::
  -- | Logging function.
  Maybe (Int -> Text -> IO ()) ->
  -- | Result code, callback destructor.
  IO (CInt, IO ())
sqlite3_config_log = \case
  Nothing -> do
    code <- C.sqlite3_config_log nullFunPtr nullPtr
    pure (code, pure ())
  Just callback -> do
    c_callback <-
      makeCallback3 \_ code c_msg -> do
        msg <- cstringToText c_msg
        callback (cintToInt code) msg
    code <- C.sqlite3_config_log c_callback nullPtr
    pure (code, freeHaskellFunPtr c_callback)

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfiglookaside
--
-- Set the default lookaside memory size.
sqlite3_config_lookaside ::
  -- | Size of each lookaside buffer slot, in bytes.
  Int ->
  -- | Number of lookaside buffer slots allocated to each connection.
  Int ->
  -- | Result code.
  IO CInt
sqlite3_config_lookaside n1 n2 =
  C.sqlite3_config_lookaside (intToCInt n1) (intToCInt n2)

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmemstatus
--
-- Set whether memory allocation statistics are collected.
sqlite3_config_memstatus ::
  Bool ->
  -- | Result code.
  IO CInt
sqlite3_config_memstatus b =
  C.sqlite3_config_memstatus (boolToCInt b)

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigmultithread
--
-- Set the threading mode to multi-thread.
sqlite3_config_multithread ::
  -- | Result code.
  IO CInt
sqlite3_config_multithread =
  C.sqlite3_config_multithread

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigpagecache
--
-- Specify memory that SQLite can use for the page cache.
sqlite3_config_pagecache ::
  -- | Size of each page cache line, in bytes.
  Int ->
  -- | Number of cache lines.
  Int ->
  -- | Result code.
  (CInt -> IO a) ->
  IO a
sqlite3_config_pagecache n1 n2 action =
  allocaBytesAligned (n1 * n2) 8 \ptr -> do
    code <- C.sqlite3_config_pagecache ptr (intToCInt n1) (intToCInt n2)
    action code

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigserialized
--
-- Set the threading mode to serialized.
sqlite3_config_serialized ::
  -- | Result code.
  IO CInt
sqlite3_config_serialized =
  C.sqlite3_config_serialized

-- | https://www.sqlite.org/c3ref/c_config_covering_index_scan.html#sqliteconfigsinglethread
--
-- Set the threading mode to single-thread.
sqlite3_config_singlethread ::
  -- | Result code.
  IO CInt
sqlite3_config_singlethread =
  C.sqlite3_config_singlethread

-- | https://www.sqlite.org/c3ref/context_db_handle.html
--
-- Get the connection for a function.
sqlite3_context_db_handle ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Connection.
  IO (Ptr C.Sqlite3)
sqlite3_context_db_handle =
  C.sqlite3_context_db_handle

-- | https://www.sqlite.org/c3ref/create_collation.html
--
-- Create a collating sequence.
sqlite3_create_collation ::
  -- | Connection.
  Sqlite3 ->
  -- | Collating sequence name.
  Text ->
  -- | Collating sequence.
  Maybe (Text -> Text -> Ordering) ->
  -- | Result code.
  IO CInt
sqlite3_create_collation (Sqlite3 connection) name maybeComparison =
  textToCString name \c_name -> do
    let createCollation = C.sqlite3_create_collation_v2 connection c_name _SQLITE_UTF8
    case maybeComparison of
      Nothing -> createCollation nullPtr nullFunPtr nullFunPtr
      Just comparison ->
        mask_ do
          c_comparison <-
            makeCallback4 \_ lx cx ly cy -> do
              x <- cstringLenToText cx (fromIntegral @CInt @Text.I8 lx)
              y <- cstringLenToText cy (fromIntegral @CInt @Text.I8 ly)
              pure case comparison x y of
                LT -> -1
                EQ -> 0
                GT -> 1
          code <- createCollation (castFunPtrToPtr c_comparison) c_comparison hs_free_fun_ptr
          when (code /= _SQLITE_OK) (freeHaskellFunPtr c_comparison)
          pure code

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
  C.sqlite3_create_filename

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create a function or aggregate function.
sqlite3_create_function ::
  -- | Connection.
  Sqlite3 ->
  -- | Function name (UTF-8).
  Text ->
  -- | Number of function arguments.
  Int ->
  -- | Encoding and flags.
  CInt ->
  -- | Function.
  (Sqlite3_context -> Array Int Sqlite3_value -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_function (Sqlite3 connection) name nargs flags func =
  textToCString name \c_name ->
    mask_ do
      c_func <-
        makeCallback8 \c_context argc argv -> do
          let maxIndex = cintToInt argc - 1
          args <-
            for [0 .. maxIndex] \i -> do
              value <- peekElemOff argv i
              pure (i, Sqlite3_value value)
          func (Sqlite3_context c_context) (Array.array (0, maxIndex) args)
      C.sqlite3_create_function_v2
        connection
        c_name
        (intToCInt nargs)
        flags
        (castFunPtrToPtr c_func)
        c_func
        nullFunPtr
        nullFunPtr
        hs_free_fun_ptr

-- | TODO
sqlite3_create_aggregate_function ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of function arguments.
  CInt ->
  -- | Encoding and flags.
  CInt ->
  -- | Aggregate function step.
  FunPtr (Ptr C.Sqlite3_context -> CInt -> Ptr (Ptr C.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function finalize.
  FunPtr (Ptr C.Sqlite3_context -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_aggregate_function = undefined

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Delete a function or aggregate function.
--
-- TODO
sqlite3_delete_function ::
  -- | Connection.
  Sqlite3 ->
  -- | Function name (UTF-8).
  Text ->
  -- | Number of function arguments.
  Int ->
  -- | Encoding and flags.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_delete_function (Sqlite3 connection) name nargs flags =
  textToCString name \c_name ->
    C.sqlite3_create_function_v2
      connection
      c_name
      (intToCInt nargs)
      flags
      nullPtr
      nullFunPtr
      nullFunPtr
      nullFunPtr
      nullFunPtr

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
sqlite3_create_module ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Module name (UTF-8).
  CString ->
  -- | Module.
  Ptr (C.Sqlite3_module a) ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_create_module =
  C.sqlite3_create_module

-- | https://www.sqlite.org/c3ref/create_module.html
--
-- Create a virtual table module.
sqlite3_create_module_v2 ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Module name (UTF-8).
  CString ->
  -- | Module.
  Ptr (C.Sqlite3_module a) ->
  -- | Application data.
  Ptr a ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_module_v2 =
  C.sqlite3_create_module_v2

-- | https://www.sqlite.org/c3ref/create_function.html
--
-- Create an aggregate window function.
sqlite3_create_window_function ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of function arguments.
  CInt ->
  -- | Flags.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Aggregate function step.
  FunPtr (Ptr C.Sqlite3_context -> CInt -> Ptr (Ptr C.Sqlite3_value) -> IO ()) ->
  -- | Aggregate function finalize.
  FunPtr (Ptr C.Sqlite3_context -> IO ()) ->
  -- | Aggregate window function get current value.
  FunPtr (Ptr C.Sqlite3_context -> IO ()) ->
  -- | Aggregate window function remove value.
  FunPtr (Ptr C.Sqlite3_context -> CInt -> Ptr (Ptr C.Sqlite3_value) -> IO ()) ->
  -- | Application data destructor.
  FunPtr (Ptr a -> IO ()) ->
  -- | Result code.
  IO CInt
sqlite3_create_window_function =
  C.sqlite3_create_window_function

-- | https://www.sqlite.org/c3ref/data_count.html
--
-- Get the number of columns in the next row of a result set.
sqlite3_data_count ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | Number of columns.
  IO CInt
sqlite3_data_count =
  C.sqlite3_data_count

-- | https://www.sqlite.org/c3ref/database_file_object.html
--
-- Get the database file object for a journal file.
sqlite3_database_file_object ::
  -- | Journal file (UTF-8).
  CString ->
  -- | Database file object.
  IO (Ptr Sqlite3_file)
sqlite3_database_file_object =
  C.sqlite3_database_file_object

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
--
-- Flush all databases' dirty pager-cache pages to disk.
sqlite3_db_cacheflush ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_db_cacheflush =
  C.sqlite3_db_cacheflush

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
sqlite3_db_config__1 :: Ptr C.Sqlite3 -> CInt -> CString -> IO CInt
sqlite3_db_config__1 =
  C.sqlite3_db_config__1

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
sqlite3_db_config__2 :: Ptr C.Sqlite3 -> CInt -> Ptr a -> CInt -> CInt -> IO CInt
sqlite3_db_config__2 =
  C.sqlite3_db_config__2

-- | https://www.sqlite.org/c3ref/db_config.html
--
-- Configure a connection.
sqlite3_db_config__3 :: Ptr C.Sqlite3 -> CInt -> CInt -> Ptr CInt -> IO CInt
sqlite3_db_config__3 =
  C.sqlite3_db_config__3

-- | https://www.sqlite.org/c3ref/db_filename.html
--
-- Get the filename for a database.
sqlite3_db_filename ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Filename (UTF-8).
  IO CString
sqlite3_db_filename =
  C.sqlite3_db_filename

-- | https://www.sqlite.org/c3ref/db_handle.html
--
-- Get the connection for a statement.
sqlite3_db_handle ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | Connection.
  Ptr C.Sqlite3
sqlite3_db_handle =
  C.sqlite3_db_handle

-- | https://www.sqlite.org/c3ref/db_mutex.html
--
-- Get the mutex of a connection.
sqlite3_db_mutex ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Mutex.
  IO (Ptr C.Sqlite3_mutex)
sqlite3_db_mutex =
  C.sqlite3_db_mutex

-- | https://www.sqlite.org/c3ref/db_name.html
--
-- Get the name of a database.
sqlite3_db_name ::
  -- | Connection
  Ptr C.Sqlite3 ->
  -- | Database index (0-based; 0 is the main database file).
  CInt ->
  -- | Database name (UTF-8).
  IO CString
sqlite3_db_name =
  C.sqlite3_db_name

-- | https://www.sqlite.org/c3ref/db_readonly.html
--
-- Get whether a database is read-only.
sqlite3_db_readonly ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | @-1@ (not attached), @0@ (not read-only), or @1@ (read-only).
  IO CInt
sqlite3_db_readonly =
  C.sqlite3_db_readonly

-- | https://www.sqlite.org/c3ref/db_release_memory.html
--
-- Release as much memory as possible from a connection.
sqlite3_db_release_memory ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_db_release_memory =
  C.sqlite3_db_release_memory

-- | https://www.sqlite.org/c3ref/db_status.html
--
-- Get a connection status value.
sqlite3_db_status ::
  -- | Connection.
  Ptr C.Sqlite3 ->
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
  C.sqlite3_db_status

-- | https://www.sqlite.org/c3ref/declare_vtab.html
--
-- Declare the schema of a virtual table.
sqlite3_declare_vtab ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Schema (UTF-8).
  CString ->
  -- | Result code.
  IO CInt
sqlite3_declare_vtab =
  C.sqlite3_declare_vtab

-- | https://www.sqlite.org/c3ref/deserialize.html
--
-- Deserialize a database.
sqlite3_deserialize ::
  -- | Connection.
  Ptr C.Sqlite3 ->
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
sqlite3_deserialize =
  C.sqlite3_deserialize

-- | https://www.sqlite.org/c3ref/drop_modules.html
--
-- Remove virtual table modules.
sqlite3_drop_modules ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Names of virtual table modules to keep (UTF-8).
  Ptr CString ->
  -- | Result code.
  IO CInt
sqlite3_drop_modules =
  C.sqlite3_drop_modules

-- | https://www.sqlite.org/c3ref/exec.html
--
-- Execute zero or more SQL statements separated by semicolons.
sqlite3_exec ::
  -- | Connection.
  Sqlite3 ->
  -- | SQL.
  Text ->
  -- | Callback.
  Maybe (Array Int Text -> Array Int Text -> IO Int) ->
  -- | Error message, result code.
  IO (Maybe Text, CInt)
sqlite3_exec (Sqlite3 connection) sql maybeCallback =
  mask_ do
    case maybeCallback of
      Nothing -> go nullFunPtr
      Just callback -> do
        c_callback <-
          makeCallback5 \_ numCols c_values c_names -> do
            let convert = carrayToArray cstringToText numCols
            values <- convert c_values
            names <- convert c_names
            intToCInt <$> callback values names
        result <- go c_callback
        freeHaskellFunPtr c_callback
        pure result
  where
    go :: FunPtr (Ptr a -> CInt -> Ptr CString -> Ptr CString -> IO CInt) -> IO (Maybe Text, CInt)
    go c_callback =
      textToCString sql \c_sql ->
        alloca \errorMessagePtr -> do
          code <- C.sqlite3_exec connection c_sql c_callback nullPtr errorMessagePtr
          c_error_message <- peek errorMessagePtr
          if c_error_message == nullPtr
            then pure (Nothing, code)
            else do
              errorMessage <- cstringToText c_error_message
              sqlite3_free c_error_message
              pure (Just errorMessage, code)

-- | https://www.sqlite.org/c3ref/extended_result_codes.html
--
-- Set whether to return extended result codes.
sqlite3_extended_result_codes ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | @0@ or @1@.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_extended_result_codes =
  C.sqlite3_extended_result_codes

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the result code of the most recent failure on a connection.
sqlite3_errcode ::
  -- | Connection.
  Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_errcode =
  coerce C.sqlite3_errcode

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the error message of the most recent failure on a connection.
sqlite3_errmsg ::
  -- | Connection.
  Sqlite3 ->
  -- | Error message.
  IO Text
sqlite3_errmsg (Sqlite3 connection) = do
  c_string <- C.sqlite3_errmsg connection
  cstringToText c_string

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the byte offset into the SQL that the most recent failure on a connection refers to.
sqlite3_error_offset ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Byte offset, or @-1@ if not applicable.
  IO CInt
sqlite3_error_offset =
  C.sqlite3_error_offset

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the error message of a result code.
sqlite3_errstr ::
  -- | Result code.
  CInt ->
  -- | Error message.
  Text
sqlite3_errstr code =
  unsafeDupablePerformIO (cstringToText (C.sqlite3_errstr code))

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the expanded SQL of a statement.
sqlite3_expanded_sql ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_expanded_sql =
  C.sqlite3_expanded_sql

-- | https://www.sqlite.org/c3ref/errcode.html
--
-- Get the extended result code of the most recent failure on a connection.
sqlite3_extended_errcode ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_extended_errcode =
  C.sqlite3_extended_errcode

-- | https://www.sqlite.org/c3ref/file_control.html
--
-- Call @xFileControl@.
sqlite3_file_control ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Opcode.
  CInt ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_file_control =
  C.sqlite3_file_control

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the database file for a database file, journal file, or WAL file.
sqlite3_filename_database ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | Database file.
  IO CString
sqlite3_filename_database =
  C.sqlite3_filename_database

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the journal file for a database file, journal file, or WAL file.
sqlite3_filename_journal ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | Journal file.
  IO CString
sqlite3_filename_journal =
  C.sqlite3_filename_journal

-- | https://www.sqlite.org/c3ref/filename_database.html
--
-- Get the WAL file for a database file, journal file, or WAL file.
sqlite3_filename_wal ::
  -- | Database file, journal file, or WAL file.
  CString ->
  -- | WAL file.
  IO CString
sqlite3_filename_wal =
  C.sqlite3_filename_wal

-- | https://www.sqlite.org/c3ref/finalize.html
--
-- Release a statement.
sqlite3_finalize ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_finalize =
  coerce C.sqlite3_finalize

-- | https://www.sqlite.org/c3ref/free.html
--
-- Release memory acquired by 'sqlite3_malloc', 'sqlite3_malloc64', 'sqlite3_realloc', or 'sqlite3_realloc64'.
sqlite3_free ::
  -- | Memory.
  Ptr a ->
  IO ()
sqlite3_free =
  C.sqlite3_free

-- | https://www.sqlite.org/c3ref/create_filename.html
--
-- Release a VFS filename.
sqlite3_free_filename ::
  -- | Filename (UTF-8).
  CString ->
  IO ()
sqlite3_free_filename =
  C.sqlite3_free_filename

-- | https://www.sqlite.org/c3ref/get_autocommit.html
--
-- Get whether a connection is in autocommit mode.
sqlite3_get_autocommit ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_get_autocommit =
  C.sqlite3_get_autocommit

-- | https://www.sqlite.org/c3ref/get_auxdata.html
--
-- Get the metadata of a function argument.
sqlite3_get_auxdata ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Argument index (0-based).
  CInt ->
  -- | Metadata.
  IO (Ptr a)
sqlite3_get_auxdata =
  C.sqlite3_get_auxdata

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
sqlite3_hard_heap_limit64 ::
  -- | Limit, in bytes, or a negative number to get the limit.
  Int64 ->
  -- | Previous limit, in bytes.
  IO Int64
sqlite3_hard_heap_limit64 =
  C.sqlite3_hard_heap_limit64

-- | https://www.sqlite.org/c3ref/initialize.html
--
-- Initialize the library.
sqlite3_initialize ::
  -- | Result code.
  IO CInt
sqlite3_initialize =
  C.sqlite3_initialize

-- | https://www.sqlite.org/c3ref/interrupt.html
--
-- Cause all in-progress operations to return `SQLITE_INTERRUPT` at the earliest opportunity.
sqlite3_interrupt ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  IO ()
sqlite3_interrupt =
  C.sqlite3_interrupt

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- Get whether a string is a keyword.
sqlite3_keyword_check ::
  -- | String (UTF-8).
  Ptr CChar ->
  -- | Size of string, in bytes.
  CInt ->
  -- | @0@ or @1@.
  CInt
sqlite3_keyword_check =
  C.sqlite3_keyword_check

-- | https://www.sqlite.org/c3ref/keyword_check.html
--
-- The number of distinct keywords.
sqlite3_keyword_count :: CInt
sqlite3_keyword_count =
  C.sqlite3_keyword_count

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
  C.sqlite3_keyword_name

-- | https://www.sqlite.org/c3ref/last_insert_rowid.html
--
-- Get the rowid of the most recent insert into a rowid table.
sqlite3_last_insert_rowid ::
  -- | Connection.
  Sqlite3 ->
  -- | Rowid.
  IO Int64
sqlite3_last_insert_rowid =
  coerce C.sqlite3_last_insert_rowid

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
sqlite3_libversion :: Text
sqlite3_libversion =
  unsafeDupablePerformIO (cstringToText C.sqlite3_libversion)

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The library version.
sqlite3_libversion_number :: Int
sqlite3_libversion_number =
  cintToInt C.sqlite3_libversion_number

-- | https://www.sqlite.org/c3ref/limit.html
--
-- Get or set a limit on a connection.
sqlite3_limit ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Limit category.
  CInt ->
  -- | Limit, or a negative number to get the limit.
  CInt ->
  -- | Previous limit.
  IO CInt
sqlite3_limit =
  C.sqlite3_limit

-- | https://www.sqlite.org/c3ref/load_extension.html
sqlite3_load_extension ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Extension shared library name.
  CString ->
  -- | Entry point.
  CString ->
  -- | /Out/: error message.
  Ptr CString ->
  -- | Result code.
  IO CInt
sqlite3_load_extension =
  C.sqlite3_load_extension

-- | https://www.sqlite.org/c3ref/log.html
--
-- Write a message to the error log.
sqlite3_log ::
  -- | Result code.
  CInt ->
  -- | Error message.
  CString ->
  IO ()
sqlite3_log =
  C.sqlite3_log

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
sqlite3_malloc ::
  -- | Size of memory, in bytes.
  CInt ->
  -- | Memory.
  IO (Ptr a)
sqlite3_malloc =
  C.sqlite3_malloc

-- | https://www.sqlite.org/c3ref/free.html
--
-- Allocate memory.
sqlite3_malloc64 ::
  -- | Size of memory, in bytes.
  Word64 ->
  -- | Memory.
  IO (Ptr a)
sqlite3_malloc64 =
  C.sqlite3_malloc64

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the highest value of 'sqlite3_memory_used'.
sqlite3_memory_highwater ::
  -- | Reset highest value? (@0@ or @1@).
  CInt ->
  -- | Highest value (prior to this reset, if this is a reset).
  IO Int64
sqlite3_memory_highwater =
  C.sqlite3_memory_highwater

-- | https://www.sqlite.org/c3ref/memory_highwater.html
--
-- Get the size of live memory, in bytes.
sqlite3_memory_used ::
  -- | Size of memory, in bytes.
  IO Int64
sqlite3_memory_used =
  C.sqlite3_memory_used

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
  C.sqlite3_msize

-- sqlite3_mutex_notheld = undefined

-- | https://www.sqlite.org/c3ref/next_stmt.html
--
-- Get the next statement of a connection.
sqlite3_next_stmt ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | Next statement.
  IO (Ptr C.Sqlite3_stmt)
sqlite3_next_stmt =
  C.sqlite3_next_stmt

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the normalized SQL of a statement.
sqlite3_normalized_sql ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_normalized_sql =
  coerce C.sqlite3_normalized_sql

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
      code <- C.sqlite3_open c_database connectionPtr
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
        code <- C.sqlite3_open_v2 c_database connectionPtr (mode .|. flags) c_vfs
        connection <- peek connectionPtr
        pure (if connection == nullPtr then Nothing else Just (Sqlite3 connection), code)
  where
    withVfs :: (CString -> IO a) -> IO a
    withVfs k =
      case maybeVfs of
        Nothing -> k nullPtr
        Just vfs -> textToCString vfs k

-- | https://www.sqlite.org/c3ref/overload_function.html
--
-- Ensure a placeholder function exists, to be overloaded by @xFindFunction@.
sqlite3_overload_function ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Function name (UTF-8).
  CString ->
  -- | Number of arguments.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_overload_function =
  C.sqlite3_overload_function

-- | https://www.sqlite.org/c3ref/prepare.html
--
-- Compile a statement.
--
-- Unlike the underlying SQLite library, this library returns @SQLITE_ERROR@ in both of the following cases:
--
--     * Trying to compile an empty statement, as in the strings @""@ or @"-- comment"@.
--     * Trying to compile a statement with trailing syntax, as in the string @"select 1; select 2"@
sqlite3_prepare_v2 ::
  -- | Connection.
  Sqlite3 ->
  -- | SQL.
  Text ->
  -- | Statement, or error code on failure.
  IO (Either CInt Sqlite3_stmt)
sqlite3_prepare_v2 =
  prepare_ C.sqlite3_prepare_v2

-- | https://www.sqlite.org/c3ref/prepare.html
--
-- Compile a statement.
--
-- Unlike the underlying SQLite library, this library returns @SQLITE_ERROR@ in both of the following cases:
--
--     * Trying to compile an empty statement, as in the strings @""@ or @"-- comment"@.
--     * Trying to compile a statement with trailing syntax, as in the string @"select 1; select 2"@
sqlite3_prepare_v3 ::
  -- | Connection.
  Sqlite3 ->
  -- | SQL.
  Text ->
  -- | Flags.
  SQLITE_PREPARE_FLAGS ->
  -- | Statement, or error code on failure.
  IO (Either CInt Sqlite3_stmt)
sqlite3_prepare_v3 connection sql (SQLITE_PREPARE_FLAGS flags) =
  prepare_ (\a b c -> C.sqlite3_prepare_v3 a b c flags) connection sql

-- Shared helper of sqlite_prepare_v2 and sqlite_prepare_v3
prepare_ ::
  (Ptr C.Sqlite3 -> Ptr CChar -> CInt -> Ptr (Ptr C.Sqlite3_stmt) -> Ptr (Ptr CChar) -> IO CInt) ->
  Sqlite3 ->
  Text ->
  IO (Either CInt Sqlite3_stmt)
prepare_ doPrepare (Sqlite3 connection) sql =
  textToCStringLen sql \c_sql c_sql_len ->
    alloca \statementPtr ->
      alloca \unusedSqlPtr -> do
        code <- doPrepare connection c_sql (intToCInt c_sql_len) statementPtr unusedSqlPtr
        statement <- peek statementPtr
        c_unused_sql <- peek unusedSqlPtr
        let unusedSqlLen = (c_sql `plusPtr` c_sql_len) `minusPtr` c_unused_sql
        if statement == nullPtr
          then pure (Left (if code == _SQLITE_OK then _SQLITE_ERROR else code))
          else
            if unusedSqlLen > 0
              then do
                void (C.sqlite3_finalize statement)
                pure (Left _SQLITE_ERROR)
              else pure (Right (Sqlite3_stmt statement))
{-# INLINE prepare_ #-}

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the index of the column being written with 'sqlite3_blob_write'.
sqlite3_preupdate_blobwrite ::
  -- | Connection.
  Sqlite3 ->
  -- | Column index, or @-1@ on failure.
  IO CInt
sqlite3_preupdate_blobwrite =
  coerce C.sqlite3_preupdate_blobwrite

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the number of columns in the row being updated.
sqlite3_preupdate_count ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of columns.
  IO CInt
sqlite3_preupdate_count =
  coerce C.sqlite3_preupdate_count

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
--
-- Get the "trigger depth" of this update, i.e. @0@ for a direct update, or @n@ for an update caused by a trigger that
-- was invoked by a depth @n-1@ update.
sqlite3_preupdate_depth ::
  -- | Connection.
  Sqlite3 ->
  -- | Trigger depth.
  IO CInt
sqlite3_preupdate_depth =
  coerce C.sqlite3_preupdate_depth

-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- sqlite3_preupdate_hook ::
--   Ptr C.Sqlite3 ->
--   FunPtr (Ptr a -> Ptr C.Sqlite3 -> CInt -> CString -> CString -> Int64 -> Int64 -> IO ()) ->
--   Ptr a ->
--   IO (Ptr b)
-- sqlite3_preupdate_hook =
--   C.sqlite3_preupdate_hook

-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- sqlite3_preupdate_new ::
--   Ptr C.Sqlite3 ->
--   CInt ->
--   Ptr (Ptr C.Sqlite3_value) ->
--   IO CInt
-- sqlite3_preupdate_new =
--   C.sqlite3_preupdate_new

-- -- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
-- sqlite3_preupdate_old ::
--   Ptr C.Sqlite3 ->
--   CInt ->
--   Ptr (Ptr C.Sqlite3_value) ->
--   IO CInt
-- sqlite3_preupdate_old =
--   C.sqlite3_preupdate_old

-- | https://www.sqlite.org/c3ref/progress_handler.html
--
-- Register a callback that is invoked periodically during long-running queries.
sqlite3_progress_handler ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Approximate number of virtual machine instructions that are evaluated between successive invocations of the
  -- callback.
  CInt ->
  -- | Callback.
  FunPtr (Ptr a -> IO CInt) ->
  -- | Application data.
  Ptr a ->
  IO ()
sqlite3_progress_handler =
  C.sqlite3_progress_handler

-- | https://www.sqlite.org/c3ref/randomness.html
--
-- Generate random bytes.
sqlite3_randomness ::
  -- | Number of bytes to generate.
  CInt ->
  -- | /Out/: buffer.
  Ptr a ->
  IO ()
sqlite3_randomness =
  C.sqlite3_randomness

-- | https://www.sqlite.org/c3ref/free.html
--
-- Resize a memory allocation.
sqlite3_realloc ::
  -- | Memory.
  Ptr a ->
  -- | Size of memory, in bytes.
  CInt ->
  -- | Memory.
  IO (Ptr a)
sqlite3_realloc =
  C.sqlite3_realloc

-- | https://www.sqlite.org/c3ref/free.html
--
-- Resize a memory allocation.
sqlite3_realloc64 ::
  -- | Memory.
  Ptr a ->
  -- | Size of memory, in bytes.
  Int64 ->
  -- | Memory.
  IO (Ptr a)
sqlite3_realloc64 =
  C.sqlite3_realloc64

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
  C.sqlite3_release_memory

-- | https://www.sqlite.org/c3ref/reset.html
--
-- Reset a statement to its initial state. Does not clear parameter bindings.
sqlite3_reset ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_reset =
  coerce C.sqlite3_reset

-- | https://www.sqlite.org/c3ref/reset_auto_extension.html
sqlite3_reset_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob from a function.
sqlite3_result_blob ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Blob.
  Ptr a ->
  -- | Size of blob, in bytes.
  CInt ->
  -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_blob =
  C.sqlite3_result_blob

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob from a function.
sqlite3_result_blob64 ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Blob.
  Ptr a ->
  -- | Size of blob, in bytes.
  Word64 ->
  -- | Blob destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_blob64 =
  C.sqlite3_result_blob64

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a double from a function.
sqlite3_result_double ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Double.
  CDouble ->
  IO ()
sqlite3_result_double =
  C.sqlite3_result_double

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw an exception from a function.
sqlite3_result_error ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Error message (UTF-8).
  CString ->
  -- | Size of error message, in bytes, or @-1@ to use the entire message.
  CInt ->
  IO ()
sqlite3_result_error =
  C.sqlite3_result_error

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw an exception from a function.
sqlite3_result_error_code ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Result code.
  CInt ->
  IO ()
sqlite3_result_error_code =
  C.sqlite3_result_error_code

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw a @SQLITE_NOMEM@ exception from a function.
sqlite3_result_error_nomem ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  IO ()
sqlite3_result_error_nomem =
  C.sqlite3_result_error_nomem

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Throw a @SQLITE_TOOBIG@ exception from a function.
sqlite3_result_error_toobig ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  IO ()
sqlite3_result_error_toobig =
  C.sqlite3_result_error_toobig

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return an integer from a function.
sqlite3_result_int ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Integer.
  CInt ->
  IO ()
sqlite3_result_int =
  C.sqlite3_result_int

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return an integer from a function.
sqlite3_result_int64 ::
  -- | Function context.
  Sqlite3_context ->
  -- | Integer.
  Int64 ->
  IO ()
sqlite3_result_int64 =
  coerce C.sqlite3_result_int64

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return null from a function.
sqlite3_result_null ::
  -- | Function context.
  Sqlite3_context ->
  IO ()
sqlite3_result_null =
  coerce C.sqlite3_result_null

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return null from a function, and associate it with a pointer.
sqlite3_result_pointer ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Pointer.
  Ptr a ->
  -- | Pointer type.
  CString ->
  -- | Pointer destructor.
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_result_pointer =
  C.sqlite3_result_pointer

-- | https://www.sqlite.org/c3ref/result_subtype.html
--
-- Set the subtype of the return value of a function.
sqlite3_result_subtype ::
  -- | Function context.
  Sqlite3_context ->
  -- | Subtype.
  CUInt ->
  IO ()
sqlite3_result_subtype =
  coerce C.sqlite3_result_subtype

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a string from a function.
sqlite3_result_text ::
  -- | Function context.
  Sqlite3_context ->
  -- | String.
  Text ->
  IO ()
sqlite3_result_text (Sqlite3_context context) string =
  textToCStringLen string \c_string len ->
    C.sqlite3_result_text context c_string (intToCInt len) C._SQLITE_TRANSIENT

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a string from a function.
sqlite3_result_text64 ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | String (UTF-8).
  Ptr CChar ->
  -- | Size of string, in bytes.
  Word64 ->
  -- | String destructor, @SQLITE_STATIC@, or @SQLITE_TRANSIENT@.
  FunPtr (Ptr a -> IO ()) ->
  -- | Encoding.
  CUChar ->
  IO ()
sqlite3_result_text64 =
  C.sqlite3_result_text64

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a value from a function.
sqlite3_result_value ::
  -- | Function context.
  Sqlite3_context ->
  -- | Value.
  Sqlite3_value ->
  IO ()
sqlite3_result_value =
  coerce C.sqlite3_result_value

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob of zeroes from a function.
sqlite3_result_zeroblob ::
  -- | Function context.
  Sqlite3_context ->
  -- | Size of blob, in bytes.
  Int ->
  IO ()
sqlite3_result_zeroblob (Sqlite3_context context) size =
  C.sqlite3_result_zeroblob context (intToCInt size)

-- | https://www.sqlite.org/c3ref/result_blob.html
--
-- Return a blob of zeroes from a function.
sqlite3_result_zeroblob64 ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Size of blob, in bytes.
  Word64 ->
  -- | Result code.
  IO CInt
sqlite3_result_zeroblob64 =
  C.sqlite3_result_zeroblob64

-- | https://www.sqlite.org/c3ref/commit_hook.html
--
-- Register a callback that is invoked whenever a transaction is committed.
sqlite3_rollback_hook ::
  -- | Connection.
  Sqlite3 ->
  -- | Rollback hook.
  IO Int ->
  IO ()
sqlite3_rollback_hook (Sqlite3 connection) hook =
  mask_ do
    c_hook <- makeCallback1 \_ -> intToCInt <$> hook
    oldHook <- C.sqlite3_rollback_hook connection c_hook (castFunPtrToPtr c_hook)
    freeAsFunPtrIfNonNull oldHook

-- | https://www.sqlite.org/c3ref/serialize.html
--
-- Serialize a database.
sqlite3_serialize ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | /Out/: size of database, in bytes.
  Ptr Int64 ->
  -- | Flags.
  CUInt ->
  -- | Serialized database.
  IO (Ptr CUChar)
sqlite3_serialize =
  C.sqlite3_serialize

-- | https://www.sqlite.org/c3ref/set_authorizer.html
--
-- Register a callback that is invoked during statement preparation to authorize actions.
sqlite3_set_authorizer ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Callback.
  FunPtr (Ptr a -> CInt -> CString -> CString -> CString -> CString -> IO CInt) ->
  -- | Application data.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_set_authorizer =
  C.sqlite3_set_authorizer

-- | https://www.sqlite.org/c3ref/get_auxdata.html
--
-- Set the metadata of a function argument.
sqlite3_set_auxdata ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | Argument index (0-based).
  CInt ->
  -- | Metadata.
  Ptr a ->
  -- | Metadata destructor.
  FunPtr (Ptr a -> IO ()) ->
  IO ()
sqlite3_set_auxdata =
  C.sqlite3_set_auxdata

-- | https://www.sqlite.org/c3ref/set_last_insert_rowid.html
--
-- Set the return value of the next 'sqlite3_last_insert_rowid'.
sqlite3_set_last_insert_rowid ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Rowid.
  Int64 ->
  IO ()
sqlite3_set_last_insert_rowid =
  C.sqlite3_set_last_insert_rowid

-- | https://www.sqlite.org/c3ref/initialize.html
--
-- Deinitialize the library.
sqlite3_shutdown ::
  -- | Result code.
  IO CInt
sqlite3_shutdown =
  C.sqlite3_shutdown

-- | https://www.sqlite.org/c3ref/sleep.html
--
-- Suspend execution.
sqlite3_sleep ::
  -- | Duration, in milliseconds.
  Int ->
  -- | Duration actually suspended, in milliseconds.
  IO Int
sqlite3_sleep =
  fmap cintToInt . C.sqlite3_sleep . intToCInt

-- | https://www.sqlite.org/c3ref/snapshot_cmp.html
--
-- Compare the ages of two snapshots of the same database.
sqlite3_snapshot_cmp ::
  -- | First snapshot.
  Sqlite3_snapshot ->
  -- | Second snapshot.
  Sqlite3_snapshot ->
  -- | @LT@ if first snapshot is older, @EQ@ if the snapshots are equal, or @GT@ if the first snapshot is newer.
  IO Ordering
sqlite3_snapshot_cmp (Sqlite3_snapshot snapshot0) (Sqlite3_snapshot snapshot1) =
  (`compare` 0) <$> C.sqlite3_snapshot_cmp snapshot0 snapshot1

-- | https://www.sqlite.org/c3ref/snapshot_free.html
--
-- Release a snapshot.
sqlite3_snapshot_free ::
  -- | Snapshot.
  Sqlite3_snapshot ->
  IO ()
sqlite3_snapshot_free =
  coerce C.sqlite3_snapshot_free

-- | https://www.sqlite.org/c3ref/snapshot_get.html
--
-- Create a snapshot.
sqlite3_snapshot_get ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | /Out/: snapshot.
  Ptr (Ptr C.Sqlite3_snapshot) ->
  -- | Result code.
  IO CInt
sqlite3_snapshot_get =
  C.sqlite3_snapshot_get

-- | https://www.sqlite.org/c3ref/snapshot_open.html
--
-- Begin a read transaction on a snapshot.
sqlite3_snapshot_open ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Snapshot.
  Ptr C.Sqlite3_snapshot ->
  -- | Result code.
  IO CInt
sqlite3_snapshot_open =
  C.sqlite3_snapshot_open

-- | https://www.sqlite.org/c3ref/snapshot_recover.html
--
-- Recover snapshots from a WAL file.
sqlite3_snapshot_recover ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Result code.
  IO CInt
sqlite3_snapshot_recover =
  C.sqlite3_snapshot_recover

-- | https://www.sqlite.org/c3ref/hard_heap_limit64.html
--
-- Get or set the soft limit on the amount of heap memory that may be allocated.
sqlite3_soft_heap_limit64 ::
  -- | Limit, in bytes, or a negative number to get the limit.
  Int64 ->
  -- | Previous limit, in bytes.
  IO Int64
sqlite3_soft_heap_limit64 =
  C.sqlite3_soft_heap_limit64

-- | https://www.sqlite.org/c3ref/libversion.html
--
-- The date, time, and hash of the library check-in.
sqlite3_sourceid :: CString
sqlite3_sourceid =
  C.sqlite3_sourceid

-- | https://www.sqlite.org/c3ref/expanded_sql.html
--
-- Get the SQL of a statement.
sqlite3_sql ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | SQL (UTF-8).
  IO CString
sqlite3_sql =
  C.sqlite3_sql

-- | https://www.sqlite.org/c3ref/status.html
--
-- Get a library status value.
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
sqlite3_status =
  C.sqlite3_status

-- | https://www.sqlite.org/c3ref/status.html
--
-- Get a library status value.
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
sqlite3_status64 =
  C.sqlite3_status64

-- | https://www.sqlite.org/c3ref/step.html
--
-- Produce the next row of a statement.
sqlite3_step ::
  -- | Statement.
  Sqlite3_stmt ->
  -- | Result code.
  IO CInt
sqlite3_step =
  coerce C.sqlite3_step

-- | https://www.sqlite.org/c3ref/stmt_busy.html
--
-- Get whether a statement is in-progress.
sqlite3_stmt_busy ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_stmt_busy =
  C.sqlite3_stmt_busy

-- | https://www.sqlite.org/c3ref/stmt_isexplain.html
--
-- Get whether a statement is an @EXPLAIN@.
sqlite3_stmt_isexplain ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | @0@ (not @EXPLAIN@), @1@ (@EXPLAIN@), or @2@ (@EXPLAIN QUERY PLAN@).
  IO CInt
sqlite3_stmt_isexplain =
  C.sqlite3_stmt_isexplain

-- | https://www.sqlite.org/c3ref/stmt_readonly.html
--
-- Get whether a statement is read-only.
sqlite3_stmt_readonly ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_stmt_readonly =
  C.sqlite3_stmt_readonly

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus.html
-- sqlite3_stmt_scanstatus ::
--   Ptr C.Sqlite3_stmt ->
--   CInt ->
--   CInt ->
--   Ptr a ->
--   IO CInt
-- sqlite3_stmt_scanstatus =
--   C.sqlite3_stmt_scanstatus

-- -- | https://www.sqlite.org/c3ref/stmt_scanstatus_reset.html
-- sqlite3_stmt_scanstatus_reset ::
--   Ptr C.Sqlite3_stmt ->
--   IO ()
-- sqlite3_stmt_scanstatus_reset =
--   C.sqlite3_stmt_scanstatus_reset

-- | https://www.sqlite.org/c3ref/stmt_status.html
--
-- Get a statement status value.
sqlite3_stmt_status ::
  -- | Statement.
  Ptr C.Sqlite3_stmt ->
  -- | Status code.
  CInt ->
  -- | Reset value? (@0@ or @1@).
  CInt ->
  -- | Value.
  IO CInt
sqlite3_stmt_status =
  C.sqlite3_stmt_status

-- | https://www.sqlite.org/c3ref/strglob.html
--
-- Get whether a string matches a glob pattern.
sqlite3_strglob ::
  -- | Glob pattern (UTF-8).
  CString ->
  -- | String (UTF-8).
  CString ->
  -- | @0@ if matches.
  CInt
sqlite3_strglob =
  C.sqlite3_strglob

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding).
sqlite3_stricmp ::
  -- | First string.
  CString ->
  -- | Second string.
  CString ->
  -- | Negative if first string is less than the second string, @0@ if the strings are equal, or positive if the first
  -- string is greater than the second string.
  CInt
sqlite3_stricmp =
  C.sqlite3_stricmp

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
  -- | @0@ if matches.
  CInt
sqlite3_strlike =
  C.sqlite3_strlike

-- | https://www.sqlite.org/c3ref/stricmp.html
--
-- Compare two strings, case-independent (ascii-only case folding), up to a certain length.
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
sqlite3_strnicmp =
  C.sqlite3_strnicmp

-- | https://www.sqlite.org/c3ref/system_errno.html
--
-- Get the system result code of the most recent failure on a connection.
sqlite3_system_errno ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Result code.
  IO CInt
sqlite3_system_errno =
  C.sqlite3_system_errno

-- | https://www.sqlite.org/c3ref/table_column_metadata.html
--
-- Get column metadata.
sqlite3_table_column_metadata ::
  -- | Connection.
  Ptr C.Sqlite3 ->
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
sqlite3_table_column_metadata =
  C.sqlite3_table_column_metadata

-- | https://www.sqlite.org/c3ref/threadsafe.html
--
-- Get whether the library is thread-safe.
sqlite3_threadsafe :: CInt
sqlite3_threadsafe =
  C.sqlite3_threadsafe

-- | https://www.sqlite.org/c3ref/total_changes.html
--
-- Get the total number of rows modified, inserted, and deleted by @UPDATE@, @INSERT@, and @DELETE@ statements made on a
-- connection.
sqlite3_total_changes ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of rows.
  IO Int
sqlite3_total_changes (Sqlite3 connection) =
  cintToInt <$> C.sqlite3_total_changes connection

-- | https://www.sqlite.org/c3ref/total_changes.html
--
-- Get the total number of rows modified, inserted, and deleted by @UPDATE@, @INSERT@, and @DELETE@ statements made on a
-- connection.
sqlite3_total_changes64 ::
  -- | Connection.
  Sqlite3 ->
  -- | Number of rows.
  IO Int64
sqlite3_total_changes64 =
  coerce C.sqlite3_total_changes64

-- | https://www.sqlite.org/c3ref/trace_v2.html
--
-- Register a callback that is invoked at various times.
sqlite3_trace_v2 ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Property mask.
  CUInt ->
  -- | Callback.
  FunPtr (CUInt -> Ptr a -> Ptr b -> Ptr c -> IO CInt) ->
  -- | Context pointer.
  Ptr a ->
  -- | Result code.
  IO CInt
sqlite3_trace_v2 =
  C.sqlite3_trace_v2

-- | https://www.sqlite.org/c3ref/txn_state.html
--
-- Get the transaction state of a database.
sqlite3_txn_state ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | @SQLITE_TXN_NONE@, @SQLITE_TXN_READ@, @SQLITE_TXN_WRITE@, or @-1@ if the database does not exist.
  IO CInt
sqlite3_txn_state =
  C.sqlite3_txn_state

-- | https://www.sqlite.org/c3ref/unlock_notify.html
sqlite3_unlock_notify ::
  Ptr C.Sqlite3 ->
  FunPtr (Ptr (Ptr a) -> CInt -> IO ()) ->
  Ptr a ->
  IO CInt
sqlite3_unlock_notify =
  C.sqlite3_unlock_notify

-- | https://www.sqlite.org/c3ref/update_hook.html
sqlite3_update_hook ::
  Ptr C.Sqlite3 ->
  FunPtr (Ptr a -> CInt -> CString -> CString -> Int64 -> IO ()) ->
  Ptr a ->
  IO (Ptr b)
sqlite3_update_hook =
  C.sqlite3_update_hook

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
  -- | Query parameter value (@0@ or @1@).
  IO CInt
sqlite3_uri_boolean =
  C.sqlite3_uri_boolean

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
  C.sqlite3_uri_int64

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
  C.sqlite3_uri_key

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
  C.sqlite3_uri_parameter

-- | https://www.sqlite.org/c3ref/user_data.html
sqlite3_user_data ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  IO (Ptr a)
sqlite3_user_data =
  C.sqlite3_user_data

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the blob of a protected value.
sqlite3_value_blob ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Blob.
  IO (Ptr a)
sqlite3_value_blob =
  C.sqlite3_value_blob

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the size of a protected blob or string value, in bytes.
sqlite3_value_bytes ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Size, in bytes.
  IO CInt
sqlite3_value_bytes =
  C.sqlite3_value_bytes

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the double of a protected value.
sqlite3_value_double ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Double.
  IO CDouble
sqlite3_value_double =
  C.sqlite3_value_double

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Copy a value.
sqlite3_value_dup ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Value copy (protected).
  IO (Ptr C.Sqlite3_value)
sqlite3_value_dup =
  C.sqlite3_value_dup

-- | https://www.sqlite.org/c3ref/value_dup.html
--
-- Release memory acquired by 'sqlite3_value_dup'.
sqlite3_value_free ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  IO ()
sqlite3_value_free =
  C.sqlite3_value_free

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get whether a protected value is a bound parameter.
sqlite3_value_frombind ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_value_frombind =
  C.sqlite3_value_frombind

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
sqlite3_value_int ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Integer.
  IO CInt
sqlite3_value_int =
  C.sqlite3_value_int

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the integer of a protected value.
sqlite3_value_int64 ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Integer.
  IO Int64
sqlite3_value_int64 =
  C.sqlite3_value_int64

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Within @xUpdate@, get whether the column corresponding to a protected value is unchanged.
sqlite3_value_nochange ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_value_nochange =
  C.sqlite3_value_nochange

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the numeric type of a protected value.
sqlite3_value_numeric_type ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Type.
  IO CInt
sqlite3_value_numeric_type =
  C.sqlite3_value_numeric_type

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the pointer of a protected value.
sqlite3_value_pointer ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Pointer type.
  CString ->
  -- | Pointer.
  IO (Ptr a)
sqlite3_value_pointer =
  C.sqlite3_value_pointer

-- | https://www.sqlite.org/c3ref/value_subtype.html
--
-- Get the subtype of the return value of a function.
sqlite3_value_subtype ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | Subtype.
  IO CUInt
sqlite3_value_subtype =
  C.sqlite3_value_subtype

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the string of a protected value.
sqlite3_value_text ::
  -- | Value.
  Sqlite3_value ->
  -- | String.
  IO Text
sqlite3_value_text (Sqlite3_value value) = do
  text <- C.sqlite3_value_text value
  cstringToText (castPtr @CUChar @CChar text)

-- | https://www.sqlite.org/c3ref/value_blob.html
--
-- Get the type of a protected value.
sqlite3_value_type ::
  -- | Value.
  Sqlite3_value ->
  -- | Type.
  IO SQLITE_DATATYPE
sqlite3_value_type =
  coerce C.sqlite3_value_type

-- | https://www.sqlite.org/c3ref/libversion.html
sqlite3_version :: CString
sqlite3_version =
  C.sqlite3_version

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Get a VFS.
sqlite3_vfs_find ::
  -- | VFS name (UTF-8).
  CString ->
  -- | VFS.
  IO (Ptr C.Sqlite3_vfs)
sqlite3_vfs_find =
  C.sqlite3_vfs_find

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Register a VFS.
sqlite3_vfs_register ::
  -- | VFS.
  Ptr C.Sqlite3_vfs ->
  -- | Make default? (@0@ or @1@).
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_vfs_register =
  C.sqlite3_vfs_register

-- | https://www.sqlite.org/c3ref/vfs_find.html
--
-- Unregister a VFS.
sqlite3_vfs_unregister ::
  -- | VFS.
  Ptr C.Sqlite3_vfs ->
  -- | Result code.
  IO CInt
sqlite3_vfs_unregister =
  C.sqlite3_vfs_unregister

-- | https://www.sqlite.org/c3ref/vtab_collation.html
--
-- Get the collating sequence of a virtual table constraint.
sqlite3_vtab_collation ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr C.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | Collating sequence name (UTF-8).
  IO CString
sqlite3_vtab_collation =
  C.sqlite3_vtab_collation

-- https://www.sqlite.org/c3ref/vtab_config.html
--
-- Configure a virtual table.
sqlite3_vtab_config__1 :: Ptr C.Sqlite3 -> CInt -> CInt -> IO CInt
sqlite3_vtab_config__1 =
  C.sqlite3_vtab_config__1

-- https://www.sqlite.org/c3ref/vtab_config.html
--
-- Configure a virtual table.
sqlite3_vtab_config__2 :: Ptr C.Sqlite3 -> CInt -> IO CInt
sqlite3_vtab_config__2 =
  C.sqlite3_vtab_config__2

-- | https://www.sqlite.org/c3ref/vtab_distinct.html
--
-- Get information about how the query planner wants output to be ordered.
sqlite3_vtab_distinct ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr C.Sqlite3_index_info ->
  -- | @0@, @1@, @2@, or @3@.
  IO CInt
sqlite3_vtab_distinct =
  C.sqlite3_vtab_distinct

-- | https://www.sqlite.org/c3ref/vtab_in.html
--
-- Get whether a virtual table constraint is an @IN@ operator that can be processed all at once.
sqlite3_vtab_in ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr C.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | @-1@, @0@, or @1@.
  CInt ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_vtab_in =
  C.sqlite3_vtab_in

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the first value on the right-hand side of an @IN@ constraint.
sqlite3_vtab_in_first ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | /Out/: Value.
  Ptr (Ptr C.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_in_first =
  C.sqlite3_vtab_in_first

-- | https://www.sqlite.org/c3ref/vtab_in_first.html
--
-- Get the next value on the right-hand side of an @IN@ constraint.
sqlite3_vtab_in_next ::
  -- | Value.
  Ptr C.Sqlite3_value ->
  -- | /Out/: Value.
  Ptr (Ptr C.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_in_next =
  C.sqlite3_vtab_in_next

-- | https://www.sqlite.org/c3ref/vtab_nochange.html
--
-- Within @xColumn@, get whether the column is being fetched as part of an @UPDATE@ in which its value will not change.
sqlite3_vtab_nochange ::
  -- | Function context.
  Ptr C.Sqlite3_context ->
  -- | @0@ or @1@.
  IO CInt
sqlite3_vtab_nochange =
  C.sqlite3_vtab_nochange

-- | https://www.sqlite.org/c3ref/vtab_on_conflict.html
--
-- Within @xUpdate@ for an @INSERT@ or @UPDATE@, get the conflict resolution algorithm.
sqlite3_vtab_on_conflict ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Conflict resolution algorithm.
  IO CInt
sqlite3_vtab_on_conflict =
  C.sqlite3_vtab_on_conflict

-- | https://www.sqlite.org/c3ref/vtab_rhs_value.html
--
-- Within @xBestIndex@, get the right-hand side of a virtual table constraint.
sqlite3_vtab_rhs_value ::
  -- | Index info (first argument to @xBestIndex@).
  Ptr C.Sqlite3_index_info ->
  -- | @aConstraint[]@ index.
  CInt ->
  -- | /Out/: value.
  Ptr (Ptr C.Sqlite3_value) ->
  -- | Result code.
  IO CInt
sqlite3_vtab_rhs_value =
  C.sqlite3_vtab_rhs_value

-- | https://www.sqlite.org/c3ref/wal_autocheckpoint.html
--
-- Register a callback that checkpoints the WAL after committing a transaction if there are more than a certain number
-- of frames in the WAL.
sqlite3_wal_autocheckpoint ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Number of frames that will trigger a checkpoint.
  CInt ->
  -- | Result code.
  IO CInt
sqlite3_wal_autocheckpoint =
  C.sqlite3_wal_autocheckpoint

-- | https://www.sqlite.org/c3ref/wal_checkpoint.html
--
-- Checkpoint the WAL.
sqlite3_wal_checkpoint ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Database name (UTF-8).
  CString ->
  -- | Result code.
  IO CInt
sqlite3_wal_checkpoint =
  C.sqlite3_wal_checkpoint

-- | https://www.sqlite.org/c3ref/wal_checkpoint_v2.html
--
-- Checkpoint the WAL.
sqlite3_wal_checkpoint_v2 ::
  -- | Connection.
  Ptr C.Sqlite3 ->
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
  C.sqlite3_wal_checkpoint_v2

-- | https://www.sqlite.org/c3ref/wal_hook.html
--
-- Register a callback that is invoked each time data is committed to a database in WAL mode.
sqlite3_wal_hook ::
  -- | Connection.
  Ptr C.Sqlite3 ->
  -- | Callback.
  FunPtr (Ptr a -> Ptr C.Sqlite3 -> CString -> CInt -> IO CInt) ->
  -- | Application data.
  Ptr a ->
  -- | Previous application data.
  IO (Ptr b)
sqlite3_wal_hook =
  C.sqlite3_wal_hook

--

freeAsFunPtrIfNonNull :: Ptr a -> IO ()
freeAsFunPtrIfNonNull ptr =
  when (ptr /= nullPtr) (freeHaskellFunPtr (castPtrToFunPtr ptr))

-- | Free the commit hook of a connection, if any.
freeCommitHook :: Ptr C.Sqlite3 -> IO ()
freeCommitHook connection = do
  hook <- C.sqlite3_commit_hook connection nullFunPtr nullPtr
  freeAsFunPtrIfNonNull hook

-- | Free the rollback hook of a connection, if any.
freeRollbackHook :: Ptr C.Sqlite3 -> IO ()
freeRollbackHook connection = do
  hook <- C.sqlite3_rollback_hook connection nullFunPtr nullPtr
  freeAsFunPtrIfNonNull hook

-- A top-level FunPtr to 'freeStablePtr'
freeStablePtrFunPtr :: FunPtr (Ptr a -> IO ())
freeStablePtrFunPtr =
  unsafePerformIO (makeCallback0 \ptr -> freeStablePtr (castPtrToStablePtr (castPtr ptr)))
{-# NOINLINE freeStablePtrFunPtr #-}

foreign import capi "HsFFI.h &hs_free_fun_ptr"
  hs_free_fun_ptr :: FunPtr (Ptr a -> IO ())

foreign import ccall "wrapper"
  makeCallback0 ::
    (Ptr a -> IO ()) ->
    IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "wrapper"
  makeCallback1 ::
    (Ptr a -> IO CInt) ->
    IO (FunPtr (Ptr a -> IO CInt))

foreign import ccall "wrapper"
  makeCallback2 ::
    (Ptr a -> CInt -> IO CInt) ->
    IO (FunPtr (Ptr a -> CInt -> IO CInt))

foreign import ccall "wrapper"
  makeCallback3 ::
    (Ptr a -> CInt -> CString -> IO ()) ->
    IO (FunPtr (Ptr a -> CInt -> CString -> IO ()))

foreign import ccall "wrapper"
  makeCallback4 ::
    (Ptr a -> CInt -> Ptr b -> CInt -> Ptr b -> IO CInt) ->
    IO (FunPtr (Ptr a -> CInt -> Ptr b -> CInt -> Ptr b -> IO CInt))

foreign import ccall "wrapper"
  makeCallback5 ::
    (Ptr a -> CInt -> Ptr CString -> Ptr CString -> IO CInt) ->
    IO (FunPtr (Ptr a -> CInt -> Ptr CString -> Ptr CString -> IO CInt))

foreign import ccall "wrapper"
  makeCallback6 ::
    (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt) ->
    IO (FunPtr (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt))

foreign import ccall "wrapper"
  makeCallback7 ::
    (Ptr a -> Ptr C.Sqlite3 -> CInt -> CString -> IO ()) ->
    IO (FunPtr (Ptr a -> Ptr C.Sqlite3 -> CInt -> CString -> IO ()))

foreign import ccall "wrapper"
  makeCallback8 ::
    (Ptr C.Sqlite3_context -> CInt -> Ptr (Ptr C.Sqlite3_value) -> IO ()) ->
    IO (FunPtr (Ptr C.Sqlite3_context -> CInt -> Ptr (Ptr C.Sqlite3_value) -> IO ()))
