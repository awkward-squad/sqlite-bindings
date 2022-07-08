module Sqlite.Bindings.Internal.Functions where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CDouble (..), CInt (..), CUChar (..), CUInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import Sqlite.Bindings.Internal.Objects

-- | https://www.sqlite.org/c3ref/aggregate_context.html
foreign import capi unsafe "sqlite3.h sqlite3_aggregate_context"
  sqlite3_aggregate_context ::
    -- | Context.
    Ptr Sqlite3_context ->
    -- | Number of bytes.
    CInt ->
    IO (Ptr a)

sqlite3_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/autovacuum_pages.html
foreign import capi unsafe "sqlite3.h sqlite3_autovacuum_pages"
  sqlite3_autovacuum_pages ::
    -- | Database.
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> CString -> CUInt -> CUInt -> CUInt -> IO CUInt) ->
    Ptr a ->
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import capi unsafe "sqlite3.h sqlite3_backup_finish"
  sqlite3_backup_finish ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import capi unsafe "sqlite3.h sqlite3_backup_init"
  sqlite3_backup_init ::
    -- | Destination database.
    Ptr Sqlite3 ->
    -- | Destination database name.
    CString ->
    -- | Source database.
    Ptr Sqlite3 ->
    -- | Source database name.
    CString ->
    IO (Ptr Sqlite3_backup)

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import capi unsafe "sqlite3.h sqlite3_backup_pagecount"
  sqlite3_backup_pagecount ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import capi unsafe "sqlite3.h sqlite3_backup_remaining"
  sqlite3_backup_remaining ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    IO CInt

-- | https://www.sqlite.org/c3ref/backup_finish.html
foreign import capi safe "sqlite3.h sqlite3_backup_step"
  sqlite3_backup_step ::
    -- | Backup.
    Ptr Sqlite3_backup ->
    -- | Number of pages to copy.
    CInt ->
    IO CInt

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
foreign import capi unsafe "sqlite3.h sqlite3_bind_blob64"
  sqlite3_bind_blob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Blob.
    Ptr a ->
    -- | Length of blob in bytes.
    Int64 ->
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
foreign import capi unsafe "sqlite3.h sqlite3_bind_int64"
  sqlite3_bind_int64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Integer.
    Int64 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_null"
  sqlite3_bind_null ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    IO CInt

sqlite3_bind_parameter_count = undefined

sqlite3_bind_parameter_index = undefined

sqlite3_bind_parameter_name = undefined

sqlite3_bind_pointer = undefined

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
    FunPtr (Ptr a -> IO ()) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_text64"
  sqlite3_bind_text64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | String (UTF-8).
    Ptr CChar ->
    -- | Length of string in bytes.
    Int64 ->
    FunPtr (Ptr a -> IO ()) ->
    -- | Encoding.
    CUChar ->
    IO CInt

sqlite3_bind_value = undefined

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

-- | https://www.sqlite.org/c3ref/bind_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_bind_zeroblob64"
  sqlite3_bind_zeroblob64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Parameter index (1-based).
    CInt ->
    -- | Length of blob in bytes.
    Int64 ->
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

sqlite3_blob_reopen = undefined

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

sqlite3_busy_timeout = undefined

sqlite3_cancel_auto_extension = undefined

-- | https://www.sqlite.org/c3ref/changes.html
foreign import capi unsafe "sqlite3.h sqlite3_changes"
  sqlite3_changes ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/changes.html
foreign import capi unsafe "sqlite3.h sqlite3_changes64"
  sqlite3_changes64 ::
    -- | Database.
    Ptr Sqlite3 ->
    IO Int64

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

sqlite3_collation_needed = undefined

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

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import capi unsafe "sqlite3.h sqlite3_column_database_name"
  sqlite3_column_database_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

sqlite3_column_decltype = undefined

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
foreign import capi unsafe "sqlite3.h sqlite3_column_int64"
  sqlite3_column_int64 ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO Int64

-- | https://www.sqlite.org/c3ref/column_name.html
foreign import capi unsafe "sqlite3.h sqlite3_column_name"
  sqlite3_column_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import capi unsafe "sqlite3.h sqlite3_column_origin_name"
  sqlite3_column_origin_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/column_database_name.html
foreign import capi unsafe "sqlite3.h sqlite3_column_table_name"
  sqlite3_column_table_name ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO CString

-- | https://www.sqlite.org/c3ref/column_blob.html
foreign import capi unsafe "sqlite3.h sqlite3_column_text"
  sqlite3_column_text ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    -- | Column index (0-based).
    CInt ->
    IO (Ptr CUChar)

sqlite3_column_type = undefined

sqlite3_column_value = undefined

-- | https://www.sqlite.org/c3ref/commit_hook.html
foreign import capi unsafe "sqlite3.h sqlite3_commit_hook"
  sqlite3_commit_hook ::
    -- | Database
    Ptr Sqlite3 ->
    -- | Commit hook.
    FunPtr (Ptr a -> IO CInt) ->
    Ptr a ->
    IO (Ptr b)

sqlite3_compileoption_get = undefined

sqlite3_compileoption_used = undefined

-- | https://www.sqlite.org/c3ref/complete.html
foreign import capi unsafe "sqlite3.h sqlite3_complete"
  sqlite3_complete ::
    -- | SQL.
    CString ->
    CInt

sqlite3_config = undefined

sqlite3_context_db_handle = undefined

sqlite3_create_collation = undefined

sqlite3_create_collation16 = undefined

sqlite3_create_collation_v2 = undefined

sqlite3_create_filename = undefined

sqlite3_create_function = undefined

sqlite3_create_function16 = undefined

sqlite3_create_function_v2 = undefined

sqlite3_create_module = undefined

sqlite3_create_module_v2 = undefined

sqlite3_create_window_function = undefined

sqlite3_data_count = undefined

sqlite3_database_file_object = undefined

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
foreign import capi safe "sqlite3.h sqlite3_db_cacheflush"
  sqlite3_db_cacheflush__safe ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

-- | https://www.sqlite.org/c3ref/db_cacheflush.html
foreign import capi unsafe "sqlite3.h sqlite3_db_cacheflush"
  sqlite3_db_cacheflush__unsafe ::
    -- | Database.
    Ptr Sqlite3 ->
    IO CInt

sqlite3_db_config = undefined

sqlite3_db_filename = undefined

sqlite3_db_handle = undefined

sqlite3_db_mutex = undefined

sqlite3_db_name = undefined

sqlite3_db_readonly = undefined

sqlite3_db_release_memory = undefined

sqlite3_db_status = undefined

sqlite3_declare_vtab = undefined

sqlite3_deserialize = undefined

sqlite3_drop_modules = undefined

sqlite3_enable_load_extension = undefined

sqlite3_enable_shared_cache = undefined

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
foreign import capi unsafe "sqlite3.h sqlite3_errstr"
  sqlite3_errstr :: CInt -> CString

sqlite3_exec = undefined

sqlite3_expanded_sql = undefined

-- | https://www.sqlite.org/c3ref/errcode.html
foreign import capi unsafe "sqlite3.h sqlite3_extended_errcode"
  sqlite3_extended_errcode ::
    -- | Database
    Ptr Sqlite3 ->
    IO CInt

sqlite3_file_control = undefined

sqlite3_filename_database = undefined

sqlite3_filename_journal = undefined

sqlite3_filename_wal = undefined

-- | https://www.sqlite.org/c3ref/finalize.html
foreign import capi unsafe "sqlite3.h sqlite3_finalize"
  sqlite3_finalize ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

sqlite3_free = undefined

sqlite3_free_filename = undefined

sqlite3_free_table = undefined

sqlite3_get_autocommit = undefined

sqlite3_get_auxdata = undefined

sqlite3_get_table = undefined

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

sqlite3_open = undefined

sqlite3_open16 = undefined

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

sqlite3_os_end = undefined

sqlite3_os_init = undefined

sqlite3_overload_function = undefined

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

sqlite3_preupdate_blobwrite = undefined

sqlite3_preupdate_count = undefined

sqlite3_preupdate_depth = undefined

sqlite3_preupdate_hook = undefined

sqlite3_preupdate_new = undefined

sqlite3_preupdate_old = undefined

sqlite3_progress_handler = undefined

sqlite3_randomness = undefined

sqlite3_realloc = undefined

sqlite3_realloc64 = undefined

sqlite3_release_memory = undefined

-- | https://www.sqlite.org/c3ref/reset.html
foreign import capi unsafe "sqlite3.h sqlite3_reset"
  sqlite3_reset ::
    -- | Statement.
    Ptr Sqlite3_stmt ->
    IO CInt

sqlite3_reset_auto_extension = undefined

sqlite3_result_blob = undefined

sqlite3_result_blob64 = undefined

sqlite3_result_double = undefined

sqlite3_result_error = undefined

sqlite3_result_error16 = undefined

sqlite3_result_error_code = undefined

sqlite3_result_error_nomem = undefined

sqlite3_result_error_toobig = undefined

sqlite3_result_int = undefined

sqlite3_result_int64 = undefined

sqlite3_result_null = undefined

sqlite3_result_pointer = undefined

sqlite3_result_subtype = undefined

sqlite3_result_text = undefined

sqlite3_result_text16 = undefined

sqlite3_result_text16be = undefined

sqlite3_result_text16le = undefined

sqlite3_result_text64 = undefined

sqlite3_result_value = undefined

sqlite3_result_zeroblob = undefined

sqlite3_result_zeroblob64 = undefined

-- | https://www.sqlite.org/c3ref/commit_hook.html
foreign import capi unsafe "sqlite3.h sqlite3_rollback_hook"
  sqlite3_rollback_hook ::
    -- | Database
    Ptr Sqlite3 ->
    -- | Rollback hook.
    FunPtr (Ptr a -> IO CInt) ->
    Ptr a ->
    IO (Ptr b)

sqlite3_serialize = undefined

sqlite3_set_authorizer = undefined

sqlite3_set_auxdata = undefined

sqlite3_set_last_insert_rowid = undefined

sqlite3_shutdown = undefined

sqlite3_sleep = undefined

sqlite3_snapshot_cmp = undefined

sqlite3_snapshot_free = undefined

sqlite3_snapshot_get = undefined

sqlite3_snapshot_open = undefined

sqlite3_snapshot_recover = undefined

sqlite3_snprintf = undefined

sqlite3_soft_heap_limit64 = undefined

sqlite3_sourceid = undefined

sqlite3_sql = undefined

sqlite3_status = undefined

sqlite3_status64 = undefined

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

sqlite3_stmt_busy = undefined

sqlite3_stmt_isexplain = undefined

sqlite3_stmt_readonly = undefined

sqlite3_stmt_scanstatus = undefined

sqlite3_stmt_scanstatus_reset = undefined

sqlite3_stmt_status = undefined

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

sqlite3_system_errno = undefined

sqlite3_table_column_metadata = undefined

sqlite3_test_control = undefined

sqlite3_threadsafe = undefined

sqlite3_total_changes = undefined

sqlite3_total_changes64 = undefined

sqlite3_trace_v2 = undefined

sqlite3_txn_state = undefined

sqlite3_unlock_notify = undefined

sqlite3_update_hook = undefined

sqlite3_uri_boolean = undefined

sqlite3_uri_int64 = undefined

sqlite3_uri_key = undefined

sqlite3_uri_parameter = undefined

sqlite3_user_data = undefined

sqlite3_value_blob = undefined

sqlite3_value_bytes = undefined

sqlite3_value_bytes16 = undefined

sqlite3_value_double = undefined

sqlite3_value_dup = undefined

sqlite3_value_free = undefined

sqlite3_value_frombind = undefined

sqlite3_value_int = undefined

sqlite3_value_int64 = undefined

sqlite3_value_nochange = undefined

sqlite3_value_numeric_type = undefined

sqlite3_value_pointer = undefined

sqlite3_value_subtype = undefined

sqlite3_value_text = undefined

sqlite3_value_text16 = undefined

sqlite3_value_text16be = undefined

sqlite3_value_text16le = undefined

sqlite3_value_type = undefined

sqlite3_version = undefined

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

sqlite3_wal_autocheckpoint = undefined

sqlite3_wal_checkpoint = undefined

sqlite3_wal_checkpoint_v2 = undefined

sqlite3_wal_hook = undefined

------------------------------------------------------------------------------------------------------------------------

-- TODO sort/rename these?

foreign import ccall "wrapper"
  createCallback0 :: (Ptr a -> CInt -> IO CInt) -> IO (FunPtr (Ptr a -> CInt -> IO CInt))

foreign import ccall "wrapper"
  createCallback1 :: (Ptr a -> IO CInt) -> IO (FunPtr (Ptr a -> IO CInt))

foreign import ccall "wrapper"
  createCallback2 :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))
