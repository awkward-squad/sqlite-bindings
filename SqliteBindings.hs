module SqliteBindings where

import Foreign.Ptr (Ptr)
import Foreign.C.Types

-- todo - annotate safe/unsafe

-- https://www.sqlite.org/cintro.html

-- https://www.sqlite.org/c3ref/sqlite3.html
--
-- sqlite3 → The database connection object. Created by sqlite3_open() and destroyed by sqlite3_close().
--
-- 2 Destructors: sqlite3_close(), sqlite3_close_v2()
--
-- sqlite3_autovacuum_pages
-- sqlite3_blob_open
-- sqlite3_busy_handler
-- sqlite3_busy_timeout
-- sqlite3_changes
-- sqlite3_changes64
-- sqlite3_collation_needed
-- sqlite3_collation_needed16
-- sqlite3_commit_hook
-- sqlite3_create_collation
-- sqlite3_create_collation16
-- sqlite3_create_collation_v2
-- sqlite3_create_function
-- sqlite3_create_function16
-- sqlite3_create_function_v2
-- sqlite3_create_module
-- sqlite3_create_module_v2
-- sqlite3_create_window_function
-- sqlite3_db_cacheflush
-- sqlite3_db_config
-- sqlite3_db_filename
-- sqlite3_db_mutex
-- sqlite3_db_name
-- sqlite3_db_readonly
-- sqlite3_db_release_memory
-- sqlite3_db_status
-- sqlite3_drop_modules
-- sqlite3_enable_load_extension
-- sqlite3_errcode
-- sqlite3_errmsg
-- sqlite3_errmsg16
-- sqlite3_error_offset
-- sqlite3_errstr
-- sqlite3_exec
-- sqlite3_extended_errcode
-- sqlite3_extended_result_codes
-- sqlite3_file_control
-- sqlite3_free_table
-- sqlite3_get_autocommit
-- sqlite3_get_table
-- sqlite3_interrupt
-- sqlite3_last_insert_rowid
-- sqlite3_limit
-- sqlite3_load_extension
-- sqlite3_next_stmt
-- sqlite3_overload_function
-- sqlite3_prepare
-- sqlite3_prepare16
-- sqlite3_prepare16_v2
-- sqlite3_prepare16_v3
-- sqlite3_prepare_v2
-- sqlite3_prepare_v3
-- sqlite3_preupdate_blobwrite
-- sqlite3_preupdate_count
-- sqlite3_preupdate_depth
-- sqlite3_preupdate_hook
-- sqlite3_preupdate_new
-- sqlite3_preupdate_old
-- sqlite3_profile
-- sqlite3_progress_handler
-- sqlite3_rollback_hook
-- sqlite3_set_authorizer
-- sqlite3_set_last_insert_rowid
-- sqlite3_system_errno
-- sqlite3_table_column_metadata
-- sqlite3_total_changes
-- sqlite3_total_changes64
-- sqlite3_trace
-- sqlite3_trace_v2
-- sqlite3_txn_state
-- sqlite3_unlock_notify
-- sqlite3_update_hook
-- sqlite3_wal_autocheckpoint
-- sqlite3_wal_checkpoint
-- sqlite3_wal_checkpoint_v2
-- sqlite3_wal_hook

data Sqlite3
data Sqlite3Stmt

foreign import ccall "sqlite3_bind_blob"
  sqlite3BindBlob :: Ptr Sqlite3Stmt -> CInt -> Ptr () -> CInt -> Ptr (Ptr () -> IO ()) -> IO CInt

foreign import ccall "sqlite3_bind_double"
  sqlite3BindDouble :: Ptr Sqlite3Stmt -> CInt -> CDouble -> IO CInt

foreign import ccall "sqlite3_bind_int"
  sqlite3BindInt :: Ptr Sqlite3Stmt -> CInt -> CInt -> IO CInt

foreign import ccall "sqlite3_bind_null"
  sqlite3BindNull :: Ptr Sqlite3Stmt -> CInt -> IO CInt

foreign import ccall "sqlite3_close_v2"
  sqlite3CloseV2 :: Ptr Sqlite3 -> IO ()

foreign import ccall "sqlite3_open_v2"
  sqlite3OpenV2 :: Ptr CChar -> Ptr (Ptr Sqlite3) -> CInt -> Ptr CChar -> IO CInt

foreign import ccall "sqlite3_prepare_v2"
  sqlite3PrepareV2 :: Ptr Sqlite3 -> Ptr CChar -> CInt -> Ptr (Ptr Sqlite3Stmt) -> Ptr (Ptr CChar) -> IO CInt


-- https://www.sqlite.org/c3ref/stmt.html
--
-- sqlite3_stmt → The prepared statement object. Created by sqlite3_prepare() and destroyed by sqlite3_finalize().
--
-- 6 Constructors:
--
--     sqlite3_prepare
--     sqlite3_prepare16
--     sqlite3_prepare16_v2
--     sqlite3_prepare16_v3
--     sqlite3_prepare_v2
--     sqlite3_prepare_v3
--
-- 1 Destructor: sqlite3_finalize()
--
-- 51 Methods:
--
--     sqlite3_bind_blob
--     sqlite3_bind_blob64
--     sqlite3_bind_double
--     sqlite3_bind_int
--     sqlite3_bind_int64
--     sqlite3_bind_null
--     sqlite3_bind_parameter_count
--     sqlite3_bind_parameter_index
--     sqlite3_bind_parameter_name
--     sqlite3_bind_pointer
--     sqlite3_bind_text
--     sqlite3_bind_text16
--     sqlite3_bind_text64
--     sqlite3_bind_value
--     sqlite3_bind_zeroblob
--     sqlite3_bind_zeroblob64
--     sqlite3_clear_bindings
--     sqlite3_column_blob
--     sqlite3_column_bytes
--     sqlite3_column_bytes16
--     sqlite3_column_count
--     sqlite3_column_database_name
--     sqlite3_column_database_name16
--     sqlite3_column_decltype
--     sqlite3_column_decltype16
--     sqlite3_column_double
--     sqlite3_column_int
--     sqlite3_column_int64
--     sqlite3_column_name
--     sqlite3_column_name16
--     sqlite3_column_origin_name
--     sqlite3_column_origin_name16
--     sqlite3_column_table_name
--     sqlite3_column_table_name16
--     sqlite3_column_text
--     sqlite3_column_text16
--     sqlite3_column_type
--     sqlite3_column_value
--     sqlite3_data_count
--     sqlite3_db_handle
--     sqlite3_expanded_sql
--     sqlite3_normalized_sql
--     sqlite3_reset
--     sqlite3_sql
--     sqlite3_step
--     sqlite3_stmt_busy
--     sqlite3_stmt_isexplain
--     sqlite3_stmt_readonly
--     sqlite3_stmt_scanstatus
--     sqlite3_stmt_scanstatus_reset
--     sqlite3_stmt_status

-- sqlite3_open() → Open a connection to a new or existing SQLite database. The constructor for sqlite3.

-- sqlite3_prepare() → Compile SQL text into byte-code that will do the work of querying or updating the database. The constructor for sqlite3_stmt.

-- sqlite3_bind() → Store application data into parameters of the original SQL.

-- sqlite3_step() → Advance an sqlite3_stmt to the next result row or to completion.

-- sqlite3_column() → Column values in the current result row for an sqlite3_stmt.

-- sqlite3_finalize() → Destructor for sqlite3_stmt.

-- sqlite3_close() → Destructor for sqlite3.

-- sqlite3_exec() → A wrapper function that does sqlite3_prepare(), sqlite3_step(), sqlite3_column(), and sqlite3_finalize() for a string of one or more SQL statements.
