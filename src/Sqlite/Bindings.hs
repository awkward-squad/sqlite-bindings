module Sqlite.Bindings
  ( -- * Objects
    Sqlite3,
    Sqlite3_backup,
    Sqlite3_blob,
    Sqlite3_context,
    Sqlite3_mutex,
    Sqlite3_stmt,
    Sqlite3_value,

    -- ** Database
    sqlite3_config,
    sqlite3_enable_shared_cache,

    -- ** Database connections
    sqlite3_open,
    sqlite3_open_v2,
    sqlite3_close,
    sqlite3_close_v2,
    sqlite3_db_config,
    sqlite3_db_filename,
    sqlite3_db_name,
    sqlite3_get_autocommit,
    sqlite3_db_readonly,
    sqlite3_db_mutex,
    sqlite3_db_status,
    sqlite3_db_release_memory,

    -- ** Binding values to prepared statements
    sqlite3_bind_blob,
    sqlite3_bind_blob64,
    sqlite3_bind_double,
    sqlite3_bind_int,
    sqlite3_bind_int64,
    sqlite3_bind_null,
    sqlite3_bind_pointer,
    sqlite3_bind_text,
    sqlite3_bind_text64,
    sqlite3_bind_value,
    sqlite3_bind_zeroblob,
    sqlite3_bind_zeroblob64,
    sqlite3_bind_parameter_index,
    sqlite3_bind_parameter_name,
    sqlite3_bind_parameter_count,

    -- ** Query result columns
    sqlite3_column_count,
    sqlite3_data_count,
    sqlite3_column_type,
    sqlite3_column_blob,
    sqlite3_column_bytes,
    sqlite3_column_double,
    sqlite3_column_int,
    sqlite3_column_int64,
    sqlite3_column_text,
    sqlite3_column_value,

    -- * Functions
    sqlite3_aggregate_context,
    sqlite3_autovacuum_pages,
    sqlite3_column_decltype,

    -- ** BLOBs
    sqlite3_blob_bytes,
    sqlite3_blob_close,
    sqlite3_blob_open,
    sqlite3_blob_read,
    sqlite3_blob_reopen,
    sqlite3_blob_write,

    -- ** Collations
    sqlite3_create_collation,
    sqlite3_create_collation_v2,
    sqlite3_collation_needed,

    -- ** Compile-time options
    sqlite3_compileoption_get,
    sqlite3_compileoption_used,

    -- ** Extensions
    sqlite3_auto_extension,
    sqlite3_cancel_auto_extension,
    sqlite3_reset_auto_extension,

    -- ** Functions
    sqlite3_create_function,
    sqlite3_create_function_v2,
    sqlite3_create_window_function,
    sqlite3_context_db_handle,
    sqlite3_user_data,
    sqlite3_result_subtype,
    sqlite3_result_blob,
    sqlite3_result_blob64,
    sqlite3_result_double,
    sqlite3_result_error,
    sqlite3_result_error_code,
    sqlite3_result_error_nomem,
    sqlite3_result_error_toobig,
    sqlite3_result_int,
    sqlite3_result_int64,
    sqlite3_result_null,
    sqlite3_result_pointer,
    sqlite3_result_text,
    sqlite3_result_text64,
    sqlite3_result_value,
    sqlite3_result_zeroblob,
    sqlite3_result_zeroblob64,

    -- ** Virtual tables
    sqlite3_create_module,
    sqlite3_create_module_v2,
    sqlite3_drop_modules,

    -- ** Online backup API
    sqlite3_backup_finish,
    sqlite3_backup_init,
    sqlite3_backup_pagecount,
    sqlite3_backup_remaining,
    sqlite3_backup_step,

    -- ** `SQLITE_BUSY` handlers
    sqlite3_busy_handler,
    sqlite3_busy_timeout,

    -- ** Count the number of rows modified
    sqlite3_changes,
    sqlite3_changes64,

    -- ** Reset all bindings on a prepared statement
    sqlite3_clear_bindings,

    -- ** Source of data in a query result
    sqlite3_column_database_name,
    sqlite3_column_origin_name,
    sqlite3_column_table_name,

    -- ** Column names in a result set
    sqlite3_column_name,

    -- ** Commit and rollback notification callbacks
    sqlite3_commit_hook,
    sqlite3_rollback_hook,

    -- ** Determine if an SQL statement is complete
    sqlite3_complete,

    -- ** Flush caches to disk mid-transaction
    sqlite3_db_cacheflush__safe,
    sqlite3_db_cacheflush__unsafe,

    -- ** Declare the schema of a virtual table
    sqlite3_declare_vtab,

    -- ** Deserialize a database
    sqlite3_deserialize,

    -- ** Error codes and messages
    sqlite3_errcode,
    sqlite3_errmsg,
    sqlite3_error_offset,
    sqlite3_errstr,
    sqlite3_extended_errcode,

    -- ** Retrieving statement SQL
    sqlite3_expanded_sql,
    sqlite3_normalized_sql,
    sqlite3_sql,

    -- ** Enable or disable extended result codes
    sqlite3_extended_result_codes,

    -- ** Low-level control of database files
    sqlite3_file_control,

    -- ** Translate filenames
    sqlite3_filename_database,
    sqlite3_filename_journal,
    sqlite3_filename_wal,

    -- ** Prepared statement objects
    sqlite3_prepare_v2,
    sqlite3_finalize,
    sqlite3_db_handle,

    -- ** Memory allocation subsystem
    sqlite3_free,
    sqlite3_malloc,
    sqlite3_malloc64,
    sqlite3_msize,
    sqlite3_realloc,
    sqlite3_realloc64,

    -- ** Function auxiliary data
    sqlite3_get_auxdata,
    sqlite3_set_auxdata,

    -- ** Impose a limit on heap size
    sqlite3_hard_heap_limit64,
    sqlite3_soft_heap_limit64,

    -- ** Initialize the SQLite library
    sqlite3_initialize,
    sqlite3_shutdown,

    -- ** Interrupt a long-running query
    sqlite3_interrupt,

    -- ** SQL keyword checking
    sqlite3_keyword_check,
    sqlite3_keyword_count,
    sqlite3_keyword_name,

    -- ** Last insert rowid
    sqlite3_last_insert_rowid,

    -- ** Runtime library version numbers
    sqlite3_libversion,
    sqlite3_libversion_number,
    sqlite3_sourceid,
    sqlite3_version,

    -- ** Runtime limits
    sqlite3_limit,

    -- ** Load an extension
    sqlite3_load_extension,

    -- ** Error logging interface
    sqlite3_log,

    -- ** Memory allocator statistics
    sqlite3_memory_highwater,
    sqlite3_memory_used,

    -- ** Mutexes
    sqlite3_mutex_alloc,
    sqlite3_mutex_enter,
    sqlite3_mutex_free,
    sqlite3_mutex_leave,
    sqlite3_mutex_try,

    -- ** Mutex verification routines
    sqlite3_mutex_held,
    sqlite3_mutex_notheld,

    -- ** Find the next prepared statement
    sqlite3_next_stmt,

    -- ** Overload a function for a virtual table
    sqlite3_overload_function,

    -- ** The pre-update hook
    sqlite3_preupdate_blobwrite,
    sqlite3_preupdate_count,
    sqlite3_preupdate_depth,
    sqlite3_preupdate_hook,
    sqlite3_preupdate_new,
    sqlite3_preupdate_old,

    -- ** SQL trace hook
    sqlite3_trace_v2,

    -- ** Query progress callbacks
    sqlite3_progress_handler,

    -- ** Pseudo-random number generator
    sqlite3_randomness,

    -- ** Attempt to free heap memory
    sqlite3_release_memory,

    -- ** Reset a prepared statement object
    sqlite3_reset,

    -- ** Serialize a database
    sqlite3_serialize,

    -- ** Compile-time authorization callbacks
    sqlite3_set_authorizer,

    -- ** Set the last insert rowid value
    sqlite3_set_last_insert_rowid,

    -- ** Suspend execution for a short time
    sqlite3_sleep,

    -- ** Snapshots
    sqlite3_snapshot_cmp,
    sqlite3_snapshot_free,
    sqlite3_snapshot_get,
    sqlite3_snapshot_open,
    sqlite3_snapshot_recover,

    -- ** Formatted string printing functions
    sqlite3_mprintf,
    sqlite3_snprintf,
    sqlite3_vmprintf,
    sqlite3_vsnprintf,

    -- ** SQLite runtime status
    sqlite3_status,
    sqlite3_status64,

    -- ** Evaluate an SQL statement
    sqlite3_step__safe,
    sqlite3_step__unsafe,

    -- ** Determine if a prepared statement has been reset
    sqlite3_stmt_busy,

    -- ** Query the `EXPLAIN` setting for a prepared statement
    sqlite3_stmt_isexplain,

    -- ** Determine if an SQL statement writes the database
    sqlite3_stmt_readonly,

    -- ** Prepared statement scan status
    sqlite3_stmt_scanstatus,

    -- ** Zero scan-status counters
    sqlite3_stmt_scanstatus_reset,

    -- ** Prepared statement status
    sqlite3_stmt_status,

    -- ** Dynamic strings
    sqlite3_str_new,
    sqlite3_str_finish,
    sqlite3_str_append,
    sqlite3_str_appendall,
    sqlite3_str_appendchar,
    sqlite3_str_appendf,
    sqlite3_str_reset,
    sqlite3_str_vappendf,
    sqlite3_str_errcode,
    sqlite3_str_length,
    sqlite3_str_value,

    -- ** String globbing
    sqlite3_strglob,

    -- ** String comparison
    sqlite3_stricmp,
    sqlite3_strnicmp,

    -- ** String `LIKE` matching
    sqlite3_strlike,

    -- ** Low-level system error code
    sqlite3_system_errno,

    -- ** Extract metadata about a column of a table
    sqlite3_table_column_metadata,

    -- ** Test to see if the library is threadsafe
    sqlite3_threadsafe,

    -- ** Total number of rows modified
    sqlite3_total_changes,
    sqlite3_total_changes64,

    -- ** Determine the transaction state of a database
    sqlite3_txn_state,

    -- ** Unlock notification
    sqlite3_unlock_notify,

    -- ** Data change notification callbacks
    sqlite3_update_hook,

    -- ** Obtain values for URI parameters
    sqlite3_uri_boolean,
    sqlite3_uri_int64,
    sqlite3_uri_key,
    sqlite3_uri_parameter,

    -- ** SQL values

    -- *** Copy and free SQL values
    sqlite3_value_dup,
    sqlite3_value_free,

    -- *** Finding the subtype of SQL values
    sqlite3_value_subtype,

    -- *** Obtaining SQL values
    sqlite3_value_blob,
    sqlite3_value_bytes,
    sqlite3_value_double,
    sqlite3_value_frombind,
    sqlite3_value_int,
    sqlite3_value_int64,
    sqlite3_value_nochange,
    sqlite3_value_numeric_type,
    sqlite3_value_pointer,
    sqlite3_value_text,
    sqlite3_value_type,

    -- ** Virtual file systems,
    sqlite3_vfs_register,
    sqlite3_vfs_unregister,
    sqlite3_vfs_find,
    sqlite3_database_file_object,
    sqlite3_create_filename,
    sqlite3_free_filename,

    -- ** Virtual tables
    sqlite3_vtab_collation,
    sqlite3_vtab_config,
    sqlite3_vtab_distinct,
    sqlite3_vtab_in,
    sqlite3_vtab_in_first,
    sqlite3_vtab_in_next,
    sqlite3_vtab_nochange,
    sqlite3_vtab_on_conflict,
    sqlite3_vtab_rhs_value,

    -- ** Write-ahead log
    sqlite3_wal_checkpoint,
    sqlite3_wal_checkpoint_v2,
    sqlite3_wal_autocheckpoint,
    sqlite3_wal_hook,

    -- * Constants
    _SQLITE_ABORT,
    _SQLITE_ABORT_ROLLBACK,
    _SQLITE_ACCESS_EXISTS,
    _SQLITE_ACCESS_READ,
    _SQLITE_ACCESS_READWRITE,
    _SQLITE_ALTER_TABLE,
    _SQLITE_ANALYZE,
    _SQLITE_ANY,
    _SQLITE_ATTACH,
    _SQLITE_AUTH,
    _SQLITE_AUTH_USER,
    _SQLITE_BLOB,
    _SQLITE_BUSY,
    _SQLITE_BUSY_RECOVERY,
    _SQLITE_BUSY_SNAPSHOT,
    _SQLITE_BUSY_TIMEOUT,
    _SQLITE_CANTOPEN,
    _SQLITE_CANTOPEN_CONVPATH,
    _SQLITE_CANTOPEN_DIRTYWAL,
    _SQLITE_CANTOPEN_FULLPATH,
    _SQLITE_CANTOPEN_ISDIR,
    _SQLITE_CANTOPEN_NOTEMPDIR,
    _SQLITE_CANTOPEN_SYMLINK,
    _SQLITE_CHECKPOINT_FULL,
    _SQLITE_CHECKPOINT_PASSIVE,
    _SQLITE_CHECKPOINT_RESTART,
    _SQLITE_CHECKPOINT_TRUNCATE,
    _SQLITE_CONFIG_COVERING_INDEX_SCAN,
    _SQLITE_CONFIG_GETMALLOC,
    _SQLITE_CONFIG_GETMUTEX,
    _SQLITE_CONFIG_GETPCACHE,
    _SQLITE_CONFIG_GETPCACHE2,
    _SQLITE_CONFIG_HEAP,
    _SQLITE_CONFIG_LOG,
    _SQLITE_CONFIG_LOOKASIDE,
    _SQLITE_CONFIG_MALLOC,
    _SQLITE_CONFIG_MEMDB_MAXSIZE,
    _SQLITE_CONFIG_MEMSTATUS,
    _SQLITE_CONFIG_MMAP_SIZE,
    _SQLITE_CONFIG_MULTITHREAD,
    _SQLITE_CONFIG_MUTEX,
    _SQLITE_CONFIG_PAGECACHE,
    _SQLITE_CONFIG_PCACHE,
    _SQLITE_CONFIG_PCACHE2,
    _SQLITE_CONFIG_PCACHE_HDRSZ,
    _SQLITE_CONFIG_PMASZ,
    _SQLITE_CONFIG_SCRATCH,
    _SQLITE_CONFIG_SERIALIZED,
    _SQLITE_CONFIG_SINGLETHREAD,
    _SQLITE_CONFIG_SMALL_MALLOC,
    _SQLITE_CONFIG_SORTERREF_SIZE,
    _SQLITE_CONFIG_SQLLOG,
    _SQLITE_CONFIG_STMTJRNL_SPILL,
    _SQLITE_CONFIG_URI,
    _SQLITE_CONFIG_WIN32_HEAPSIZE,
    _SQLITE_CONSTRAINT,
    _SQLITE_CONSTRAINT_CHECK,
    _SQLITE_CONSTRAINT_COMMITHOOK,
    _SQLITE_CONSTRAINT_DATATYPE,
    _SQLITE_CONSTRAINT_FOREIGNKEY,
    _SQLITE_CONSTRAINT_FUNCTION,
    _SQLITE_CONSTRAINT_NOTNULL,
    _SQLITE_CONSTRAINT_PINNED,
    _SQLITE_CONSTRAINT_PRIMARYKEY,
    _SQLITE_CONSTRAINT_ROWID,
    _SQLITE_CONSTRAINT_TRIGGER,
    _SQLITE_CONSTRAINT_UNIQUE,
    _SQLITE_CONSTRAINT_VTAB,
    _SQLITE_COPY,
    _SQLITE_CORRUPT,
    _SQLITE_CORRUPT_INDEX,
    _SQLITE_CORRUPT_SEQUENCE,
    _SQLITE_CORRUPT_VTAB,
    _SQLITE_CREATE_INDEX,
    _SQLITE_CREATE_TABLE,
    _SQLITE_CREATE_TEMP_INDEX,
    _SQLITE_CREATE_TEMP_TABLE,
    _SQLITE_CREATE_TEMP_TRIGGER,
    _SQLITE_CREATE_TEMP_VIEW,
    _SQLITE_CREATE_TRIGGER,
    _SQLITE_CREATE_VIEW,
    _SQLITE_CREATE_VTABLE,
    _SQLITE_DBCONFIG_DEFENSIVE,
    _SQLITE_DBCONFIG_DQS_DDL,
    _SQLITE_DBCONFIG_DQS_DML,
    _SQLITE_DBCONFIG_ENABLE_FKEY,
    _SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER,
    _SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION,
    _SQLITE_DBCONFIG_ENABLE_QPSG,
    _SQLITE_DBCONFIG_ENABLE_TRIGGER,
    _SQLITE_DBCONFIG_ENABLE_VIEW,
    _SQLITE_DBCONFIG_LEGACY_ALTER_TABLE,
    _SQLITE_DBCONFIG_LEGACY_FILE_FORMAT,
    _SQLITE_DBCONFIG_LOOKASIDE,
    _SQLITE_DBCONFIG_MAINDBNAME,
    _SQLITE_DBCONFIG_MAX,
    _SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE,
    _SQLITE_DBCONFIG_RESET_DATABASE,
    _SQLITE_DBCONFIG_TRIGGER_EQP,
    _SQLITE_DBCONFIG_TRUSTED_SCHEMA,
    _SQLITE_DBCONFIG_WRITABLE_SCHEMA,
    _SQLITE_DBSTATUS_CACHE_HIT,
    _SQLITE_DBSTATUS_CACHE_MISS,
    _SQLITE_DBSTATUS_CACHE_SPILL,
    _SQLITE_DBSTATUS_CACHE_USED,
    _SQLITE_DBSTATUS_CACHE_USED_SHARED,
    _SQLITE_DBSTATUS_CACHE_WRITE,
    _SQLITE_DBSTATUS_DEFERRED_FKS,
    _SQLITE_DBSTATUS_LOOKASIDE_HIT,
    _SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL,
    _SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE,
    _SQLITE_DBSTATUS_LOOKASIDE_USED,
    _SQLITE_DBSTATUS_MAX,
    _SQLITE_DBSTATUS_SCHEMA_USED,
    _SQLITE_DBSTATUS_STMT_USED,
    _SQLITE_DELETE,
    _SQLITE_DENY,
    _SQLITE_DESERIALIZE_FREEONCLOSE,
    _SQLITE_DESERIALIZE_READONLY,
    _SQLITE_DESERIALIZE_RESIZEABLE,
    _SQLITE_DETACH,
    _SQLITE_DETERMINISTIC,
    _SQLITE_DIRECTONLY,
    _SQLITE_DONE,
    _SQLITE_DROP_INDEX,
    _SQLITE_DROP_TABLE,
    _SQLITE_DROP_TEMP_INDEX,
    _SQLITE_DROP_TEMP_TABLE,
    _SQLITE_DROP_TEMP_TRIGGER,
    _SQLITE_DROP_TEMP_VIEW,
    _SQLITE_DROP_TRIGGER,
    _SQLITE_DROP_VIEW,
    _SQLITE_DROP_VTABLE,
    _SQLITE_EMPTY,
    _SQLITE_ERROR,
    _SQLITE_ERROR_MISSING_COLLSEQ,
    _SQLITE_ERROR_RETRY,
    _SQLITE_ERROR_SNAPSHOT,
    _SQLITE_FAIL,
    _SQLITE_FCNTL_BEGIN_ATOMIC_WRITE,
    _SQLITE_FCNTL_BUSYHANDLER,
    _SQLITE_FCNTL_CHUNK_SIZE,
    _SQLITE_FCNTL_CKPT_DONE,
    _SQLITE_FCNTL_CKPT_START,
    _SQLITE_FCNTL_CKSM_FILE,
    _SQLITE_FCNTL_COMMIT_ATOMIC_WRITE,
    _SQLITE_FCNTL_COMMIT_PHASETWO,
    _SQLITE_FCNTL_DATA_VERSION,
    _SQLITE_FCNTL_EXTERNAL_READER,
    _SQLITE_FCNTL_FILE_POINTER,
    _SQLITE_FCNTL_GET_LOCKPROXYFILE,
    _SQLITE_FCNTL_HAS_MOVED,
    _SQLITE_FCNTL_JOURNAL_POINTER,
    _SQLITE_FCNTL_LAST_ERRNO,
    _SQLITE_FCNTL_LOCKSTATE,
    _SQLITE_FCNTL_LOCK_TIMEOUT,
    _SQLITE_FCNTL_MMAP_SIZE,
    _SQLITE_FCNTL_OVERWRITE,
    _SQLITE_FCNTL_PDB,
    _SQLITE_FCNTL_PERSIST_WAL,
    _SQLITE_FCNTL_POWERSAFE_OVERWRITE,
    _SQLITE_FCNTL_PRAGMA,
    _SQLITE_FCNTL_RBU,
    _SQLITE_FCNTL_RESERVE_BYTES,
    _SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE,
    _SQLITE_FCNTL_SET_LOCKPROXYFILE,
    _SQLITE_FCNTL_SIZE_HINT,
    _SQLITE_FCNTL_SIZE_LIMIT,
    _SQLITE_FCNTL_SYNC,
    _SQLITE_FCNTL_SYNC_OMITTED,
    _SQLITE_FCNTL_TEMPFILENAME,
    _SQLITE_FCNTL_TRACE,
    _SQLITE_FCNTL_VFSNAME,
    _SQLITE_FCNTL_VFS_POINTER,
    _SQLITE_FCNTL_WAL_BLOCK,
    _SQLITE_FCNTL_WIN32_AV_RETRY,
    _SQLITE_FCNTL_WIN32_GET_HANDLE,
    _SQLITE_FCNTL_WIN32_SET_HANDLE,
    _SQLITE_FCNTL_ZIPVFS,
    _SQLITE_FLOAT,
    _SQLITE_FORMAT,
    _SQLITE_FULL,
    _SQLITE_FUNCTION,
    _SQLITE_IGNORE,
    _SQLITE_INDEX_CONSTRAINT_EQ,
    _SQLITE_INDEX_CONSTRAINT_FUNCTION,
    _SQLITE_INDEX_CONSTRAINT_GE,
    _SQLITE_INDEX_CONSTRAINT_GLOB,
    _SQLITE_INDEX_CONSTRAINT_GT,
    _SQLITE_INDEX_CONSTRAINT_IS,
    _SQLITE_INDEX_CONSTRAINT_ISNOT,
    _SQLITE_INDEX_CONSTRAINT_ISNOTNULL,
    _SQLITE_INDEX_CONSTRAINT_ISNULL,
    _SQLITE_INDEX_CONSTRAINT_LE,
    _SQLITE_INDEX_CONSTRAINT_LIKE,
    _SQLITE_INDEX_CONSTRAINT_LIMIT,
    _SQLITE_INDEX_CONSTRAINT_LT,
    _SQLITE_INDEX_CONSTRAINT_MATCH,
    _SQLITE_INDEX_CONSTRAINT_NE,
    _SQLITE_INDEX_CONSTRAINT_OFFSET,
    _SQLITE_INDEX_CONSTRAINT_REGEXP,
    _SQLITE_INDEX_SCAN_UNIQUE,
    _SQLITE_INNOCUOUS,
    _SQLITE_INSERT,
    _SQLITE_INTEGER,
    _SQLITE_INTERNAL,
    _SQLITE_INTERRUPT,
    _SQLITE_IOCAP_ATOMIC,
    _SQLITE_IOCAP_ATOMIC16K,
    _SQLITE_IOCAP_ATOMIC1K,
    _SQLITE_IOCAP_ATOMIC2K,
    _SQLITE_IOCAP_ATOMIC32K,
    _SQLITE_IOCAP_ATOMIC4K,
    _SQLITE_IOCAP_ATOMIC512,
    _SQLITE_IOCAP_ATOMIC64K,
    _SQLITE_IOCAP_ATOMIC8K,
    _SQLITE_IOCAP_BATCH_ATOMIC,
    _SQLITE_IOCAP_IMMUTABLE,
    _SQLITE_IOCAP_POWERSAFE_OVERWRITE,
    _SQLITE_IOCAP_SAFE_APPEND,
    _SQLITE_IOCAP_SEQUENTIAL,
    _SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN,
    _SQLITE_IOERR,
    _SQLITE_IOERR_ACCESS,
    _SQLITE_IOERR_AUTH,
    _SQLITE_IOERR_BEGIN_ATOMIC,
    _SQLITE_IOERR_BLOCKED,
    _SQLITE_IOERR_CHECKRESERVEDLOCK,
    _SQLITE_IOERR_CLOSE,
    _SQLITE_IOERR_COMMIT_ATOMIC,
    _SQLITE_IOERR_CONVPATH,
    _SQLITE_IOERR_CORRUPTFS,
    _SQLITE_IOERR_DATA,
    _SQLITE_IOERR_DELETE,
    _SQLITE_IOERR_DELETE_NOENT,
    _SQLITE_IOERR_DIR_CLOSE,
    _SQLITE_IOERR_DIR_FSYNC,
    _SQLITE_IOERR_FSTAT,
    _SQLITE_IOERR_FSYNC,
    _SQLITE_IOERR_GETTEMPPATH,
    _SQLITE_IOERR_LOCK,
    _SQLITE_IOERR_MMAP,
    _SQLITE_IOERR_NOMEM,
    _SQLITE_IOERR_RDLOCK,
    _SQLITE_IOERR_READ,
    _SQLITE_IOERR_ROLLBACK_ATOMIC,
    _SQLITE_IOERR_SEEK,
    _SQLITE_IOERR_SHMLOCK,
    _SQLITE_IOERR_SHMMAP,
    _SQLITE_IOERR_SHMOPEN,
    _SQLITE_IOERR_SHMSIZE,
    _SQLITE_IOERR_SHORT_READ,
    _SQLITE_IOERR_TRUNCATE,
    _SQLITE_IOERR_UNLOCK,
    _SQLITE_IOERR_VNODE,
    _SQLITE_IOERR_WRITE,
    _SQLITE_LIMIT_ATTACHED,
    _SQLITE_LIMIT_COLUMN,
    _SQLITE_LIMIT_COMPOUND_SELECT,
    _SQLITE_LIMIT_EXPR_DEPTH,
    _SQLITE_LIMIT_FUNCTION_ARG,
    _SQLITE_LIMIT_LENGTH,
    _SQLITE_LIMIT_LIKE_PATTERN_LENGTH,
    _SQLITE_LIMIT_SQL_LENGTH,
    _SQLITE_LIMIT_TRIGGER_DEPTH,
    _SQLITE_LIMIT_VARIABLE_NUMBER,
    _SQLITE_LIMIT_VDBE_OP,
    _SQLITE_LIMIT_WORKER_THREADS,
    _SQLITE_LOCKED,
    _SQLITE_LOCKED_SHAREDCACHE,
    _SQLITE_LOCKED_VTAB,
    _SQLITE_LOCK_EXCLUSIVE,
    _SQLITE_LOCK_NONE,
    _SQLITE_LOCK_PENDING,
    _SQLITE_LOCK_RESERVED,
    _SQLITE_LOCK_SHARED,
    _SQLITE_MISMATCH,
    _SQLITE_MISUSE,
    _SQLITE_MUTEX_FAST,
    _SQLITE_MUTEX_RECURSIVE,
    _SQLITE_MUTEX_STATIC_APP1,
    _SQLITE_MUTEX_STATIC_APP2,
    _SQLITE_MUTEX_STATIC_APP3,
    _SQLITE_MUTEX_STATIC_LRU,
    _SQLITE_MUTEX_STATIC_LRU2,
    _SQLITE_MUTEX_STATIC_MAIN,
    _SQLITE_MUTEX_STATIC_MEM,
    _SQLITE_MUTEX_STATIC_MEM2,
    _SQLITE_MUTEX_STATIC_OPEN,
    _SQLITE_MUTEX_STATIC_PMEM,
    _SQLITE_MUTEX_STATIC_PRNG,
    _SQLITE_MUTEX_STATIC_VFS1,
    _SQLITE_MUTEX_STATIC_VFS2,
    _SQLITE_MUTEX_STATIC_VFS3,
    _SQLITE_NOLFS,
    _SQLITE_NOMEM,
    _SQLITE_NOTADB,
    _SQLITE_NOTFOUND,
    _SQLITE_NOTICE,
    _SQLITE_NOTICE_RECOVER_ROLLBACK,
    _SQLITE_NOTICE_RECOVER_WAL,
    _SQLITE_NULL,
    _SQLITE_OK,
    _SQLITE_OK_LOAD_PERMANENTLY,
    _SQLITE_OK_SYMLINK,
    _SQLITE_OPEN_AUTOPROXY,
    _SQLITE_OPEN_CREATE,
    _SQLITE_OPEN_DELETEONCLOSE,
    _SQLITE_OPEN_EXCLUSIVE,
    _SQLITE_OPEN_EXRESCODE,
    _SQLITE_OPEN_FULLMUTEX,
    _SQLITE_OPEN_MAIN_DB,
    _SQLITE_OPEN_MAIN_JOURNAL,
    _SQLITE_OPEN_MEMORY,
    _SQLITE_OPEN_NOFOLLOW,
    _SQLITE_OPEN_NOMUTEX,
    _SQLITE_OPEN_PRIVATECACHE,
    _SQLITE_OPEN_READONLY,
    _SQLITE_OPEN_READWRITE,
    _SQLITE_OPEN_SHAREDCACHE,
    _SQLITE_OPEN_SUBJOURNAL,
    _SQLITE_OPEN_SUPER_JOURNAL,
    _SQLITE_OPEN_TEMP_DB,
    _SQLITE_OPEN_TEMP_JOURNAL,
    _SQLITE_OPEN_TRANSIENT_DB,
    _SQLITE_OPEN_URI,
    _SQLITE_OPEN_WAL,
    _SQLITE_PERM,
    _SQLITE_PRAGMA,
    _SQLITE_PREPARE_NORMALIZE,
    _SQLITE_PREPARE_NO_VTAB,
    _SQLITE_PREPARE_PERSISTENT,
    _SQLITE_PROTOCOL,
    _SQLITE_RANGE,
    _SQLITE_READ,
    _SQLITE_READONLY,
    _SQLITE_READONLY_CANTINIT,
    _SQLITE_READONLY_CANTLOCK,
    _SQLITE_READONLY_DBMOVED,
    _SQLITE_READONLY_DIRECTORY,
    _SQLITE_READONLY_RECOVERY,
    _SQLITE_READONLY_ROLLBACK,
    _SQLITE_RECURSIVE,
    _SQLITE_REINDEX,
    _SQLITE_REPLACE,
    _SQLITE_ROLLBACK,
    _SQLITE_ROW,
    _SQLITE_SAVEPOINT,
    _SQLITE_SCANSTAT_EST,
    _SQLITE_SCANSTAT_EXPLAIN,
    _SQLITE_SCANSTAT_NAME,
    _SQLITE_SCANSTAT_NLOOP,
    _SQLITE_SCANSTAT_NVISIT,
    _SQLITE_SCANSTAT_SELECTID,
    _SQLITE_SCHEMA,
    _SQLITE_SELECT,
    _SQLITE_SERIALIZE_NOCOPY,
    _SQLITE_SHM_EXCLUSIVE,
    _SQLITE_SHM_LOCK,
    _SQLITE_SHM_NLOCK,
    _SQLITE_SHM_SHARED,
    _SQLITE_SHM_UNLOCK,
    _SQLITE_SOURCE_ID,
    _SQLITE_STATIC,
    _SQLITE_STATUS_MALLOC_COUNT,
    _SQLITE_STATUS_MALLOC_SIZE,
    _SQLITE_STATUS_MEMORY_USED,
    _SQLITE_STATUS_PAGECACHE_OVERFLOW,
    _SQLITE_STATUS_PAGECACHE_SIZE,
    _SQLITE_STATUS_PAGECACHE_USED,
    _SQLITE_STATUS_PARSER_STACK,
    _SQLITE_STATUS_SCRATCH_OVERFLOW,
    _SQLITE_STATUS_SCRATCH_SIZE,
    _SQLITE_STATUS_SCRATCH_USED,
    _SQLITE_STMTSTATUS_AUTOINDEX,
    _SQLITE_STMTSTATUS_FILTER_HIT,
    _SQLITE_STMTSTATUS_FILTER_MISS,
    _SQLITE_STMTSTATUS_FULLSCAN_STEP,
    _SQLITE_STMTSTATUS_MEMUSED,
    _SQLITE_STMTSTATUS_REPREPARE,
    _SQLITE_STMTSTATUS_RUN,
    _SQLITE_STMTSTATUS_SORT,
    _SQLITE_STMTSTATUS_VM_STEP,
    _SQLITE_SUBTYPE,
    _SQLITE_SYNC_DATAONLY,
    _SQLITE_SYNC_FULL,
    _SQLITE_SYNC_NORMAL,
    _SQLITE_TESTCTRL_ALWAYS,
    _SQLITE_TESTCTRL_ASSERT,
    _SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS,
    _SQLITE_TESTCTRL_BITVEC_TEST,
    _SQLITE_TESTCTRL_BYTEORDER,
    _SQLITE_TESTCTRL_EXPLAIN_STMT,
    _SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS,
    _SQLITE_TESTCTRL_FAULT_INSTALL,
    _SQLITE_TESTCTRL_FIRST,
    _SQLITE_TESTCTRL_IMPOSTER,
    _SQLITE_TESTCTRL_INTERNAL_FUNCTIONS,
    _SQLITE_TESTCTRL_ISINIT,
    _SQLITE_TESTCTRL_ISKEYWORD,
    _SQLITE_TESTCTRL_LAST,
    _SQLITE_TESTCTRL_LOCALTIME_FAULT,
    _SQLITE_TESTCTRL_LOGEST,
    _SQLITE_TESTCTRL_NEVER_CORRUPT,
    _SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD,
    _SQLITE_TESTCTRL_OPTIMIZATIONS,
    _SQLITE_TESTCTRL_PARSER_COVERAGE,
    _SQLITE_TESTCTRL_PENDING_BYTE,
    _SQLITE_TESTCTRL_PRNG_RESET,
    _SQLITE_TESTCTRL_PRNG_RESTORE,
    _SQLITE_TESTCTRL_PRNG_SAVE,
    _SQLITE_TESTCTRL_PRNG_SEED,
    _SQLITE_TESTCTRL_RESERVE,
    _SQLITE_TESTCTRL_RESULT_INTREAL,
    _SQLITE_TESTCTRL_SCRATCHMALLOC,
    _SQLITE_TESTCTRL_SEEK_COUNT,
    _SQLITE_TESTCTRL_SORTER_MMAP,
    _SQLITE_TESTCTRL_TRACEFLAGS,
    _SQLITE_TESTCTRL_TUNE,
    _SQLITE_TESTCTRL_VDBE_COVERAGE,
    _SQLITE_TEXT,
    _SQLITE_TOOBIG,
    _SQLITE_TRACE_CLOSE,
    _SQLITE_TRACE_PROFILE,
    _SQLITE_TRACE_ROW,
    _SQLITE_TRACE_STMT,
    _SQLITE_TRANSACTION,
    _SQLITE_TRANSIENT,
    _SQLITE_TXN_NONE,
    _SQLITE_TXN_READ,
    _SQLITE_TXN_WRITE,
    _SQLITE_UPDATE,
    _SQLITE_UTF16,
    _SQLITE_UTF16BE,
    _SQLITE_UTF16LE,
    _SQLITE_UTF16_ALIGNED,
    _SQLITE_UTF8,
    _SQLITE_VERSION,
    _SQLITE_VERSION_NUMBER,
    _SQLITE_VTAB_CONSTRAINT_SUPPORT,
    _SQLITE_VTAB_DIRECTONLY,
    _SQLITE_VTAB_INNOCUOUS,
    _SQLITE_WARNING,
    _SQLITE_WARNING_AUTOINDEX,
    _SQLITE_WIN32_DATA_DIRECTORY_TYPE,
    _SQLITE_WIN32_TEMP_DIRECTORY_TYPE,
  )
where

import Sqlite.Bindings.Internal.Constants
import Sqlite.Bindings.Internal.Functions
import Sqlite.Bindings.Internal.Objects
