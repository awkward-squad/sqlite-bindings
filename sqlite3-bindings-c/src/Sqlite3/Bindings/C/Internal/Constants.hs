-- GHC thinks the SQLITE_STATIC/SQLITE_TRANSIENT are dodgy because they're function pointers that don't begin with "&"
{-# OPTIONS_GHC -fno-warn-dodgy-foreign-imports #-}

module Sqlite3.Bindings.C.Internal.Constants where

import Foreign.C (CInt (..), CString)
import Foreign.Ptr (FunPtr, Ptr)

foreign import capi "sqlite3.h value SQLITE_ABORT" _SQLITE_ABORT :: CInt

foreign import capi "sqlite3.h value SQLITE_ABORT_ROLLBACK" _SQLITE_ABORT_ROLLBACK :: CInt

foreign import capi "sqlite3.h value SQLITE_ACCESS_EXISTS" _SQLITE_ACCESS_EXISTS :: CInt

foreign import capi "sqlite3.h value SQLITE_ACCESS_READ" _SQLITE_ACCESS_READ :: CInt

foreign import capi "sqlite3.h value SQLITE_ACCESS_READWRITE" _SQLITE_ACCESS_READWRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_ALTER_TABLE" _SQLITE_ALTER_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_ANALYZE" _SQLITE_ANALYZE :: CInt

foreign import capi "sqlite3.h value SQLITE_ANY" _SQLITE_ANY :: CInt

foreign import capi "sqlite3.h value SQLITE_ATTACH" _SQLITE_ATTACH :: CInt

foreign import capi "sqlite3.h value SQLITE_AUTH" _SQLITE_AUTH :: CInt

foreign import capi "sqlite3.h value SQLITE_AUTH_USER" _SQLITE_AUTH_USER :: CInt

foreign import capi "sqlite3.h value SQLITE_BLOB" _SQLITE_BLOB :: CInt

foreign import capi "sqlite3.h value SQLITE_BUSY" _SQLITE_BUSY :: CInt

foreign import capi "sqlite3.h value SQLITE_BUSY_RECOVERY" _SQLITE_BUSY_RECOVERY :: CInt

foreign import capi "sqlite3.h value SQLITE_BUSY_SNAPSHOT" _SQLITE_BUSY_SNAPSHOT :: CInt

foreign import capi "sqlite3.h value SQLITE_BUSY_TIMEOUT" _SQLITE_BUSY_TIMEOUT :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN" _SQLITE_CANTOPEN :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_CONVPATH" _SQLITE_CANTOPEN_CONVPATH :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_DIRTYWAL" _SQLITE_CANTOPEN_DIRTYWAL :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_FULLPATH" _SQLITE_CANTOPEN_FULLPATH :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_ISDIR" _SQLITE_CANTOPEN_ISDIR :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_NOTEMPDIR" _SQLITE_CANTOPEN_NOTEMPDIR :: CInt

foreign import capi "sqlite3.h value SQLITE_CANTOPEN_SYMLINK" _SQLITE_CANTOPEN_SYMLINK :: CInt

foreign import capi "sqlite3.h value SQLITE_CHECKPOINT_FULL" _SQLITE_CHECKPOINT_FULL :: CInt

foreign import capi "sqlite3.h value SQLITE_CHECKPOINT_PASSIVE" _SQLITE_CHECKPOINT_PASSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_CHECKPOINT_RESTART" _SQLITE_CHECKPOINT_RESTART :: CInt

foreign import capi "sqlite3.h value SQLITE_CHECKPOINT_TRUNCATE" _SQLITE_CHECKPOINT_TRUNCATE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_COVERING_INDEX_SCAN" _SQLITE_CONFIG_COVERING_INDEX_SCAN :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_GETMALLOC" _SQLITE_CONFIG_GETMALLOC :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_GETMUTEX" _SQLITE_CONFIG_GETMUTEX :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_GETPCACHE2" _SQLITE_CONFIG_GETPCACHE2 :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_HEAP" _SQLITE_CONFIG_HEAP :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_LOG" _SQLITE_CONFIG_LOG :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_LOOKASIDE" _SQLITE_CONFIG_LOOKASIDE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MALLOC" _SQLITE_CONFIG_MALLOC :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MEMDB_MAXSIZE" _SQLITE_CONFIG_MEMDB_MAXSIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MEMSTATUS" _SQLITE_CONFIG_MEMSTATUS :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MMAP_SIZE" _SQLITE_CONFIG_MMAP_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MULTITHREAD" _SQLITE_CONFIG_MULTITHREAD :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_MUTEX" _SQLITE_CONFIG_MUTEX :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_PAGECACHE" _SQLITE_CONFIG_PAGECACHE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_PCACHE2" _SQLITE_CONFIG_PCACHE2 :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_PCACHE_HDRSZ" _SQLITE_CONFIG_PCACHE_HDRSZ :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_PMASZ" _SQLITE_CONFIG_PMASZ :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_SERIALIZED" _SQLITE_CONFIG_SERIALIZED :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_SINGLETHREAD" _SQLITE_CONFIG_SINGLETHREAD :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_SMALL_MALLOC" _SQLITE_CONFIG_SMALL_MALLOC :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_SORTERREF_SIZE" _SQLITE_CONFIG_SORTERREF_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_SQLLOG" _SQLITE_CONFIG_SQLLOG :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_STMTJRNL_SPILL" _SQLITE_CONFIG_STMTJRNL_SPILL :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_URI" _SQLITE_CONFIG_URI :: CInt

foreign import capi "sqlite3.h value SQLITE_CONFIG_WIN32_HEAPSIZE" _SQLITE_CONFIG_WIN32_HEAPSIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT" _SQLITE_CONSTRAINT :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_CHECK" _SQLITE_CONSTRAINT_CHECK :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_COMMITHOOK" _SQLITE_CONSTRAINT_COMMITHOOK :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_DATATYPE" _SQLITE_CONSTRAINT_DATATYPE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_FOREIGNKEY" _SQLITE_CONSTRAINT_FOREIGNKEY :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_FUNCTION" _SQLITE_CONSTRAINT_FUNCTION :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_NOTNULL" _SQLITE_CONSTRAINT_NOTNULL :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_PINNED" _SQLITE_CONSTRAINT_PINNED :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_PRIMARYKEY" _SQLITE_CONSTRAINT_PRIMARYKEY :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_ROWID" _SQLITE_CONSTRAINT_ROWID :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_TRIGGER" _SQLITE_CONSTRAINT_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_UNIQUE" _SQLITE_CONSTRAINT_UNIQUE :: CInt

foreign import capi "sqlite3.h value SQLITE_CONSTRAINT_VTAB" _SQLITE_CONSTRAINT_VTAB :: CInt

foreign import capi "sqlite3.h value SQLITE_COPY" _SQLITE_COPY :: CInt

foreign import capi "sqlite3.h value SQLITE_CORRUPT" _SQLITE_CORRUPT :: CInt

foreign import capi "sqlite3.h value SQLITE_CORRUPT_INDEX" _SQLITE_CORRUPT_INDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_CORRUPT_SEQUENCE" _SQLITE_CORRUPT_SEQUENCE :: CInt

foreign import capi "sqlite3.h value SQLITE_CORRUPT_VTAB" _SQLITE_CORRUPT_VTAB :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_INDEX" _SQLITE_CREATE_INDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TABLE" _SQLITE_CREATE_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TEMP_INDEX" _SQLITE_CREATE_TEMP_INDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TEMP_TABLE" _SQLITE_CREATE_TEMP_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TEMP_TRIGGER" _SQLITE_CREATE_TEMP_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TEMP_VIEW" _SQLITE_CREATE_TEMP_VIEW :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_TRIGGER" _SQLITE_CREATE_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_VIEW" _SQLITE_CREATE_VIEW :: CInt

foreign import capi "sqlite3.h value SQLITE_CREATE_VTABLE" _SQLITE_CREATE_VTABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_DEFENSIVE" _SQLITE_DBCONFIG_DEFENSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_DQS_DDL" _SQLITE_DBCONFIG_DQS_DDL :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_DQS_DML" _SQLITE_DBCONFIG_DQS_DML :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_FKEY" _SQLITE_DBCONFIG_ENABLE_FKEY :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER" _SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION" _SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_QPSG" _SQLITE_DBCONFIG_ENABLE_QPSG :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_TRIGGER" _SQLITE_DBCONFIG_ENABLE_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_ENABLE_VIEW" _SQLITE_DBCONFIG_ENABLE_VIEW :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_LEGACY_ALTER_TABLE" _SQLITE_DBCONFIG_LEGACY_ALTER_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_LEGACY_FILE_FORMAT" _SQLITE_DBCONFIG_LEGACY_FILE_FORMAT :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_LOOKASIDE" _SQLITE_DBCONFIG_LOOKASIDE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_MAINDBNAME" _SQLITE_DBCONFIG_MAINDBNAME :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_MAX" _SQLITE_DBCONFIG_MAX :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE" _SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_RESET_DATABASE" _SQLITE_DBCONFIG_RESET_DATABASE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_TRIGGER_EQP" _SQLITE_DBCONFIG_TRIGGER_EQP :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_TRUSTED_SCHEMA" _SQLITE_DBCONFIG_TRUSTED_SCHEMA :: CInt

foreign import capi "sqlite3.h value SQLITE_DBCONFIG_WRITABLE_SCHEMA" _SQLITE_DBCONFIG_WRITABLE_SCHEMA :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_HIT" _SQLITE_DBSTATUS_CACHE_HIT :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_MISS" _SQLITE_DBSTATUS_CACHE_MISS :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_SPILL" _SQLITE_DBSTATUS_CACHE_SPILL :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_USED" _SQLITE_DBSTATUS_CACHE_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_USED_SHARED" _SQLITE_DBSTATUS_CACHE_USED_SHARED :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_CACHE_WRITE" _SQLITE_DBSTATUS_CACHE_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_DEFERRED_FKS" _SQLITE_DBSTATUS_DEFERRED_FKS :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_LOOKASIDE_HIT" _SQLITE_DBSTATUS_LOOKASIDE_HIT :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL" _SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE" _SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_LOOKASIDE_USED" _SQLITE_DBSTATUS_LOOKASIDE_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_MAX" _SQLITE_DBSTATUS_MAX :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_SCHEMA_USED" _SQLITE_DBSTATUS_SCHEMA_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_DBSTATUS_STMT_USED" _SQLITE_DBSTATUS_STMT_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_DELETE" _SQLITE_DELETE :: CInt

foreign import capi "sqlite3.h value SQLITE_DENY" _SQLITE_DENY :: CInt

foreign import capi "sqlite3.h value SQLITE_DESERIALIZE_FREEONCLOSE" _SQLITE_DESERIALIZE_FREEONCLOSE :: CInt

foreign import capi "sqlite3.h value SQLITE_DESERIALIZE_READONLY" _SQLITE_DESERIALIZE_READONLY :: CInt

foreign import capi "sqlite3.h value SQLITE_DESERIALIZE_RESIZEABLE" _SQLITE_DESERIALIZE_RESIZEABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_DETACH" _SQLITE_DETACH :: CInt

foreign import capi "sqlite3.h value SQLITE_DETERMINISTIC" _SQLITE_DETERMINISTIC :: CInt

foreign import capi "sqlite3.h value SQLITE_DIRECTONLY" _SQLITE_DIRECTONLY :: CInt

foreign import capi "sqlite3.h value SQLITE_DONE" _SQLITE_DONE :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_INDEX" _SQLITE_DROP_INDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TABLE" _SQLITE_DROP_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TEMP_INDEX" _SQLITE_DROP_TEMP_INDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TEMP_TABLE" _SQLITE_DROP_TEMP_TABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TEMP_TRIGGER" _SQLITE_DROP_TEMP_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TEMP_VIEW" _SQLITE_DROP_TEMP_VIEW :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_TRIGGER" _SQLITE_DROP_TRIGGER :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_VIEW" _SQLITE_DROP_VIEW :: CInt

foreign import capi "sqlite3.h value SQLITE_DROP_VTABLE" _SQLITE_DROP_VTABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_EMPTY" _SQLITE_EMPTY :: CInt

foreign import capi "sqlite3.h value SQLITE_ERROR" _SQLITE_ERROR :: CInt

foreign import capi "sqlite3.h value SQLITE_ERROR_MISSING_COLLSEQ" _SQLITE_ERROR_MISSING_COLLSEQ :: CInt

foreign import capi "sqlite3.h value SQLITE_ERROR_RETRY" _SQLITE_ERROR_RETRY :: CInt

foreign import capi "sqlite3.h value SQLITE_ERROR_SNAPSHOT" _SQLITE_ERROR_SNAPSHOT :: CInt

foreign import capi "sqlite3.h value SQLITE_FAIL" _SQLITE_FAIL :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_BEGIN_ATOMIC_WRITE" _SQLITE_FCNTL_BEGIN_ATOMIC_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_BUSYHANDLER" _SQLITE_FCNTL_BUSYHANDLER :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_CHUNK_SIZE" _SQLITE_FCNTL_CHUNK_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_CKPT_DONE" _SQLITE_FCNTL_CKPT_DONE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_CKPT_START" _SQLITE_FCNTL_CKPT_START :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_CKSM_FILE" _SQLITE_FCNTL_CKSM_FILE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_COMMIT_ATOMIC_WRITE" _SQLITE_FCNTL_COMMIT_ATOMIC_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_COMMIT_PHASETWO" _SQLITE_FCNTL_COMMIT_PHASETWO :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_DATA_VERSION" _SQLITE_FCNTL_DATA_VERSION :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_EXTERNAL_READER" _SQLITE_FCNTL_EXTERNAL_READER :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_FILE_POINTER" _SQLITE_FCNTL_FILE_POINTER :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_GET_LOCKPROXYFILE" _SQLITE_FCNTL_GET_LOCKPROXYFILE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_HAS_MOVED" _SQLITE_FCNTL_HAS_MOVED :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_JOURNAL_POINTER" _SQLITE_FCNTL_JOURNAL_POINTER :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_LAST_ERRNO" _SQLITE_FCNTL_LAST_ERRNO :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_LOCKSTATE" _SQLITE_FCNTL_LOCKSTATE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_LOCK_TIMEOUT" _SQLITE_FCNTL_LOCK_TIMEOUT :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_MMAP_SIZE" _SQLITE_FCNTL_MMAP_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_OVERWRITE" _SQLITE_FCNTL_OVERWRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_PDB" _SQLITE_FCNTL_PDB :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_PERSIST_WAL" _SQLITE_FCNTL_PERSIST_WAL :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_POWERSAFE_OVERWRITE" _SQLITE_FCNTL_POWERSAFE_OVERWRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_PRAGMA" _SQLITE_FCNTL_PRAGMA :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_RBU" _SQLITE_FCNTL_RBU :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_RESERVE_BYTES" _SQLITE_FCNTL_RESERVE_BYTES :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE" _SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_SET_LOCKPROXYFILE" _SQLITE_FCNTL_SET_LOCKPROXYFILE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_SIZE_HINT" _SQLITE_FCNTL_SIZE_HINT :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_SIZE_LIMIT" _SQLITE_FCNTL_SIZE_LIMIT :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_SYNC" _SQLITE_FCNTL_SYNC :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_SYNC_OMITTED" _SQLITE_FCNTL_SYNC_OMITTED :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_TEMPFILENAME" _SQLITE_FCNTL_TEMPFILENAME :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_TRACE" _SQLITE_FCNTL_TRACE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_VFSNAME" _SQLITE_FCNTL_VFSNAME :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_VFS_POINTER" _SQLITE_FCNTL_VFS_POINTER :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_WAL_BLOCK" _SQLITE_FCNTL_WAL_BLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_WIN32_AV_RETRY" _SQLITE_FCNTL_WIN32_AV_RETRY :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_WIN32_GET_HANDLE" _SQLITE_FCNTL_WIN32_GET_HANDLE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_WIN32_SET_HANDLE" _SQLITE_FCNTL_WIN32_SET_HANDLE :: CInt

foreign import capi "sqlite3.h value SQLITE_FCNTL_ZIPVFS" _SQLITE_FCNTL_ZIPVFS :: CInt

foreign import capi "sqlite3.h value SQLITE_FLOAT" _SQLITE_FLOAT :: CInt

foreign import capi "sqlite3.h value SQLITE_FORMAT" _SQLITE_FORMAT :: CInt

foreign import capi "sqlite3.h value SQLITE_FULL" _SQLITE_FULL :: CInt

foreign import capi "sqlite3.h value SQLITE_FUNCTION" _SQLITE_FUNCTION :: CInt

foreign import capi "sqlite3.h value SQLITE_IGNORE" _SQLITE_IGNORE :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_EQ" _SQLITE_INDEX_CONSTRAINT_EQ :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_FUNCTION" _SQLITE_INDEX_CONSTRAINT_FUNCTION :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_GE" _SQLITE_INDEX_CONSTRAINT_GE :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_GLOB" _SQLITE_INDEX_CONSTRAINT_GLOB :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_GT" _SQLITE_INDEX_CONSTRAINT_GT :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_IS" _SQLITE_INDEX_CONSTRAINT_IS :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_ISNOT" _SQLITE_INDEX_CONSTRAINT_ISNOT :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_ISNOTNULL" _SQLITE_INDEX_CONSTRAINT_ISNOTNULL :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_ISNULL" _SQLITE_INDEX_CONSTRAINT_ISNULL :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_LE" _SQLITE_INDEX_CONSTRAINT_LE :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_LIKE" _SQLITE_INDEX_CONSTRAINT_LIKE :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_LIMIT" _SQLITE_INDEX_CONSTRAINT_LIMIT :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_LT" _SQLITE_INDEX_CONSTRAINT_LT :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_MATCH" _SQLITE_INDEX_CONSTRAINT_MATCH :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_NE" _SQLITE_INDEX_CONSTRAINT_NE :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_OFFSET" _SQLITE_INDEX_CONSTRAINT_OFFSET :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_CONSTRAINT_REGEXP" _SQLITE_INDEX_CONSTRAINT_REGEXP :: CInt

foreign import capi "sqlite3.h value SQLITE_INDEX_SCAN_UNIQUE" _SQLITE_INDEX_SCAN_UNIQUE :: CInt

foreign import capi "sqlite3.h value SQLITE_INNOCUOUS" _SQLITE_INNOCUOUS :: CInt

foreign import capi "sqlite3.h value SQLITE_INSERT" _SQLITE_INSERT :: CInt

foreign import capi "sqlite3.h value SQLITE_INTEGER" _SQLITE_INTEGER :: CInt

foreign import capi "sqlite3.h value SQLITE_INTERNAL" _SQLITE_INTERNAL :: CInt

foreign import capi "sqlite3.h value SQLITE_INTERRUPT" _SQLITE_INTERRUPT :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC" _SQLITE_IOCAP_ATOMIC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC16K" _SQLITE_IOCAP_ATOMIC16K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC1K" _SQLITE_IOCAP_ATOMIC1K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC2K" _SQLITE_IOCAP_ATOMIC2K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC32K" _SQLITE_IOCAP_ATOMIC32K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC4K" _SQLITE_IOCAP_ATOMIC4K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC512" _SQLITE_IOCAP_ATOMIC512 :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC64K" _SQLITE_IOCAP_ATOMIC64K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_ATOMIC8K" _SQLITE_IOCAP_ATOMIC8K :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_BATCH_ATOMIC" _SQLITE_IOCAP_BATCH_ATOMIC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_IMMUTABLE" _SQLITE_IOCAP_IMMUTABLE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_POWERSAFE_OVERWRITE" _SQLITE_IOCAP_POWERSAFE_OVERWRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_SAFE_APPEND" _SQLITE_IOCAP_SAFE_APPEND :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_SEQUENTIAL" _SQLITE_IOCAP_SEQUENTIAL :: CInt

foreign import capi "sqlite3.h value SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN" _SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR" _SQLITE_IOERR :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_ACCESS" _SQLITE_IOERR_ACCESS :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_AUTH" _SQLITE_IOERR_AUTH :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_BEGIN_ATOMIC" _SQLITE_IOERR_BEGIN_ATOMIC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_BLOCKED" _SQLITE_IOERR_BLOCKED :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_CHECKRESERVEDLOCK" _SQLITE_IOERR_CHECKRESERVEDLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_CLOSE" _SQLITE_IOERR_CLOSE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_COMMIT_ATOMIC" _SQLITE_IOERR_COMMIT_ATOMIC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_CONVPATH" _SQLITE_IOERR_CONVPATH :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_CORRUPTFS" _SQLITE_IOERR_CORRUPTFS :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_DATA" _SQLITE_IOERR_DATA :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_DELETE" _SQLITE_IOERR_DELETE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_DELETE_NOENT" _SQLITE_IOERR_DELETE_NOENT :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_DIR_CLOSE" _SQLITE_IOERR_DIR_CLOSE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_DIR_FSYNC" _SQLITE_IOERR_DIR_FSYNC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_FSTAT" _SQLITE_IOERR_FSTAT :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_FSYNC" _SQLITE_IOERR_FSYNC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_GETTEMPPATH" _SQLITE_IOERR_GETTEMPPATH :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_LOCK" _SQLITE_IOERR_LOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_MMAP" _SQLITE_IOERR_MMAP :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_NOMEM" _SQLITE_IOERR_NOMEM :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_RDLOCK" _SQLITE_IOERR_RDLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_READ" _SQLITE_IOERR_READ :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_ROLLBACK_ATOMIC" _SQLITE_IOERR_ROLLBACK_ATOMIC :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SEEK" _SQLITE_IOERR_SEEK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SHMLOCK" _SQLITE_IOERR_SHMLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SHMMAP" _SQLITE_IOERR_SHMMAP :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SHMOPEN" _SQLITE_IOERR_SHMOPEN :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SHMSIZE" _SQLITE_IOERR_SHMSIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_SHORT_READ" _SQLITE_IOERR_SHORT_READ :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_TRUNCATE" _SQLITE_IOERR_TRUNCATE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_UNLOCK" _SQLITE_IOERR_UNLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_VNODE" _SQLITE_IOERR_VNODE :: CInt

foreign import capi "sqlite3.h value SQLITE_IOERR_WRITE" _SQLITE_IOERR_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_ATTACHED" _SQLITE_LIMIT_ATTACHED :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_COLUMN" _SQLITE_LIMIT_COLUMN :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_COMPOUND_SELECT" _SQLITE_LIMIT_COMPOUND_SELECT :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_EXPR_DEPTH" _SQLITE_LIMIT_EXPR_DEPTH :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_FUNCTION_ARG" _SQLITE_LIMIT_FUNCTION_ARG :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_LENGTH" _SQLITE_LIMIT_LENGTH :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_LIKE_PATTERN_LENGTH" _SQLITE_LIMIT_LIKE_PATTERN_LENGTH :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_SQL_LENGTH" _SQLITE_LIMIT_SQL_LENGTH :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_TRIGGER_DEPTH" _SQLITE_LIMIT_TRIGGER_DEPTH :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_VARIABLE_NUMBER" _SQLITE_LIMIT_VARIABLE_NUMBER :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_VDBE_OP" _SQLITE_LIMIT_VDBE_OP :: CInt

foreign import capi "sqlite3.h value SQLITE_LIMIT_WORKER_THREADS" _SQLITE_LIMIT_WORKER_THREADS :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCKED" _SQLITE_LOCKED :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCKED_SHAREDCACHE" _SQLITE_LOCKED_SHAREDCACHE :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCKED_VTAB" _SQLITE_LOCKED_VTAB :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCK_EXCLUSIVE" _SQLITE_LOCK_EXCLUSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCK_NONE" _SQLITE_LOCK_NONE :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCK_PENDING" _SQLITE_LOCK_PENDING :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCK_RESERVED" _SQLITE_LOCK_RESERVED :: CInt

foreign import capi "sqlite3.h value SQLITE_LOCK_SHARED" _SQLITE_LOCK_SHARED :: CInt

foreign import capi "sqlite3.h value SQLITE_MISMATCH" _SQLITE_MISMATCH :: CInt

foreign import capi "sqlite3.h value SQLITE_MISUSE" _SQLITE_MISUSE :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_FAST" _SQLITE_MUTEX_FAST :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_RECURSIVE" _SQLITE_MUTEX_RECURSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_APP1" _SQLITE_MUTEX_STATIC_APP1 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_APP2" _SQLITE_MUTEX_STATIC_APP2 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_APP3" _SQLITE_MUTEX_STATIC_APP3 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_LRU" _SQLITE_MUTEX_STATIC_LRU :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_LRU2" _SQLITE_MUTEX_STATIC_LRU2 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_MAIN" _SQLITE_MUTEX_STATIC_MAIN :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_MEM" _SQLITE_MUTEX_STATIC_MEM :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_MEM2" _SQLITE_MUTEX_STATIC_MEM2 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_OPEN" _SQLITE_MUTEX_STATIC_OPEN :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_PMEM" _SQLITE_MUTEX_STATIC_PMEM :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_PRNG" _SQLITE_MUTEX_STATIC_PRNG :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_VFS1" _SQLITE_MUTEX_STATIC_VFS1 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_VFS2" _SQLITE_MUTEX_STATIC_VFS2 :: CInt

foreign import capi "sqlite3.h value SQLITE_MUTEX_STATIC_VFS3" _SQLITE_MUTEX_STATIC_VFS3 :: CInt

foreign import capi "sqlite3.h value SQLITE_NOLFS" _SQLITE_NOLFS :: CInt

foreign import capi "sqlite3.h value SQLITE_NOMEM" _SQLITE_NOMEM :: CInt

foreign import capi "sqlite3.h value SQLITE_NOTADB" _SQLITE_NOTADB :: CInt

foreign import capi "sqlite3.h value SQLITE_NOTFOUND" _SQLITE_NOTFOUND :: CInt

foreign import capi "sqlite3.h value SQLITE_NOTICE" _SQLITE_NOTICE :: CInt

foreign import capi "sqlite3.h value SQLITE_NOTICE_RECOVER_ROLLBACK" _SQLITE_NOTICE_RECOVER_ROLLBACK :: CInt

foreign import capi "sqlite3.h value SQLITE_NOTICE_RECOVER_WAL" _SQLITE_NOTICE_RECOVER_WAL :: CInt

foreign import capi "sqlite3.h value SQLITE_NULL" _SQLITE_NULL :: CInt

foreign import capi "sqlite3.h value SQLITE_OK" _SQLITE_OK :: CInt

foreign import capi "sqlite3.h value SQLITE_OK_LOAD_PERMANENTLY" _SQLITE_OK_LOAD_PERMANENTLY :: CInt

foreign import capi "sqlite3.h value SQLITE_OK_SYMLINK" _SQLITE_OK_SYMLINK :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_AUTOPROXY" _SQLITE_OPEN_AUTOPROXY :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_CREATE" _SQLITE_OPEN_CREATE :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_DELETEONCLOSE" _SQLITE_OPEN_DELETEONCLOSE :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_EXCLUSIVE" _SQLITE_OPEN_EXCLUSIVE :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_EXRESCODE" _SQLITE_OPEN_EXRESCODE :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_FULLMUTEX" _SQLITE_OPEN_FULLMUTEX :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_MAIN_DB" _SQLITE_OPEN_MAIN_DB :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_MAIN_JOURNAL" _SQLITE_OPEN_MAIN_JOURNAL :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_MEMORY" _SQLITE_OPEN_MEMORY :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_NOFOLLOW" _SQLITE_OPEN_NOFOLLOW :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_NOMUTEX" _SQLITE_OPEN_NOMUTEX :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_READONLY" _SQLITE_OPEN_READONLY :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_READWRITE" _SQLITE_OPEN_READWRITE :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_SUBJOURNAL" _SQLITE_OPEN_SUBJOURNAL :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_SUPER_JOURNAL" _SQLITE_OPEN_SUPER_JOURNAL :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_TEMP_DB" _SQLITE_OPEN_TEMP_DB :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_TEMP_JOURNAL" _SQLITE_OPEN_TEMP_JOURNAL :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_TRANSIENT_DB" _SQLITE_OPEN_TRANSIENT_DB :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_URI" _SQLITE_OPEN_URI :: CInt

-- https://www.sqlite.org/c3ref/c_open_autoproxy.html
foreign import capi "sqlite3.h value SQLITE_OPEN_WAL" _SQLITE_OPEN_WAL :: CInt

foreign import capi "sqlite3.h value SQLITE_PERM" _SQLITE_PERM :: CInt

foreign import capi "sqlite3.h value SQLITE_PRAGMA" _SQLITE_PRAGMA :: CInt

foreign import capi "sqlite3.h value SQLITE_PREPARE_NORMALIZE" _SQLITE_PREPARE_NORMALIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_PREPARE_NO_VTAB" _SQLITE_PREPARE_NO_VTAB :: CInt

foreign import capi "sqlite3.h value SQLITE_PREPARE_PERSISTENT" _SQLITE_PREPARE_PERSISTENT :: CInt

foreign import capi "sqlite3.h value SQLITE_PROTOCOL" _SQLITE_PROTOCOL :: CInt

foreign import capi "sqlite3.h value SQLITE_RANGE" _SQLITE_RANGE :: CInt

foreign import capi "sqlite3.h value SQLITE_READ" _SQLITE_READ :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY" _SQLITE_READONLY :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_CANTINIT" _SQLITE_READONLY_CANTINIT :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_CANTLOCK" _SQLITE_READONLY_CANTLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_DBMOVED" _SQLITE_READONLY_DBMOVED :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_DIRECTORY" _SQLITE_READONLY_DIRECTORY :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_RECOVERY" _SQLITE_READONLY_RECOVERY :: CInt

foreign import capi "sqlite3.h value SQLITE_READONLY_ROLLBACK" _SQLITE_READONLY_ROLLBACK :: CInt

foreign import capi "sqlite3.h value SQLITE_RECURSIVE" _SQLITE_RECURSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_REINDEX" _SQLITE_REINDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_REPLACE" _SQLITE_REPLACE :: CInt

foreign import capi "sqlite3.h value SQLITE_ROLLBACK" _SQLITE_ROLLBACK :: CInt

foreign import capi "sqlite3.h value SQLITE_ROW" _SQLITE_ROW :: CInt

foreign import capi "sqlite3.h value SQLITE_SAVEPOINT" _SQLITE_SAVEPOINT :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_EST" _SQLITE_SCANSTAT_EST :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_EXPLAIN" _SQLITE_SCANSTAT_EXPLAIN :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_NAME" _SQLITE_SCANSTAT_NAME :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_NLOOP" _SQLITE_SCANSTAT_NLOOP :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_NVISIT" _SQLITE_SCANSTAT_NVISIT :: CInt

foreign import capi "sqlite3.h value SQLITE_SCANSTAT_SELECTID" _SQLITE_SCANSTAT_SELECTID :: CInt

foreign import capi "sqlite3.h value SQLITE_SCHEMA" _SQLITE_SCHEMA :: CInt

foreign import capi "sqlite3.h value SQLITE_SELECT" _SQLITE_SELECT :: CInt

foreign import capi "sqlite3.h value SQLITE_SERIALIZE_NOCOPY" _SQLITE_SERIALIZE_NOCOPY :: CInt

foreign import capi "sqlite3.h value SQLITE_SHM_EXCLUSIVE" _SQLITE_SHM_EXCLUSIVE :: CInt

foreign import capi "sqlite3.h value SQLITE_SHM_LOCK" _SQLITE_SHM_LOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_SHM_NLOCK" _SQLITE_SHM_NLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_SHM_SHARED" _SQLITE_SHM_SHARED :: CInt

foreign import capi "sqlite3.h value SQLITE_SHM_UNLOCK" _SQLITE_SHM_UNLOCK :: CInt

foreign import capi "sqlite3.h value SQLITE_SOURCE_ID" _SQLITE_SOURCE_ID :: CString

foreign import capi "sqlite3.h value SQLITE_STATIC" _SQLITE_STATIC :: FunPtr (Ptr a -> IO ())

foreign import capi "sqlite3.h value SQLITE_STATUS_MALLOC_COUNT" _SQLITE_STATUS_MALLOC_COUNT :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_MALLOC_SIZE" _SQLITE_STATUS_MALLOC_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_MEMORY_USED" _SQLITE_STATUS_MEMORY_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_PAGECACHE_OVERFLOW" _SQLITE_STATUS_PAGECACHE_OVERFLOW :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_PAGECACHE_SIZE" _SQLITE_STATUS_PAGECACHE_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_PAGECACHE_USED" _SQLITE_STATUS_PAGECACHE_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_PARSER_STACK" _SQLITE_STATUS_PARSER_STACK :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_SCRATCH_OVERFLOW" _SQLITE_STATUS_SCRATCH_OVERFLOW :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_SCRATCH_SIZE" _SQLITE_STATUS_SCRATCH_SIZE :: CInt

foreign import capi "sqlite3.h value SQLITE_STATUS_SCRATCH_USED" _SQLITE_STATUS_SCRATCH_USED :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_AUTOINDEX" _SQLITE_STMTSTATUS_AUTOINDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_FILTER_HIT" _SQLITE_STMTSTATUS_FILTER_HIT :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_FILTER_MISS" _SQLITE_STMTSTATUS_FILTER_MISS :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_FULLSCAN_STEP" _SQLITE_STMTSTATUS_FULLSCAN_STEP :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_MEMUSED" _SQLITE_STMTSTATUS_MEMUSED :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_REPREPARE" _SQLITE_STMTSTATUS_REPREPARE :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_RUN" _SQLITE_STMTSTATUS_RUN :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_SORT" _SQLITE_STMTSTATUS_SORT :: CInt

foreign import capi "sqlite3.h value SQLITE_STMTSTATUS_VM_STEP" _SQLITE_STMTSTATUS_VM_STEP :: CInt

foreign import capi "sqlite3.h value SQLITE_SUBTYPE" _SQLITE_SUBTYPE :: CInt

foreign import capi "sqlite3.h value SQLITE_SYNC_DATAONLY" _SQLITE_SYNC_DATAONLY :: CInt

foreign import capi "sqlite3.h value SQLITE_SYNC_FULL" _SQLITE_SYNC_FULL :: CInt

foreign import capi "sqlite3.h value SQLITE_SYNC_NORMAL" _SQLITE_SYNC_NORMAL :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_ALWAYS" _SQLITE_TESTCTRL_ALWAYS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_ASSERT" _SQLITE_TESTCTRL_ASSERT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS" _SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_BITVEC_TEST" _SQLITE_TESTCTRL_BITVEC_TEST :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_BYTEORDER" _SQLITE_TESTCTRL_BYTEORDER :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_EXPLAIN_STMT" _SQLITE_TESTCTRL_EXPLAIN_STMT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS" _SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_FAULT_INSTALL" _SQLITE_TESTCTRL_FAULT_INSTALL :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_FIRST" _SQLITE_TESTCTRL_FIRST :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_IMPOSTER" _SQLITE_TESTCTRL_IMPOSTER :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_INTERNAL_FUNCTIONS" _SQLITE_TESTCTRL_INTERNAL_FUNCTIONS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_ISINIT" _SQLITE_TESTCTRL_ISINIT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_ISKEYWORD" _SQLITE_TESTCTRL_ISKEYWORD :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_LAST" _SQLITE_TESTCTRL_LAST :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_LOCALTIME_FAULT" _SQLITE_TESTCTRL_LOCALTIME_FAULT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_LOGEST" _SQLITE_TESTCTRL_LOGEST :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_NEVER_CORRUPT" _SQLITE_TESTCTRL_NEVER_CORRUPT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD" _SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_OPTIMIZATIONS" _SQLITE_TESTCTRL_OPTIMIZATIONS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PARSER_COVERAGE" _SQLITE_TESTCTRL_PARSER_COVERAGE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PENDING_BYTE" _SQLITE_TESTCTRL_PENDING_BYTE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PRNG_RESET" _SQLITE_TESTCTRL_PRNG_RESET :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PRNG_RESTORE" _SQLITE_TESTCTRL_PRNG_RESTORE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PRNG_SAVE" _SQLITE_TESTCTRL_PRNG_SAVE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_PRNG_SEED" _SQLITE_TESTCTRL_PRNG_SEED :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_RESERVE" _SQLITE_TESTCTRL_RESERVE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_RESULT_INTREAL" _SQLITE_TESTCTRL_RESULT_INTREAL :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_SCRATCHMALLOC" _SQLITE_TESTCTRL_SCRATCHMALLOC :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_SEEK_COUNT" _SQLITE_TESTCTRL_SEEK_COUNT :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_SORTER_MMAP" _SQLITE_TESTCTRL_SORTER_MMAP :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_TRACEFLAGS" _SQLITE_TESTCTRL_TRACEFLAGS :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_TUNE" _SQLITE_TESTCTRL_TUNE :: CInt

foreign import capi "sqlite3.h value SQLITE_TESTCTRL_VDBE_COVERAGE" _SQLITE_TESTCTRL_VDBE_COVERAGE :: CInt

foreign import capi "sqlite3.h value SQLITE_TEXT" _SQLITE_TEXT :: CInt

foreign import capi "sqlite3.h value SQLITE_TOOBIG" _SQLITE_TOOBIG :: CInt

foreign import capi "sqlite3.h value SQLITE_TRACE_CLOSE" _SQLITE_TRACE_CLOSE :: CInt

foreign import capi "sqlite3.h value SQLITE_TRACE_PROFILE" _SQLITE_TRACE_PROFILE :: CInt

foreign import capi "sqlite3.h value SQLITE_TRACE_ROW" _SQLITE_TRACE_ROW :: CInt

foreign import capi "sqlite3.h value SQLITE_TRACE_STMT" _SQLITE_TRACE_STMT :: CInt

foreign import capi "sqlite3.h value SQLITE_TRANSACTION" _SQLITE_TRANSACTION :: CInt

foreign import capi "sqlite3.h value SQLITE_TRANSIENT" _SQLITE_TRANSIENT :: FunPtr (Ptr a -> IO ())

foreign import capi "sqlite3.h value SQLITE_TXN_NONE" _SQLITE_TXN_NONE :: CInt

foreign import capi "sqlite3.h value SQLITE_TXN_READ" _SQLITE_TXN_READ :: CInt

foreign import capi "sqlite3.h value SQLITE_TXN_WRITE" _SQLITE_TXN_WRITE :: CInt

foreign import capi "sqlite3.h value SQLITE_UPDATE" _SQLITE_UPDATE :: CInt

foreign import capi "sqlite3.h value SQLITE_UTF16" _SQLITE_UTF16 :: CInt

foreign import capi "sqlite3.h value SQLITE_UTF16BE" _SQLITE_UTF16BE :: CInt

foreign import capi "sqlite3.h value SQLITE_UTF16LE" _SQLITE_UTF16LE :: CInt

foreign import capi "sqlite3.h value SQLITE_UTF16_ALIGNED" _SQLITE_UTF16_ALIGNED :: CInt

foreign import capi "sqlite3.h value SQLITE_UTF8" _SQLITE_UTF8 :: CInt

foreign import capi "sqlite3.h value SQLITE_VERSION" _SQLITE_VERSION :: CString

foreign import capi "sqlite3.h value SQLITE_VERSION_NUMBER" _SQLITE_VERSION_NUMBER :: CInt

foreign import capi "sqlite3.h value SQLITE_VTAB_CONSTRAINT_SUPPORT" _SQLITE_VTAB_CONSTRAINT_SUPPORT :: CInt

foreign import capi "sqlite3.h value SQLITE_VTAB_DIRECTONLY" _SQLITE_VTAB_DIRECTONLY :: CInt

foreign import capi "sqlite3.h value SQLITE_VTAB_INNOCUOUS" _SQLITE_VTAB_INNOCUOUS :: CInt

foreign import capi "sqlite3.h value SQLITE_WARNING" _SQLITE_WARNING :: CInt

foreign import capi "sqlite3.h value SQLITE_WARNING_AUTOINDEX" _SQLITE_WARNING_AUTOINDEX :: CInt

foreign import capi "sqlite3.h value SQLITE_WIN32_DATA_DIRECTORY_TYPE" _SQLITE_WIN32_DATA_DIRECTORY_TYPE :: CInt

foreign import capi "sqlite3.h value SQLITE_WIN32_TEMP_DIRECTORY_TYPE" _SQLITE_WIN32_TEMP_DIRECTORY_TYPE :: CInt
