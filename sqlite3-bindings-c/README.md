#### Overview

This library provides (mostly) complete bindings to SQLite.

#### Things that don't have bindings

| Symbol | Reason |
| --- | --- |
| `SQLITE_ANY` | Deprecated |
| `SQLITE_CONFIG_COVERING_INDEX_SCAN` | Uncommon |
| `SQLITE_CONFIG_GETMALLOC` | Uncommon |
| `SQLITE_CONFIG_GETMUTEX` | Uncommon |
| `SQLITE_CONFIG_GETPCACHE` | Unused |
| `SQLITE_CONFIG_MALLOC` | Uncommon |
| `SQLITE_CONFIG_MUTEX` | Uncommon |
| `SQLITE_CONFIG_PCACHE` | Unused |
| `SQLITE_CONFIG_SCRATCH` | Unused |
| `SQLITE_OPEN_PRIVATECACHE` | Deprecated |
| `SQLITE_OPEN_SHAREDCACHE` | Deprecated |
| `SQLITE_PREPARE_NORMALIZE` | Unused |
| `sqlite3_aggregate_count` | Deprecated |
| `sqlite3_enable_load_extension` | Insecure |
| `sqlite3_enable_shared_cache` | Deprecated |
| `sqlite3_expired` | Deprecated |
| `sqlite3_free_table` | Deprecated |
| `sqlite3_get_table` | Deprecated |
| `sqlite3_global_recover` | Deprecated |
| `sqlite3_memory_alarm` | Deprecated |
| `sqlite3_mprintf` | No useful non-variadic usage |
| `sqlite3_mutex_alloc` | Intended for internal use |
| `sqlite3_mutex_enter` | Intended for internal use |
| `sqlite3_mutex_free` | Intended for internal use |
| `sqlite3_mutex_leave` | Intended for internal use |
| `sqlite3_mutex_try` | Intended for internal use |
| `sqlite3_os_end` | Shouldn't be called by application |
| `sqlite3_os_init` | Shouldn't be called by application |
| `sqlite3_prepare` | Deprecated |
| `sqlite3_profile` | Deprecated |
| `sqlite3_snprintf` | No useful non-variadic usage |
| `sqlite3_soft_heap_limit` | Deprecated |
| `sqlite3_str_append` | Not useful |
| `sqlite3_str_appendall` | Not useful |
| `sqlite3_str_appendchar` | Not useful |
| `sqlite3_str_appendf` | Not useful |
| `sqlite3_str_errcode` | Not useful |
| `sqlite3_str_finish` | Not useful |
| `sqlite3_str_length` | Not useful |
| `sqlite3_str_new` | Not useful |
| `sqlite3_str_reset` | Not useful |
| `sqlite3_str_value` | Not useful |
| `sqlite3_str_vappendf` | Not useful |
| `sqlite3_test_control` | Shouldn't be called by application |
| `sqlite3_thread_cleanup` | Deprecated |
| `sqlite3_trace` | Deprecated |
| `sqlite3_transfer_bindings` | Deprecated |
| `sqlite3_vmprintf` | No useful non-variadic usage |
| `sqlite3_vsnprintf` | No useful non-variadic usage |
