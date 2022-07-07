#### Functions that (probably?) won't get bindings

These functions are officially deprecated.

| Function |
| --- |
| `sqlite3_aggregate_count` |
| `sqlite3_expired` |
| `sqlite3_global_recover` |
| `sqlite3_memory_alarm` |
| `sqlite3_profile` |
| `sqlite3_soft_heap_limit` |
| `sqlite3_thread_cleanup` |
| `sqlite3_trace` |
| `sqlite3_transfer_bindings` |

These functions encode text using UTF-16, which seems less convenient than UTF-8, especially after `text-2.0`.

| Function |
| --- |
| `sqlite3_bind_text16` |
| `sqlite3_collation_needed16` |
| `sqlite3_column_bytes16` |
| `sqlite3_column_database_name16` |
| `sqlite3_column_decltype16` |
| `sqlite3_column_name16` |
| `sqlite3_column_origin_name16` |
| `sqlite3_column_table_name16` |
| `sqlite3_column_text16` |
| `sqlite3_complete16` |
| `sqlite3_create_collation16` |
| `sqlite3_create_function16` |
| `sqlite3_errmsg16` |
| `sqlite3_open16` |
| `sqlite3_prepare16_v2` |
| `sqlite3_prepare16_v3` |
| `sqlite3_prepare16` |
| `sqlite3_result_error16` |
| `sqlite3_result_text16` |
| `sqlite3_result_text16be` |
| `sqlite3_result_text16le` |
| `sqlite3_value_bytes16` |
| `sqlite3_value_text16` |
| `sqlite3_value_text16be` |
| `sqlite3_value_text16le` |
| `sqlite3_win32_set_directory16` |

These functions seem obviated by other, more powerful functions.

| Function | Reason |
| --- | --- |
| `sqlite3_open` | Less powerful than `sqlite3_open_v2` |
