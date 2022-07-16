module Sqlite3.Bindings.Internal.Objects
  ( Sqlite3 (..),
    Sqlite.Bindings.Sqlite3_api_routines,
    Sqlite.Bindings.Sqlite3_backup,
    Sqlite.Bindings.Sqlite3_blob,
    Sqlite.Bindings.Sqlite3_context,
    Sqlite.Bindings.Sqlite3_data_directory,
    Sqlite.Bindings.Sqlite3_file,
    Sqlite.Bindings.Sqlite3_index_info (..),
    Sqlite.Bindings.Sqlite3_io_methods,
    Sqlite.Bindings.Sqlite3_mem_methods,
    Sqlite.Bindings.Sqlite3_module (..),
    Sqlite.Bindings.Sqlite3_mutex,
    Sqlite.Bindings.Sqlite3_mutex_methods,
    Sqlite.Bindings.Sqlite3_pcache,
    Sqlite.Bindings.Sqlite3_pcache_methods2,
    Sqlite.Bindings.Sqlite3_pcache_page,
    Sqlite.Bindings.Sqlite3_snapshot,
    Sqlite.Bindings.Sqlite3_stmt,
    Sqlite.Bindings.Sqlite3_temp_directory,
    Sqlite.Bindings.Sqlite3_value,
    Sqlite.Bindings.Sqlite3_vfs (..),
    Sqlite.Bindings.Sqlite3_vtab,
    Sqlite.Bindings.Sqlite3_vtab_cursor,
  )
where

import Foreign.Ptr (Ptr)
import qualified Sqlite.Bindings

-- | https://www.sqlite.org/c3ref/sqlite3.html
newtype Sqlite3
  = Sqlite3 (Ptr Sqlite.Bindings.Sqlite3)
