module Sqlite3.Bindings.Internal.Objects
  ( Sqlite3 (..),
    C.Sqlite3_api_routines,
    C.Sqlite3_backup,
    C.Sqlite3_blob,
    C.Sqlite3_context,
    C.Sqlite3_data_directory,
    C.Sqlite3_file,
    C.Sqlite3_index_info (..),
    C.Sqlite3_io_methods,
    C.Sqlite3_mem_methods,
    C.Sqlite3_module (..),
    C.Sqlite3_mutex,
    C.Sqlite3_mutex_methods,
    C.Sqlite3_pcache,
    C.Sqlite3_pcache_methods2,
    C.Sqlite3_pcache_page,
    C.Sqlite3_snapshot,
    Sqlite3_stmt (..),
    C.Sqlite3_temp_directory,
    C.Sqlite3_value,
    C.Sqlite3_vfs (..),
    C.Sqlite3_vtab,
    C.Sqlite3_vtab_cursor,
  )
where

import Foreign.Ptr (Ptr)
import qualified Sqlite3.Bindings.C as C

-- | https://www.sqlite.org/c3ref/sqlite3.html
newtype Sqlite3
  = Sqlite3 (Ptr C.Sqlite3)

-- | https://www.sqlite.org/c3ref/stmt.html
newtype Sqlite3_stmt
  = Sqlite3_stmt (Ptr C.Sqlite3_stmt)
