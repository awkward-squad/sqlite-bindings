module Sqlite3.Bindings.Internal.Objects
  ( Sqlite3 (..),
    Sqlite3_api_routines (..),
    Sqlite3_backup (..),
    Sqlite3_blob (..),
    Sqlite3_context (..),
    C.Sqlite3_file,
    C.Sqlite3_index_info (..),
    C.Sqlite3_io_methods,
    C.Sqlite3_module (..),
    C.Sqlite3_mutex,
    C.Sqlite3_pcache,
    C.Sqlite3_pcache_methods2 (..),
    C.Sqlite3_pcache_page (..),
    Sqlite3_snapshot (..),
    Sqlite3_stmt (..),
    Sqlite3_value (..),
    C.Sqlite3_vfs (..),
    C.Sqlite3_vtab (..),
    C.Sqlite3_vtab_cursor (..),
  )
where

import Foreign.Ptr (Ptr)
import Sqlite3.Bindings.C qualified as C

-- | https://www.sqlite.org/c3ref/sqlite3.html
--
-- TODO
newtype Sqlite3
  = Sqlite3 (Ptr C.Sqlite3)

-- | https://www.sqlite.org/c3ref/api_routines.html
--
-- TODO
newtype Sqlite3_api_routines
  = Sqlite3_api_routines (Ptr C.Sqlite3_api_routines)

-- | https://www.sqlite.org/c3ref/backup.html
--
-- TODO
newtype Sqlite3_backup
  = Sqlite3_backup (Ptr C.Sqlite3_backup)

-- | https://www.sqlite.org/c3ref/blob.html
--
-- TODO
newtype Sqlite3_blob
  = Sqlite3_blob (Ptr C.Sqlite3_blob)

-- | https://www.sqlite.org/c3ref/context.html
--
-- A function context.
newtype Sqlite3_context
  = Sqlite3_context (Ptr C.Sqlite3_context)

-- | https://www.sqlite.org/c3ref/snapshot.html
--
-- A database snapshot.
newtype Sqlite3_snapshot
  = Sqlite3_snapshot (Ptr C.Sqlite3_snapshot)

-- | https://www.sqlite.org/c3ref/stmt.html
--
-- TODO
newtype Sqlite3_stmt
  = Sqlite3_stmt (Ptr C.Sqlite3_stmt)

-- | https://www.sqlite.org/c3ref/value.html
--
-- TODO
newtype Sqlite3_value
  = Sqlite3_value (Ptr C.Sqlite3_value)
