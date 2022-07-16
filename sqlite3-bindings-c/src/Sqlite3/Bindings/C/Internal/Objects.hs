module Sqlite3.Bindings.C.Internal.Objects where

import Foreign (FunPtr, Ptr)
import Foreign.C (CInt, CString)

-- | https://www.sqlite.org/c3ref/sqlite3.html
data {-# CTYPE "sqlite3.h" "sqlite3" #-} Sqlite3

data Sqlite3_api_routines

-- | https://www.sqlite.org/c3ref/backup.html
data {-# CTYPE "sqlite3.h" "sqlite3_backup" #-} Sqlite3_backup

-- | https://www.sqlite.org/c3ref/blob.html
data {-# CTYPE "sqlite3.h" "sqlite3_blob" #-} Sqlite3_blob

-- | https://www.sqlite.org/c3ref/context.html
data {-# CTYPE "sqlite3.h" "sqlite3_context" #-} Sqlite3_context

data Sqlite3_data_directory

data Sqlite3_file

-- | https://www.sqlite.org/c3ref/index_info.html
--
-- TODO rest of the fields
data {-# CTYPE "sqlite3.h" "sqlite3_index_info" #-} Sqlite3_index_info = Sqlite3_index_info
  { nConstraint :: !CInt
  }

data Sqlite3_io_methods

-- | https://www.sqlite.org/c3ref/mem_methods.html
data {-# CTYPE "sqlite3.h" "sqlite3_mem_methods" #-} Sqlite3_mem_methods

-- | https://www.sqlite.org/c3ref/module.html
--
-- TODO rest of the fields
data {-# CTYPE "sqlite3.h" "sqlite3_module" #-} Sqlite3_module = Sqlite3_module
  { iVersion :: !CInt,
    xCreate ::
      !( forall a.
         FunPtr
           ( Ptr Sqlite3 ->
             Ptr a ->
             CInt ->
             Ptr CString ->
             Ptr (Ptr Sqlite3_vtab) ->
             Ptr CString ->
             IO CInt
           )
       ),
    xConnect ::
      !( forall a.
         FunPtr
           ( Ptr Sqlite3 ->
             Ptr a ->
             CInt ->
             Ptr CString ->
             Ptr (Ptr Sqlite3_vtab) ->
             Ptr CString ->
             IO CInt
           )
       )
  }

-- | https://www.sqlite.org/c3ref/mutex.html
data {-# CTYPE "sqlite3.h" "sqlite3_mutex" #-} Sqlite3_mutex

-- | https://www.sqlite.org/c3ref/mutex_methods.html
data {-# CTYPE "sqlite3.h" "sqlite3_mutex_methods" #-} Sqlite3_mutex_methods

data Sqlite3_pcache

-- | https://www.sqlite.org/c3ref/pcache_methods2.html
data {-# CTYPE "sqlite3.h" "sqlite3_pcache_methods2" #-} Sqlite3_pcache_methods2

data Sqlite3_pcache_page

-- | https://www.sqlite.org/c3ref/snapshot.html
data {-# CTYPE "sqlite3.h" "sqlite3_snapshot" #-} Sqlite3_snapshot

-- | https://www.sqlite.org/c3ref/stmt.html
data {-# CTYPE "sqlite3.h" "sqlite3_stmt" #-} Sqlite3_stmt

data Sqlite3_temp_directory

-- | https://www.sqlite.org/c3ref/value.html
data {-# CTYPE "sqlite3.h" "sqlite3_value" #-} Sqlite3_value

-- | https://www.sqlite.org/c3ref/vfs.html
--
-- TODO rest of the fields
data {-# CTYPE "sqlite3.h" "sqlite3_vfs" #-} Sqlite3_vfs = Sqlite3_vfs
  { iVersion :: !CInt,
    szOsFile :: !CInt,
    mxPathname :: !CInt,
    pNext :: !(Ptr Sqlite3_vfs)
  }

data Sqlite3_vtab

data Sqlite3_vtab_cursor
