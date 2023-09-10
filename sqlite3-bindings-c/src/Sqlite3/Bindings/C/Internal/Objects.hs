module Sqlite3.Bindings.C.Internal.Objects where

import Foreign (FunPtr, Ptr)
import Foreign.C (CInt, CString, CUInt)

-- | https://www.sqlite.org/c3ref/sqlite3.html
--
-- TODO
data {-# CTYPE "sqlite3.h" "sqlite3" #-} Sqlite3

-- | https://www.sqlite.org/c3ref/api_routines.html
--
-- A boilerplate data structure that must be provided to the @SQLITE_EXTENSION_INIT2@ macro when initializing an
-- extension.
data {-# CTYPE "sqlite3.h" "sqlite3_api_routines" #-} Sqlite3_api_routines

-- | https://www.sqlite.org/c3ref/backup.html
--
-- An online backup object.
data {-# CTYPE "sqlite3.h" "sqlite3_backup" #-} Sqlite3_backup

-- | https://www.sqlite.org/c3ref/blob.html
--
-- A blob object.
data {-# CTYPE "sqlite3.h" "sqlite3_blob" #-} Sqlite3_blob

-- | https://www.sqlite.org/c3ref/context.html
--
-- A function context object.
data {-# CTYPE "sqlite3.h" "sqlite3_context" #-} Sqlite3_context

-- TODO
data Sqlite3_file

-- | https://www.sqlite.org/c3ref/index_info.html
--
-- TODO rest of the fields
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_index_info" #-} Sqlite3_index_info = Sqlite3_index_info
  { nConstraint :: !CInt
  }

data Sqlite3_io_methods

-- | https://www.sqlite.org/c3ref/module.html
--
-- TODO rest of the fields
-- TODO document
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
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_mutex" #-} Sqlite3_mutex

-- | TODO document
data Sqlite3_pcache

-- | https://www.sqlite.org/c3ref/pcache_methods2.html
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_pcache_methods2" #-} Sqlite3_pcache_methods2 a = Sqlite3_pcache_methods2
  { iVersion :: !CInt,
    pArg :: !(Ptr a),
    xInit :: !(FunPtr (Ptr a -> IO CInt)),
    xShutdown :: !(FunPtr (Ptr a -> IO ())),
    xCreate :: !(FunPtr (CInt -> CInt -> CInt -> IO (Ptr Sqlite3_pcache))),
    xCachesize :: !(FunPtr (Ptr Sqlite3_pcache -> CInt -> IO ())),
    xPagecount :: !(FunPtr (Ptr Sqlite3_pcache -> IO CInt)),
    xFetch :: !(FunPtr (Ptr Sqlite3_pcache -> CUInt -> CInt -> IO (Ptr Sqlite3_pcache_page))),
    xUnpin :: !(FunPtr (Ptr Sqlite3_pcache -> Ptr Sqlite3_pcache_page -> CInt -> IO ())),
    xRekey :: !(FunPtr (Ptr Sqlite3_pcache -> Ptr Sqlite3_pcache_page -> CUInt -> CUInt -> IO ())),
    xTruncate :: !(FunPtr (Ptr Sqlite3_pcache -> CUInt -> IO ())),
    xDestroy :: !(FunPtr (Ptr Sqlite3_pcache -> IO ())),
    xShrink :: !(FunPtr (Ptr Sqlite3_pcache -> IO ()))
  }

-- | TODO document
data Sqlite3_pcache_page

-- | https://www.sqlite.org/c3ref/snapshot.html
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_snapshot" #-} Sqlite3_snapshot

-- | https://www.sqlite.org/c3ref/stmt.html
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_stmt" #-} Sqlite3_stmt

-- | TODO document
data Sqlite3_temp_directory

-- | https://www.sqlite.org/c3ref/value.html
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_value" #-} Sqlite3_value

-- | https://www.sqlite.org/c3ref/vfs.html
--
-- TODO rest of the fields
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_vfs" #-} Sqlite3_vfs = Sqlite3_vfs
  { iVersion :: !CInt,
    szOsFile :: !CInt,
    mxPathname :: !CInt,
    pNext :: !(Ptr Sqlite3_vfs)
  }

-- | TODO document
data Sqlite3_vtab

-- | TODO document
data Sqlite3_vtab_cursor
