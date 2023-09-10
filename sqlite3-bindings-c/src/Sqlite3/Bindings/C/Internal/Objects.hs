module Sqlite3.Bindings.C.Internal.Objects where

import Foreign (FunPtr, Ptr)
import Foreign.C (CInt, CLLong, CString, CUInt)

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

-- | https://www.sqlite.org/c3ref/file.html
--
-- A file object.
data {-# CTYPE "sqlite3.h" "sqlite3_file" #-} Sqlite3_file

-- | https://www.sqlite.org/c3ref/index_info.html
--
-- TODO rest of the fields
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_index_info" #-} Sqlite3_index_info = Sqlite3_index_info
  { nConstraint :: {-# UNPACK #-} !CInt
  }

data Sqlite3_io_methods

-- | https://www.sqlite.org/c3ref/module.html
--
-- TODO document
data {-# CTYPE "sqlite3.h" "sqlite3_module" #-} Sqlite3_module a = Sqlite3_module
  { iVersion :: {-# UNPACK #-} !CInt,
    xCreate ::
      {-# UNPACK #-} !( FunPtr
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
      {-# UNPACK #-} !( FunPtr
                          ( Ptr Sqlite3 ->
                            Ptr a ->
                            CInt ->
                            Ptr CString ->
                            Ptr (Ptr Sqlite3_vtab) ->
                            Ptr CString ->
                            IO CInt
                          )
                      ),
    xBestIndex :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> Ptr Sqlite3_index_info -> IO CInt)),
    xDisconnect :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xDestroy :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xOpen :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> Ptr (Ptr Sqlite3_vtab_cursor) -> IO CInt)),
    xClose :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab_cursor -> IO CInt)),
    xFilter ::
      {-# UNPACK #-} !( FunPtr
                          ( Ptr Sqlite3_vtab_cursor ->
                            CInt ->
                            CString ->
                            CInt ->
                            Ptr (Ptr Sqlite3_value) ->
                            IO CInt
                          )
                      ),
    xNext :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab_cursor -> IO CInt)),
    xEof :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab_cursor -> IO CInt)),
    xColumn :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab_cursor -> Ptr Sqlite3_context -> CInt -> IO CInt)),
    xRowid :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab_cursor -> Ptr CLLong -> IO CInt)),
    xUpdate :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> CInt -> Ptr (Ptr Sqlite3_value) -> Ptr CLLong -> IO CInt)),
    xBegin :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xSync :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xCommit :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xRollback :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> IO CInt)),
    xFindFunction ::
      {-# UNPACK #-} !( forall x.
                        FunPtr
                          ( Ptr Sqlite3_vtab ->
                            CInt ->
                            CString ->
                            Ptr
                              ( FunPtr
                                  ( Ptr Sqlite3_context ->
                                    CInt ->
                                    Ptr (Ptr Sqlite3_value)
                                  )
                              ) ->
                            Ptr (Ptr x) ->
                            IO CInt
                          )
                      ),
    xRename :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> CString -> IO CInt)),
    xSavepoint :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> CInt -> IO CInt)),
    xRelease :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> CInt -> IO CInt)),
    xRollbackTo :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vtab -> CInt -> IO CInt)),
    xShadowName :: {-# UNPACK #-} !(FunPtr (CString -> IO CInt))
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
  { iVersion :: {-# UNPACK #-} !CInt,
    pArg :: {-# UNPACK #-} !(Ptr a),
    xInit :: {-# UNPACK #-} !(FunPtr (Ptr a -> IO CInt)),
    xShutdown :: {-# UNPACK #-} !(FunPtr (Ptr a -> IO ())),
    xCreate :: {-# UNPACK #-} !(FunPtr (CInt -> CInt -> CInt -> IO (Ptr Sqlite3_pcache))),
    xCachesize :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> CInt -> IO ())),
    xPagecount :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> IO CInt)),
    xFetch :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> CUInt -> CInt -> IO (Ptr Sqlite3_pcache_page))),
    xUnpin :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> Ptr Sqlite3_pcache_page -> CInt -> IO ())),
    xRekey :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> Ptr Sqlite3_pcache_page -> CUInt -> CUInt -> IO ())),
    xTruncate :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> CUInt -> IO ())),
    xDestroy :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> IO ())),
    xShrink :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_pcache -> IO ()))
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
  { iVersion :: {-# UNPACK #-} !CInt,
    szOsFile :: {-# UNPACK #-} !CInt,
    mxPathname :: {-# UNPACK #-} !CInt,
    pNext :: {-# UNPACK #-} !(Ptr Sqlite3_vfs)
  }

-- | TODO document
data Sqlite3_vtab

-- | TODO document
data Sqlite3_vtab_cursor
