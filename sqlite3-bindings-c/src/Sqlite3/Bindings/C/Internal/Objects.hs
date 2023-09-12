module Sqlite3.Bindings.C.Internal.Objects where

import Data.Int (Int64)
import Foreign (FunPtr, Ptr)
import Foreign.C (CDouble, CInt, CString, CUInt)

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
-- An online backup.
data {-# CTYPE "sqlite3.h" "sqlite3_backup" #-} Sqlite3_backup

-- | https://www.sqlite.org/c3ref/blob.html
--
-- A blob.
data {-# CTYPE "sqlite3.h" "sqlite3_blob" #-} Sqlite3_blob

-- | https://www.sqlite.org/c3ref/context.html
--
-- A function context.
data {-# CTYPE "sqlite3.h" "sqlite3_context" #-} Sqlite3_context

-- | https://www.sqlite.org/c3ref/file.html
--
-- A file.
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
-- A virtual table module.
data {-# CTYPE "sqlite3.h" "sqlite3_module" #-} Sqlite3_module a = Sqlite3_module
  { iVersion :: {-# UNPACK #-} !CInt,
    xCreate ::
      {-# UNPACK #-} !( FunPtr
                          ( Ptr Sqlite3 ->
                            Ptr a ->
                            CInt ->
                            Ptr CString ->
                            Ptr (Ptr (Sqlite3_vtab a)) ->
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
                            Ptr (Ptr (Sqlite3_vtab a)) ->
                            Ptr CString ->
                            IO CInt
                          )
                      ),
    xBestIndex :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> Ptr Sqlite3_index_info -> IO CInt)),
    xDisconnect :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xDestroy :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xOpen :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> Ptr (Ptr (Sqlite3_vtab_cursor a)) -> IO CInt)),
    xClose :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab_cursor a) -> IO CInt)),
    xFilter ::
      {-# UNPACK #-} !( FunPtr
                          ( Ptr (Sqlite3_vtab_cursor a) ->
                            CInt ->
                            CString ->
                            CInt ->
                            Ptr (Ptr Sqlite3_value) ->
                            IO CInt
                          )
                      ),
    xNext :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab_cursor a) -> IO CInt)),
    xEof :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab_cursor a) -> IO CInt)),
    xColumn :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab_cursor a) -> Ptr Sqlite3_context -> CInt -> IO CInt)),
    xRowid :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab_cursor a) -> Ptr Int64 -> IO CInt)),
    xUpdate :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> CInt -> Ptr (Ptr Sqlite3_value) -> Ptr Int64 -> IO CInt)),
    xBegin :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xSync :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xCommit :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xRollback :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> IO CInt)),
    xFindFunction ::
      {-# UNPACK #-} !( FunPtr
                          ( Ptr (Sqlite3_vtab a) ->
                            CInt ->
                            CString ->
                            Ptr
                              ( FunPtr
                                  ( Ptr Sqlite3_context ->
                                    CInt ->
                                    Ptr (Ptr Sqlite3_value)
                                  )
                              ) ->
                            Ptr (Ptr ()) ->
                            IO CInt
                          )
                      ),
    xRename :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> CString -> IO CInt)),
    xSavepoint :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> CInt -> IO CInt)),
    xRelease :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> CInt -> IO CInt)),
    xRollbackTo :: {-# UNPACK #-} !(FunPtr (Ptr (Sqlite3_vtab a) -> CInt -> IO CInt)),
    xShadowName :: {-# UNPACK #-} !(FunPtr (CString -> IO CInt))
  }

-- | https://www.sqlite.org/c3ref/mutex.html
--
-- A mutex.
data {-# CTYPE "sqlite3.h" "sqlite3_mutex" #-} Sqlite3_mutex

-- | https://www.sqlite.org/c3ref/pcache.html
--
-- A page cache.
data {-# CTYPE "sqlite3.h" "sqlite3_pcache" #-} Sqlite3_pcache

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

-- | https://www.sqlite.org/c3ref/pcache_page.html
--
-- A page cache page.
data Sqlite3_pcache_page = Sqlite3_pcache_page
  { pBuf :: {-# UNPACK #-} !(Ptr ()),
    pExtra :: {-# UNPACK #-} !(Ptr ())
  }

-- | https://www.sqlite.org/c3ref/snapshot.html
--
-- A database snapshot.
data {-# CTYPE "sqlite3.h" "sqlite3_snapshot" #-} Sqlite3_snapshot

-- | https://www.sqlite.org/c3ref/stmt.html
--
-- A prepared statement.
data {-# CTYPE "sqlite3.h" "sqlite3_stmt" #-} Sqlite3_stmt

-- | https://www.sqlite.org/c3ref/value.html
--
-- A value (integer, real number, string, blob, or null).
data {-# CTYPE "sqlite3.h" "sqlite3_value" #-} Sqlite3_value

-- | https://www.sqlite.org/c3ref/vfs.html
--
-- A virtual filesystem.
data {-# CTYPE "sqlite3.h" "sqlite3_vfs" #-} Sqlite3_vfs = Sqlite3_vfs
  { iVersion :: {-# UNPACK #-} !CInt,
    szOsFile :: {-# UNPACK #-} !CInt,
    mxPathname :: {-# UNPACK #-} !CInt,
    pNext :: {-# UNPACK #-} !(Ptr Sqlite3_vfs),
    zName :: {-# UNPACK #-} !CString,
    pAppData :: {-# UNPACK #-} !(Ptr ()),
    xOpen :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> Ptr Sqlite3_file -> CInt -> Ptr CInt -> IO CInt)),
    xDelete :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> CInt -> IO CInt)),
    xAccess :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> CInt -> Ptr CInt -> IO CInt)),
    xFullPathname :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> CInt -> CString -> IO CInt)),
    xDlOpen :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> IO (Ptr ()))),
    xDlError :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CInt -> CString -> IO ())),
    xDlSym :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> Ptr () -> CString -> IO (FunPtr (IO ())))),
    xDlClose :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> Ptr () -> IO ())),
    xRandomness :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CInt -> CString -> IO CInt)),
    xSleep :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CInt -> IO CInt)),
    xCurrentTime :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> Ptr CDouble -> IO CInt)),
    xGetLastError :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CInt -> CString -> IO CInt)),
    xCurrentTimeInt64 :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> Ptr Int64 -> IO CInt)),
    xSetSystemCall :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> FunPtr (IO ()) -> IO CInt)),
    xGetSystemCall :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> IO (FunPtr (IO ())))),
    xNextSystemCall :: {-# UNPACK #-} !(FunPtr (Ptr Sqlite3_vfs -> CString -> IO CString))
  }

-- | https://www.sqlite.org/c3ref/vtab.html
--
-- A virtual table instance.
data {-# CTYPE "sqlite3.h" "sqlite3_vtab" #-} Sqlite3_vtab a = Sqlite3_vtab
  { pModule :: {-# UNPACK #-} !(Ptr (Sqlite3_module a)),
    nRef :: {-# UNPACK #-} !CInt,
    zErrMsg :: {-# UNPACK #-} !CString
  }

-- | https://www.sqlite.org/c3ref/vtab_cursor.html
--
-- A virtual table cursor.
data {-# CTYPE "sqlite3.h" "sqlite3_vtab_cursor" #-} Sqlite3_vtab_cursor a = Sqlite3_vtab_cursor
  { pVtab :: {-# UNPACK #-} !(Ptr (Sqlite3_vtab a))
  }
