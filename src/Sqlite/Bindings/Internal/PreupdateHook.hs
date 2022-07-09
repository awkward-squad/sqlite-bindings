module Sqlite.Bindings.Internal.PreupdateHook
  ( sqlite3_preupdate_blobwrite,
    sqlite3_preupdate_count,
    sqlite3_preupdate_depth,
    sqlite3_preupdate_hook,
    sqlite3_preupdate_new,
    sqlite3_preupdate_old,
  )
where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import Sqlite.Bindings.Internal.Objects

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_blobwrite"
  sqlite3_preupdate_blobwrite :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_count"
  sqlite3_preupdate_count :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_depth"
  sqlite3_preupdate_depth :: Ptr Sqlite3 -> IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_hook"
  sqlite3_preupdate_hook ::
    Ptr Sqlite3 ->
    FunPtr (Ptr a -> Ptr Sqlite3 -> CInt -> CString -> CString -> Int64 -> Int64 -> IO ()) ->
    Ptr a ->
    IO (Ptr b)

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_new"
  sqlite3_preupdate_new ::
    Ptr Sqlite3 ->
    CInt ->
    Ptr (Ptr Sqlite3_value) ->
    IO CInt

-- | https://www.sqlite.org/c3ref/preupdate_blobwrite.html
foreign import capi unsafe "sqlite3.h sqlite3_preupdate_old"
  sqlite3_preupdate_old ::
    Ptr Sqlite3 ->
    CInt ->
    Ptr (Ptr Sqlite3_value) ->
    IO CInt
