module Sqlite.Bindings.Internal.Objects where

-- | https://www.sqlite.org/c3ref/sqlite3.html
data {-# CTYPE "sqlite3.h" "sqlite3" #-} Sqlite3

-- | https://www.sqlite.org/c3ref/blob.html
data {-# CTYPE "sqlite3.h" "sqlite3_blob" #-} Sqlite3_blob

-- | https://www.sqlite.org/c3ref/stmt.html
data {-# CTYPE "sqlite3.h" "sqlite3_stmt" #-} Sqlite3_stmt
