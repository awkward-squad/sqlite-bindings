module Sqlite.Bindings.Internal.Objects where

-- | https://www.sqlite.org/c3ref/sqlite3.html
data {-# CTYPE "sqlite3.h" "sqlite3" #-} Sqlite3

-- | https://www.sqlite.org/c3ref/backup.html
data {-# CTYPE "sqlite3.h" "sqlite3_backup" #-} Sqlite3_backup

-- | https://www.sqlite.org/c3ref/blob.html
data {-# CTYPE "sqlite3.h" "sqlite3_blob" #-} Sqlite3_blob

-- | https://www.sqlite.org/c3ref/context.html
data {-# CTYPE "sqlite3.h" "sqlite3_context" #-} Sqlite3_context

-- | https://www.sqlite.org/c3ref/mutex.html
data {-# CTYPE "sqlite3.h" "sqlite3_mutex" #-} Sqlite3_mutex

-- | https://www.sqlite.org/c3ref/stmt.html
data {-# CTYPE "sqlite3.h" "sqlite3_stmt" #-} Sqlite3_stmt

-- | https://www.sqlite.org/c3ref/value.html
data {-# CTYPE "sqlite3.h" "sqlite3_value" #-} Sqlite3_value
