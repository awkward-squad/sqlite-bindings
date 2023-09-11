#include "sqlite3ext.h"
SQLITE_EXTENSION_INIT1

#include <stdio.h>
#include "junkyard.h"
#include "Main_stub.h"

int my_entrypoint(sqlite3* db, const char **errmsg, const sqlite3_api_routines* api) {
  SQLITE_EXTENSION_INIT2(api);
  return myEntrypoint(db, errmsg);
}
