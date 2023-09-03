#include <sqlite3.h>
#include <stdio.h>
#include "junkyard.h"

int my_entrypoint(sqlite3* db, const char **errmsg, const sqlite3_api_routines* api) {
  printf("Hello from my_entrypoint!\n");
  return 0;
}
