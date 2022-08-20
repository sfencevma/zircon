
#include"chisel_db.h"

bool dump;
sqlite3 *mem_db;
char * zErrMsg;
int rc;

static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  return 0;
}

  

  


void init_db(bool en){
  dump = en;
  if(!en) return;
  rc = sqlite3_open(":memory:", &mem_db);
  if(rc) {
    printf("Can't open database: %s\n", sqlite3_errmsg(mem_db));
    exit(0);
  } else {
    printf("Open database successfully\n");
  }
  

}


void save_db(const char *zFilename) {
  printf("saving memdb to %s ...\n", zFilename);
  sqlite3 *disk_db;
  sqlite3_backup *pBackup;
  rc = sqlite3_open(zFilename, &disk_db);
  if(rc == SQLITE_OK){
    pBackup = sqlite3_backup_init(disk_db, "main", mem_db, "main");
    if(pBackup){
      (void)sqlite3_backup_step(pBackup, -1);
      (void)sqlite3_backup_finish(pBackup);
    }
    rc = sqlite3_errcode(disk_db);
  }
  sqlite3_close(disk_db);
}


