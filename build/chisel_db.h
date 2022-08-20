
#ifndef __CHISEL_DB_H__
#define __CHISEL_DB_H__

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <cstdint>
#include <cerrno>
#include <unistd.h>
#include <sqlite3.h>

void init_db(bool en);
void save_db(const char * filename);

#endif
