
/*
   M_sqlite.c -[M_sqlite] -
   C wrappers callable from Fortran for the SQLite library
*/
#include <string.h>
#include <stdio.h>
#include <sqlite3.h>
#include <stdlib.h>

/* Result table for sqlite3_get_table */
static char **result;
/**********************************************************************************************************************************/
static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  int i;
  for(i=0; i<argc; i++){
    printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  printf("\n");
  return 0;
}
/**********************************************************************************************************************************/
int sqlite3_open_c(char *fname, sqlite3 **db) {
   int rc ;

   rc = sqlite3_open(fname, db);

   if(rc != 0){
      sqlite3_close(*db);
   }
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_close_c(sqlite3 **db) {
   int rc ;
   rc = sqlite3_close(*db);
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_do_c(sqlite3 **db, char *command, char *errmsg[]) {
   int   rc  ;
   rc = sqlite3_exec(*db, command, callback, 0, errmsg) ;
   return rc ;
}
/**********************************************************************************************************************************/
void sqlite3_finalize_c(sqlite3_stmt **stmt) {
   int   rc  ;
   rc = sqlite3_finalize(*stmt) ;
   return ;
}
/**********************************************************************************************************************************/
void sqlite3_reset_c(sqlite3_stmt **stmt) {
   int   rc  ;
   rc = sqlite3_reset(*stmt) ;
   return ;
}
/**********************************************************************************************************************************/
void sqlite3_step_c(sqlite3_stmt **stmt, int *completion) {
   *completion = sqlite3_step(*stmt) ;
   return ;
}
/**********************************************************************************************************************************/
void sqlite3_errmsg_c(sqlite3 **db, const char *errmsg) {
   errmsg = sqlite3_errmsg(*db) ;
   return ;
}
/**********************************************************************************************************************************/
//int sqlite3_prepare(
//  sqlite3 *db,            /* Database handle */
//  const char *zSql,       /* SQL statement, UTF-8 encoded */
//  int nByte,              /* Maximum length of zSql in bytes. */
//  sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
//  const char **pzTail     /* OUT: Pointer to unused portion of zSql */
//);

int sqlite3_prepare_c(sqlite3 **db, char *command, sqlite3_stmt **stmt) {
   int   rc   ;
   const char **pstr ;
   rc = sqlite3_prepare(*db, command, (-1), stmt, pstr) ;
   return rc ;
}
/**********************************************************************************************************************************/
void sqlite3_column_count_c(sqlite3_stmt **stmt, int *count) {
   *count = sqlite3_column_count(*stmt) ;
   return ;
}
/**********************************************************************************************************************************/
void sqlite3_column_name_type_c(sqlite3_stmt **stmt, int  *colidx, char **name, char **type) {
   int   rc   ;

   *name = (char *)sqlite3_column_name(*stmt, *colidx) ;
   *type = (char *)sqlite3_column_decltype(*stmt, *colidx) ;
   return ;
}
/**********************************************************************************************************************************/
int sqlite3_bind_int_c(sqlite3_stmt **stmt, int *colidx, int *value) {
   int   rc   ;
   rc = sqlite3_bind_int(*stmt, *colidx, *value) ;
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_bind_double_c(sqlite3_stmt **stmt, int *colidx, double *value) {
   int   rc   ;
   rc = sqlite3_bind_double(*stmt, *colidx, *value) ;
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_bind_null_c(sqlite3_stmt **stmt, int *colidx) {
   int   rc   ;
   rc = sqlite3_bind_null(*stmt, *colidx) ;
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_bind_text_c(sqlite3_stmt **stmt, int *colidx, char *text) {
   int   rc   ;
   rc = sqlite3_bind_text(*stmt, *colidx, text, strlen(text), SQLITE_TRANSIENT) ;
   return rc ;
}
/**********************************************************************************************************************************/
int sqlite3_column_int_c(sqlite3_stmt **stmt, int colidx ) {
   return sqlite3_column_int(*stmt, colidx) ;
}
/**********************************************************************************************************************************/
double sqlite3_column_double_c(sqlite3_stmt **stmt, int colidx ) {
   return sqlite3_column_double(*stmt, colidx) ;
}
/**********************************************************************************************************************************/
char *sqlite3_column_text_c( sqlite3_stmt **stmt, int colidx, char *text ) {
   return (char *)sqlite3_column_text(*stmt, colidx ) ;
}
/**********************************************************************************************************************************/
int sqlite3_get_table_1_c(sqlite3 **db, char *command, int *ncol, int *nrow, char *errmsg) {
   int   rc  ;
   char *msg ;

   rc = sqlite3_get_table(*db, command, &result, nrow, ncol, &msg) ;
   if(msg != NULL){
      strncpy(errmsg, msg, strlen(errmsg)) ;
   }

   return rc ;
}
/**********************************************************************************************************************************/
char *sqlite3_get_table_2_c(int ncol, int nrow, int len_result) {
   int   i,j,k,n ;
   char *result_table;

   result_table=malloc(ncol*(nrow+1)*len_result);

   /* Note: one extra row! */
   for (j = 0 ; j <= (nrow) ; j ++){
      for (i = 0 ; i < (ncol) ; i ++){
         k = i + j*(ncol) ;

         strncpy(&result_table[k*len_result], result[k], len_result) ;

         for (n = strlen(result[k]) ; n < len_result ; n ++){
            result_table[k*len_result+n] = ' ' ;
         }
      }
   }

   sqlite3_free_table(result) ;
   result = NULL;

   //fprintf(stdout,"%s\n",result_table);fflush(stdout);

   return(result_table) ;
}
/**********************************************************************************************************************************/
