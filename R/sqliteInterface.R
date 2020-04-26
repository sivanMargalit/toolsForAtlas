#' interface to ATLAS sqlite tables
#'
#' retreive table from sqlite file to R data frame
#'
#' @param dbn sqlite file name
#' @param tbl table name. default value "localizations" table
#'
#' @return table content as data.frame
#'
#' @examples
#' loadFromSQLite(sqliteFieName)
#'
#' @export
loadFromSQLite<- function(dbn, tbl="localizations")
{
  # connect to the sqlite file
  con = RSQLite::dbConnect(RSQLite::SQLite(),dbname=dbn)
  query = paste0('select * from "',tbl,'"')
  p1 = RSQLite::dbGetQuery(con,query)
  RSQLite::dbDisconnect(con)
  return(p1)
}

#' write to ATLAS sqlite tables
#'
#' TODO...
#'
#' @param dbn sqlite file name
#' @param tbl table name
#'
#'
#' @examples
#' saveIntoSQLite(<sqliteFieName>)
#'
#' @export
saveIntoSQLite<- function(dbn,tbls,append=FALSE)
{
  # connect to the sqlite file
  con = RSQLite::dbConnect(RSQLite::SQLite(),dbname=dbn)

  for (ix in c(1:length(tbls))){
    tryCatch({
      RSQLite::dbWriteTable(
          conn = con,
          name = names(tbls)[ix],
          tbls[[ix]],
          append=append,
          row.names = F
        )
      }    ,
    warning = function(w) {
      print (paste("[WARNING] dbWriteTable to rslt Sqlite: ", w))
      warning(w)
    }, error = function(e) {
      print (paste("[ERROR]  dbWriteTable to rslt Sqlite: ", e))
    }, finally = {
      #cleanup-code
    })
  }
  RSQLite::dbDisconnect(con)
}

# relative information:
# https://stackoverflow.com/questions/46316992/adding-value-to-existing-database-table-in-rsqlite
sqliteSendQuery<-function(dbn,aQuery){
  require (DBI)
  con = RSQLite::dbConnect(RSQLite::SQLite(),dbname=dbn)
  tryCatch({
    RSQLite::dbSendQuery(con,aQuery)
  },
  warning = function(w) {
      print (paste("[WARNING] on sqliteQuery: ", w))
      warning(w)
  }, error = function(e) {
      print (paste("[ERROR] on sqliteQuery: ", e))
  }, finally = {
      #cleanup-code
    })
  RSQLite::dbDisconnect(con)
}

# relative information:
# https://stackoverflow.com/questions/46316992/adding-value-to-existing-database-table-in-rsqlite
sqliteGetQuery<-function(dbn,aQuery){
  require (DBI)
  rslt<-NULL
  con = dbConnect(RSQLite::SQLite(),dbname=dbn)
  tryCatch({
    rslt<-dbGetQuery(con,aQuery)
  },
  warning = function(w) {
    print (paste("[WARNING] on sqliteQuery: ", w))
    warning(w)
  }, error = function(e) {
    print (paste("[ERROR] on sqliteQuery: ", e))
  }, finally = {
    #cleanup-code
  })
  dbDisconnect(con)
  return(rslt)
}
