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
#' @import RSQLite
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
#' @import RSQLite
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


#' exportForKamadata requires as input sqlite file with empty tables known by Kamadata
#' It update the localization table with the data in loc.df data frame
#'
#'
#'
#' @param loc.df input data frame should include the fields: TAG (number up to 10 digits),
#' TIME (13 digit) - epoch time in milisec, recomended in UTC Timezone
#' X, Y (double) - coordinates in ITM marctor
#' @param sqliteName sqlite file name with atlas table
#' @param append default TRUE. means append data on previous data
#'
#'
#' @examples
#' exportForKamadata(my.df, "kamadata.sqlite")
#'
#'
#' @export
exportForKamadata<-function(loc.df, sqliteName, append=TRUE){
  if(!file.exists(sqliteName)){
    # check if excel file exists
    print(sprintf("exportForKamadata(): file %s not exists", sqliteName))
    return
  }
  else{
    saveIntoSQLite(sqliteName,list("LOCALIZATIONS"=loc.df),append = append)
  }

}


#' generate sqlite file in atlas format  for KAMADATA
#'
#' It update the localization table with the data in loc.df data frame
#'
#'
#'
#' @param loc.df input data frame should include the fields: TAG (number up to 10 digits),
#' TIME (13 digit) - epoch time in milisec, recomended in UTC Timezone
#' X, Y (double) - coordinates in ITM marctor
#' @param sqliteName sqlite file name with atlas table
#' @param append default FALSE. means do not append data on previous data
#' @param overwrite default FALSE. means co not overwrite if file exists.  If overwrite parameter is TRUE than append ignored
#' @param atlas.system default 'hula'.
#'
#'
#' @examples
#' exportForKamadata("kamadata.sqlite", my.df, append=TRUE, atlas.system="harod")
#'
#' @import DBI
#' @import RSQLite
#'
#' @export
exportForKamadata<- function(sqliteName,
                             loc.df=NULL,
                             append=FALSE,
                             overwrite=FALSE,
                             atlas.system="hula"){

  if (file.exists(sqliteName)){
    if ((append==FALSE) & (overwrite==FALSE)){
      print(sprintf("File %s already exists and append=FALSE & overwrite=FALSE. please choose different file name or update write permissions",
                    sqliteName))
      return(0)
    }
  }

  # connect to the sqlite file - if not exists it create empty file
  con = RSQLite::dbConnect(RSQLite::SQLite(),dbname=sqliteName)

  #if atlas table not exists - create tables
  tbls<-dbListTables(con)
  if (length(tbls)==0){
    con<-generateAtlasEmptySQlite(con,atlas.system)
    append<-TRUE
  }
  else if (overwrite == TRUE){
    print(sprintf("File %s already exists but overwrite permission will delete previous data",
                  sqliteName))
    con<-cleanAtlasData(con)
    append<-TRUE
  }

  RSQLite::dbDisconnect(con)
  #unlink(sqliteName)


  if (is.null(loc.df)){
    print(sprintf("File %s has now empty ATLAS tables",
                  sqliteName))
  }else{
    saveIntoSQLite(sqliteName,
                   list("LOCALIZATIONS"=loc.df),
                   append = append)
  }
}


generateAtlasEmptySQlite<-function(conn, atlas.system="hula"){
  rslt<-dbExecute(conn,
                  "CREATE TABLE LOCALIZATIONS(
                TAG           bigint(20) NOT NULL,
                TX            bigint(20) DEFAULT NULL,
                TIME          bigint(20) NOT NULL,
                X             double     DEFAULT NULL,
                Y             double     DEFAULT NULL,
                Z             double     DEFAULT NULL,
                FREQ          double  DEFAULT NULL,
                DIM           int(11) DEFAULT NULL,
                NBS           int(11) DEFAULT NULL,
                NCONSTRAINTS  int(11) DEFAULT NULL,
                PENALTY       float   DEFAULT NULL,
                GRADNRM       float   DEFAULT NULL,
                VARX          float   DEFAULT NULL,
                VARY          float   DEFAULT NULL,
                VARZ          float   DEFAULT NULL,
                COVXY         float   DEFAULT NULL,
                COVXZ         float   DEFAULT NULL,
                COVYZ         float   DEFAULT NULL,
                PRIMARY KEY   (TAG,TIME)
            );")
  #dbFetch(rs)
  #dbClearResult(rs)
  rslt<-dbExecute(conn,
                  "CREATE TABLE DETECTIONS (
                BS          int(11)      NOT NULL,
                TAG         bigint(20)   NOT NULL,
                TX          bigint(20)   NOT NULL,
                TIME        bigint(20)    NOT NULL,
                SAMPLES_CLK double       NOT NULL,
                SNR         float        DEFAULT NULL,
                RSSI        float        DEFAULT NULL,
                HEADROOM    float        DEFAULT NULL,
                GAIN        float        DEFAULT NULL,
                PRIMARY KEY (TAG, BS, TIME)
              );")
  rslt<-dbExecute(conn,
                  "CREATE TABLE GPS_LOCALIZATIONS (
                ID            bigint(20) NOT NULL,
                TIME          bigint(20) NOT NULL,
                LAT           double     DEFAULT NULL,
                LON           double     DEFAULT NULL,
                ASL           float      DEFAULT NULL,
                HDOP          float      DEFAULT NULL,
                PRIMARY KEY (ID, TIME)
              );")

  rslt<-dbExecute(conn,
                  "CREATE TABLE PROPERTIES (
                KEY          VARCHAR      NOT NULL,
                VALUE        VARCHAR   NOT NULL,
                PRIMARY KEY (KEY)
              );")

  rslt<-dbExecute(conn,
                  "CREATE INDEX DET_TIME_IDX ON DETECTIONS (TIME);")

  rslt<-dbExecute(conn,
                  "CREATE INDEX LOC_TIME_IDX ON LOCALIZATIONS (TIME);")

  rslt<-dbExecute(conn,
                  "CREATE INDEX GPS_TIME_IDX ON GPS_LOCALIZATIONS (TIME) ;")


  dbWriteTable(conn,
               "PROPERTIES",
               data.frame("KEY"=c("atlas-system"),
                          "VALUE"=c(atlas.system)),
               append=T,
               row.names = F)
  return(conn)
}

cleanAtlasData<-function(conn){
  rslt<-dbExecute(conn,
                  "delete from LOCALIZATIONS;")
  rslt<-dbExecute(conn,
                  "delete from DETECTIONS;")
  rslt<-dbExecute(conn,
                  "delete from  GPS_LOCALIZATIONS;")
  return(conn)
}
