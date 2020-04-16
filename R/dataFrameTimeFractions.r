tracing<-FALSE
dbgPrint<-function(msg){
  if (tracing){
    print(msg)
  }
}

#' add time categories for further aggregate atlas data (locallizations/detections) data
#'
#' TODO...
#'
#' @param df  data frame whith a column names "TIME" , its values are epoch time in milisec resolution in UTCtime zone
#' @param fieldsTypes  list of time fields types text:
#'  - "date"
#'  - "hour", "min"
#'  - "day"
#'  @param refTime - reference time (default 0)
#'
#' @return data.frame with added columns in format <timeUnit><number>
#'
#' @examples
#' TODO
#'
#' @export
addTimeDateFields<-function(df, fieldsTypes, refTime=0){
  hasTimeCol<-"TIME" %in% colnames(df)
  if (hasTimeCol){
    refHour<-as.numeric(as.POSIXct(refTime, tz="UTC",origin="1970-01-01"))
    dateIndex<-grep("date", fieldsTypes, value=FALSE)
    if (length(dateIndex)==1) {
       df$date<-as.Date(as.POSIXct(df$TIME/1000, tz="UTC",origin="1970-01-01"))
    }

    dayIndex<-grep("day", fieldsTypes, value=FALSE)
    dbgPrint(paste0("1. check day "))
    if (length(dayIndex)>0) {
      x<-gsub("([0-9]*) ?day", "\\1",fieldsTypes[dayIndex])
      for (i in x) {
        if (i=="") {
          n<-1
          colName<-"day"
        }
        else {
          n<-as.numeric(i)
          colName<-paste0("days",n)
        }
        dbgPrint(paste("adding column",colName, "n=",n))
        calcColIx<-which(colnames(df)==colName)
        if (length(calcColIx)>0){
           df[,calcColIx]<-(floor(((df$TIME/1000)-refHour)/(3600*24*n))*n)
        }
        else{
          df$newCol<-(floor(((df$TIME/1000)-refHour)/(3600*24*n))*n)
          newColIx<-which(colnames(df)=="newCol")
          colnames(df)[newColIx] <- colName
        }
      }
    }


    hourIndex<-grep("hour", fieldsTypes, value=FALSE)
    dbgPrint(paste0("2. check hour "))
    if (length(hourIndex)>0) {
        x<-gsub("([0-9]*) ?hour", "\\1",fieldsTypes[hourIndex])
        for (i in x) {
          if (i=="") {
            n<-1
            colName<-"hour"
          }
          else {
            n<-as.numeric(i)
            colName<-paste0("hours",n)
          }
          dbgPrint(paste("adding column",colName, "n=",n))
          calcColIx<-which(colnames(df)==colName)
          if (length(calcColIx)>0){
            df[,calcColIx]<-(floor(((df$TIME/1000)-refHour)/(3600*n))*n)%%24
          }
          else{
            df$newCol<-(floor(((df$TIME/1000)-refHour)/(3600*n))*n)%%24
            newColIx<-which(colnames(df)=="newCol")
            colnames(df)[newColIx] <- colName
          }
        }
    }

    minIndex<-grep("min", fieldsTypes, value=FALSE)
    dbgPrint(paste0("4. check minutes. indexes: ", minIndex))
    if (length(minIndex)>0) {
      x<-gsub("([0-9]*) ?min", "\\1",fieldsTypes[minIndex])
      for (i in x) {
        if (i=="") {
          n<-1
          colName<-"min"
        }
        else {
          n<-as.numeric(i)
          colName<-paste0("min",n)
        }
        dbgPrint(paste("adding column",colName))
        calcColIx<-which(colnames(df)==colName)
        if (length(calcColIx)>0){
          df[,calcColIx]<-(floor(((df$TIME/1000)-refHour)/(60*n))*n)%%(24*60)
        }
        else{
          df$newCol<-(floor(((df$TIME/1000)-refHour)/(60*n))*n)%%(24*60)
          newColIx<-which(colnames(df)=="newCol")
          colnames(df)[newColIx] <- colName
        }
      }
    }
  }
  return(df)

}
