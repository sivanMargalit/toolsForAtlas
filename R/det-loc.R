#' merge BS from detections from localizations
#'
#' TODO...
#'
#' @param loc.df data frame of atlas localizations (should include at least TAG, TIME, X,Y)
#' @param det.df data frame of atlas detections (should include at least TAG, TIME, BS)
#'
#' @return merged table
#'
#' @examples
#' loadFromSQLite(<sqliteFieName>)
#'
#' @export
mergeLocDet<-function(loc.df, det.df){

  det.df<-det.df%>%
    rename(TIME.DET=TIME)

  loc.df<-loc.df%>%
    mutate("TIME_plus_1"=TIME+1,
           "TIME_minus_1"=TIME-1)

  merge.det.locTime<-merge(x=det.df,
                           y=loc.df[,c("TAG","TIME","NBS")],
                           by.x=c("TAG","TIME.DET"),
                           by.y=c("TAG","TIME"),
                           all=TRUE)
  merge.det.locTime<-merge.det.locTime%>%
    mutate("TIME"=ifelse(!is.na(NBS),TIME.DET, NA))

  merge.det.locTime<-merge(x=merge.det.locTime,
                           y=loc.df[,c("TAG","TIME_plus_1","TIME")],
                           by.x=c("TAG","TIME.DET"),
                           by.y=c("TAG","TIME_plus_1"),
                           all.x=TRUE,
                           suffixes=c("",".+1"))

  merge.det.locTime<-merge(x=merge.det.locTime,
                           y=loc.df[,c("TAG","TIME_minus_1","TIME")],
                           by.x=c("TAG","TIME.DET"),
                           by.y=c("TAG","TIME_minus_1"),
                           all.x=TRUE,
                           suffixes=c("",".-1"))
  merge.det.locTime<-merge.det.locTime%>%
    rename(TIME.LOC=TIME)%>%
    mutate(TIME.LOC=ifelse(!is.na(`TIME.+1`),`TIME.+1`,TIME.LOC))%>%
    mutate(TIME.LOC=ifelse(!is.na(`TIME.-1`),`TIME.-1`,TIME.LOC))%>%
    select(-`TIME.-1`,-`TIME.+1`)

  loc.df<-loc.df%>%
    select(-TIME_plus_1, -TIME_minus_1)

  merge.loc.det<-merge(x=merge.det.locTime%>%select(-NBS),
                       y=loc.df,
                       by.x=c("TAG", "TX", "TIME.LOC"),
                       by.y=c("TAG", "TX", "TIME"),
                       all=TRUE)

  return(merge.loc.det)
}
