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
  loc.df$mergeTime<-round(loc.df$TIME/10,0)
  det.df$mergeTime<-round(det.df$TIME/10,0)

  merge.loc.det<-merge(x=loc.df,
                       y=det.df,
                       by=c("TAG","mergeTime","TX"),
                       all=TRUE,
                       suffixes=c(".LOC",".DET"))

  return(merge.loc.det)
}
