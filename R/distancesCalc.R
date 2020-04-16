#library("dplyr")
#speed based filter by Ingo Schiffner 2017
#calculates speed between consecutive localizations and filters out segments exceeding spd_lim
#assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#returns a data.frame containing filtered x,y and time(t)

options(digits=14)
#' calculate distance between sequence locations
#'
#' TODO...
#'
#' @param x x vector
#' @param y y vector
#' @param t time vector
#'
#' @return dataframe
#'
#' @examples
#' TODO
#'
#' @export
distances.spd.Calc <- function(x,y,t)
{
  dx = diff(x,1)
  dy = diff(y,1)
  dt = as.integer(diff(t,1))/1000
  de = (dx^2 + dy^2) ^ 0.5
  de=c(NA,de)
  dt=c(NA,dt)
  spd=de/dt
  dist <- data.frame("X"=x,"Y"=y,"TIME"=t, "dT"=dt, "distance"=de,"spd"=spd)
  return (dist)
}

# add auclidien distance and delta Time  values
#' calculate distance at space and time columns
#'
#' TODO...
#'
#' @param input.loc.df localization data frame
#' @param merics.vec   which metrics to add (distance, dT, spd)
#'
#' @return input.loc.df
#'
#' @examples
#' TODO
#' @import dplyr
#'
#' @export
addDistanceSpeed<-function(input.loc.df,
                           merics.vec=c("distance","dT","spd")){

  if (is.null(merics.vec) | (length(merics.vec)==0)){
    print(sprintf("addDistanceSpeed(): missing merics.vec. default is c('distance','dT','speed')"))
    return(input.loc.df)
  }
  if (length(c("distance","dT","speed","spd") %in% merics.vec) ==0){
    print(sprintf("addDistanceSpeed(): invalide or empty merics.vec. default is c('distance','dT','spd')"))
    return(input.loc.df)
  }
  newColumns<-merics.vec[(merics.vec %in% c("distance","dT","speed","spd"))]
  newColumns<-unique(sub("speed","spd",newColumns))
  baseColumns<-c("X","Y","TIME")

  prevMetricsIx<-which(colnames(input.loc.df) %in% newColumns)
  if(length(prevMetricsIx)>0){
    input.loc.df<-input.loc.df[,-prevMetricsIx]
  }
  output.loc.df<-NULL
  for (tg in levels(factor((input.loc.df$TAG)))){
    tag.df<-input.loc.df%>%filter(TAG==tg)%>%arrange(TIME)
    print (sprintf("addDistanceCol tag=%s [%d rows] time=%s",tg,nrow(tag.df),Sys.time()))

    dist.df<-distances.spd.Calc(x=tag.df$X,y=tag.df$Y,t=tag.df$TIME)
    #locColNames<-c(colnames(tag.df),"distance","spd")
    columnsIx<-colnames(dist.df)[which(colnames(dist.df) %in% c(newColumns,baseColumns))]
    merge.dist.df<-merge(x=tag.df,
                        y=dist.df[columnsIx],
                        by=c("X","Y", "TIME"),
                        all=TRUE)
    merge.dist.df<-merge.dist.df%>%arrange(TIME)

    #merge.spd.df<-merge.spd.df[,locColNames]
    output.loc.df<-rbind(output.loc.df, merge.dist.df)
  }
  print (sprintf("addDistanceCol end time time=%s",Sys.time()))
  return(output.loc.df)
}
