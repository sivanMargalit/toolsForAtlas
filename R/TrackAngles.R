#' calculate anglebetween each 3 loclizations
#'
#' TODO...
#'
#' @param x vector x
#' @param y vector y
#'
#' @return vector of angles
#'
#' @examples
#' TODO
#'
#' @export
TrackAngles<-function(x, y){
  dx<- diff(x) #delta x
  dy<- diff(y) #delta y
  edst <- (dx^2 + dy^2)^0.5 #euclidean distance
  norm.dx<-dx/edst
  norm.dy<-dy/edst
  norm.dx[which(is.na(norm.dx))]<-0
  norm.dy[which(is.na(norm.dy))]<-0
  cnt<-length(norm.dx)
  inner.prod<-(norm.dx[1:(cnt-1)]*(-norm.dx[2:cnt]))+(norm.dy[1:(cnt-1)]*(-norm.dy[2:cnt]))
  inner.prod<-round(inner.prod, 5)
  angl<-(acos(inner.prod) * 180 / pi)*sign((norm.dy[1:(cnt-1)]*(-norm.dy[2:cnt])))
  angl<-c(NA,angl,NA)
  return(angl)
}

#' clean Shrp angle with quick movement - it not realistic
#'
#' TODO...
#'
#' @param df one data frame
#' @param anglTh sngle threshold
#' @param distTh distance threshold
#' @param timeFrame timeframe
#'
#' @return cleaned data frame
#'
#' @examples
#' TODO
#'
#' @export
cleanShrpFastPath<-function(df, anglTh=10, distTh=100, timeFrame){
  df<-df%>%arrange(TIME)
  startTime<- min(df$TIME)
  endTime<-max(df$TIME)
  #assume timeFrame smaller than endTime-startTime
  t1<-startTime
  t2<-t1+timeFrame
  originCnt<-nrow(df)
  cnt<-0
  while (t2<endTime){
    candidates_ix<-which(between(df$TIME, t1-5, t2+5) &
                           (abs(df$angl)<anglTh) & (df$spd>=distTh))
    if (length(candidates_ix)>0){
      ix<-candidates_ix[1]
      print(sprintf("delete outlier at time %s, angl=%.2f, dist=%.1f, spd=%2f",
                    df$dateTime[ix],df$angl[ix], df$distance[ix], df$spd[ix] ))
      df$dT[ix+1]=(df$TIME[ix+1]-df$TIME[ix-1])/1000
      df$distance[ix+1]=sqrt((df$X[ix+1]-df$X[ix-1])^2+(df$Y[ix+1]-df$Y[ix-1])^2)
      df$spd[ix+1]=df$distance[ix+1]/df$dT[ix+1]
      df<-df[-ix,]
      t1<-df$TIME[ix]
      cnt<-cnt+1
      t2<-t1+timeFrame
    } else{
      t1<-t2
      t2<-t1+timeFrame
    }

  }
  print(sprintf("remove %d outliers (%2f%%)", cnt, (cnt/originCnt)*100))
  return(df)
}
