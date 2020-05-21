#' basic median filter by Ingo Schiffner 2017
#' calculates median x y localizations and time (t) over a predefined window (win) in ms
#' assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#' returns a data.frame containing x,y and time(t) and cnt is number of localizations used for median calculation
#'
#' @param x x
#' @param y y
#' @param t time
#' @param win window
#'
#' @return data frame with median values
#'
#' @examples
#' TODO
#'
#'
#' @export

MedSmoothing <- function(x,y,t,win)
{
  #set bins
  tb = trunc((t - t[1])/win)+1
  mx <- aggregate(x ~ tb, FUN = median)
  my <- aggregate(y ~ tb, FUN = median)
  mt <- aggregate(t ~ tb, FUN = median)
  msum<-aggregate(x ~ tb, FUN = length)
  mtf <- data.frame(x=as.numeric(mx$x),y=as.numeric(my$y),t=as.numeric(mt$t),cnt=msum)
  return (mtf)
}

#' median convolution
#'
#' calculates median x y localizations and time (t) over a predefined window (win) in ms
#' assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#' returns a data.frame containing x,y and time(t) and cnt is number of localizations used for median calculation
#'
#' @param x x
#' @param y y
#' @param t time
#' @param win window (ms)
#' @param freq (ms)
#' @param skip (#samples) default =1
#'
#' @return data frame with median values
#'
#' @examples
#' TODO
#'
#' @import dplyr
#'
#' @export
MedConvSmoothing <- function(x,y,t,win,freq,skip=1)
{
  tmp.t<-as.numeric(t)
  tmp.x<-x
  tmp.y<-y
  steps<-win/freq
  startTime<-floor(t[1]/1000)*1000
  mtf<-c()
  for (i in seq(from=1, to=steps, by=skip)){

    #set bins
    tb = trunc((tmp.t - startTime)/win)+1
    mx <- aggregate(tmp.x ~ tb, FUN = median)
    my <- aggregate(tmp.y ~ tb, FUN = median)
    mt <- aggregate(tmp.t ~ tb, FUN = median)
    msum<-aggregate(tmp.x ~ tb, FUN = length)
    mtf<- rbind(data.frame(x=as.numeric(mx[,2]),y=as.numeric(my[,2]),t=as.numeric(mt[,2]),cnt=as.numeric(msum[,2])),
                mtf)
    startTime<-startTime+freq
    skipX<-which(tmp.t<startTime)
    if (length(skipX)>0){
      tmp.t<-tmp.t[-skipX]
      tmp.x<-tmp.x[-skipX]
      tmp.y<-tmp.y[-skipX]
    }
  }
  mtf<-as.data.frame(mtf%>%arrange(t))
  mtf<-mtf[-which(duplicated(mtf$t)),]
  return (mtf)
}
