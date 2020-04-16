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
#'
#' @import dplyr
triangularSmooth <- function(x,y,t){
  cnt<-length(t)
  locFixed<-0
  cntNoDist<-0


  distances<-sqrt((x[2:cnt]-x[1:(cnt-1)])^2 + (y[2:cnt]-y[1:(cnt-1)])^2)
  dt<-t[2:cnt]-t[1:(cnt-1)]/1000
  spd<-distances/dt


  i=2
  for (i in 2:(cnt-1)){
    #d1<-distances[i-1]
    #d2<-distances[i]
    d1=sqrt((x[i]-x[i-1])^2 + (y[i]-y[i-1])^2) # distance i to i-1 (side1)
    d2<-sqrt((x[i]-x[i+1])^2 + (y[i]-y[i+1])^2) # distance i to i+1 (side2)
    d3<-sqrt((x[i-1]-x[i+1])^2 + (y[i-1]-y[i+1])^2); # distance i-1 to i+1 (base)

    if ((d1>0) & (d2>0)){
      if ((d3*1.5)<=d1){                         # if its a triangle with the base small
        x[i]=(x[i-1]+x[i+1])/2;
        y[i]=(y[i-1]+y[i+1])/2;
        t[i]=(t[i-1]+t[i+1])/2;
        locFixed<-locFixed+1
      }
    }else{
      cntNoDist<-cntNoDist+(d1>0)
    }
  }


  smooth.df<-data.frame(X=x,
                        Y=y,
                        TIME=t)
  print(sprintf("triangularSmooth(): %d sharp angles fixed and %d localization with no distance found",
        locFixed,cntNoDist))
  return(smooth.df)
}

