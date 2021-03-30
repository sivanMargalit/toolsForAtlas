
#' calcTagRate
#' define tags broadcast  rate
#'
#' @param tags.lst     input data frame contains TAG, TIME
#' @param timeUnit      timestampe units. default : ms (means milisec)
#'
#' @return data frame with tag name and freq
#'
#' @examples
#' TODO
#'
#'
#' @export
calcTagRate<-function(tags.lst, timeunit="ms"){
  rate.df<-data.frame("TAG"=c(), "RATE"=c())
  for (i in 1:length(tags.lst)){
    tg<-unique(tags.lst[[i]]$TAG)
    df<-tags.lst[[i]]%>%arrange(TIME)
    dT<-diff(df$TIME)
    rate<-min(dT)
    if (timeunit == "ms") {
      rate<-round(rate/1000)
    }
    rate.df<-rbind(data.frame("TAG"=tg, "RATE"=rate),rate.df)
  }
  return(rate.df)
}
