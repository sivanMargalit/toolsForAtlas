#'
#' velocity filter removes a location according to velocity
#' @param data data frame of single animal
#' @param spdThreshold threshold for speed, (in units m/sec). default 15 m/sec
#' @param x name of X column (meters units). default name "X"
#' @param y name of Y column (meters units). default name "Y"
#' @param time name of time column (epoch time in milisec). default name "TIME"
#' @param steps how close the localizations neibours. default 1
#' @param printfiltthis argument silence printout when in FALSE. default = TRUE
#'
#' @return cleaned data frame
#'
#' details:
#' it removes a location whenever both the velocity from the previous point to it (v_in) and the velocity from it to the next point (v_out) are greater than  spdThreshold
#' it repeats the filtering with different step size:
#' step=0 means that the velocity is calculated between nearest neighbors (in time)
#' step=1 means that the velocity is calculated between second nearest neighbors
#' the input variable "steps" determines up to which neighbor to check velocity (default=1)
#' thus it can filter locations in case that a set of points up to size "steps" was drifted away
#' @import data.table
#' @import assertthat
#' @export
velocity_filter <- function (data,
                             spdThreshold=15,
                             x = "X", y = "Y", time = "TIME",
                             steps=1,
                             printfilt=T)
{
  for(i in 1:steps){
    spd <- matl_get_speed(data,
                          x=x,
                          y=y,
                          time=time,
                          type="in",
                          step = i)*1000
    KEEP <- (spd<spdThreshold)|(shift(spd,-i)<spdThreshold)
    KEEP[is.na(KEEP)] <- TRUE
    data<-data[which(KEEP),]
    if(printfilt){
      print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
    }
  }
  return(data)
}

#'
#' velocity filter removes a location  according to distance
#' @param data data frame of single animal
#' @param distThreshold threshold for distance, (in units m). default 15*8 m
#' @param x name of X column (meters units). default name "X"
#' @param y name of Y column (meters units). default name "Y"
#' @param time name of time column (epoch time in milisec). default name "TIME"
#' @param steps how close the localizations neibours. default 1
#' @param printfiltthis argument silence printout when in FALSE. default = TRUE
#'
#' @return cleaned data frame
#'
#' details:
#' it removes a location whenever both the distance from the previous point to it and the distance from it to the next point are greater than distThreshold
#' it repeats the filtering with different step size:
#' step=0 means that the distance is calculated between nearest neighbors (in time)
#' step=1 means that the distance is calculated between second nearest neighbors
#' the input variable "steps" determines up to which neighbor to check distance (default=1)
#' thus it can filter locations in case that a set of points up to size "steps" was drifted away
#'
#' @export
distance_filter <- function (data,
                             distThreshold=15*8,
                             x = "X", y = "Y", time = "TIME",
                             steps=1,
                             printfilt=T)
{
  for(i in 1:steps){
    dst <- matl_simple_dist(data,x=x,y=y,step = i)
    KEEP <- (dst<distThreshold)|(shift(dst,i)<distThreshold)
    KEEP[is.na(KEEP)] <- TRUE
    data<-data[which(KEEP),]
    if (printfilt) {
      print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
    }
  }
  return(data)
}

#' calculates a distance between subsequent points (or the next "step" point)
#' used within the filters
matl_simple_dist <- function (data,
                              x = "x",
                              y = "y",step=1)
{
  assertthat::assert_that(is.data.frame(data), is.character(x),
                          is.character(y), msg = "simpleDist: some data assumptions are not met")
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - step)]
    x2 <- data[[x]][(1+step):nrow(data)]
    y1 <- data[[y]][seq_len(nrow(data) - step)]
    y2 <- data[[y]][(1+step):nrow(data)]
    dist <- c(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }
  else {
    dist <- NA_real_
  }
  return(dist)
}

#' calculates a speed between subsequent points (or the next "step" point)
#' used within the filters
matl_get_speed <- function (data,
                            x = "x", y = "y", time = "time",
                            type = "in",
                            step=1)
{
  # atlastools::atl_check_data(data, names_expected = c(x, y, time))
  data.table::setorderv(data, time)
  distance <- matl_simple_dist(data, x, y,step)
  # distance <- distance[(step+1):length(distance)]
  dtime <- data[[time]][(step+1):nrow(data)]-data[[time]][1:(nrow(data)-step)]
  # time <- c(NA, diff(data[[time]]))
  speed <- distance/dtime
  if (type == "in") {
    speed <- c(rep(NA,step),speed)
  }
  else if (type == "out") {
    speed <-c(speed,rep(NA,step))
  }
  return(speed)
}



# old version - translated from Adi Wiler code
# filterByVelocity<-function(locs,
#                            dataLegend=c("X","Y","TIME"),
#                            options=c("v_filter_max_v"=20,
#                                      "min_time_dif_for_stats"=10,
#                                      "v_filter_time_dif"=10)) {
#   stats.timeDifs <- NULL;
#   stats.fractionAfterVelocityFilter<-NULL;
#   t1<-Sys.time()
#
#   out=locs;
#
#   VELOCITY_UPPER_BOUND<-options[["v_filter_max_v"]]
#   NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY=options[["min_time_dif_for_stats"]] #5;
#   TIME_BOUND=options[["v_filter_time_dif"]];
#   X.col<-which(colnames(locs) %in% dataLegend[1])
#   Y.col<-which(colnames(locs) %in% dataLegend[2])
#   time.col<-which(colnames(locs) %in% dataLegend[3])
#   xy=locs[,c(X.col,Y.col)];
#   colnames(xy)<-c("X","Y")
#   time=locs[,time.col]
#   end.ix<-nrow(xy)
#
#   dists=calcDist(xy[2:end.ix,],xy[1:(end.ix-1),])
#   timeDifs<-round((time[2:end.ix]-time[1:(end.ix-1)])/1000)
#
#
#   stats.timeDifs=c(stats.timeDifs,
#                        timeDifs[which(timeDifs>NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY)]);
#   if (length(dists)==length(timeDifs)){
#     velocities<-dists/timeDifs
#   } else {
#     velocities=0
#   }
#   if (sum(velocities[which(velocities<0)])>1){
#       print('WARNING: time seems to not be in ascending order, some negative velocities found.');
#       print(paste0('# such points: ',sum(velocities[which(velocities<0)])));
#   }
#
#   reachedEnd<-FALSE;
#   reliablePoints<-rep(FALSE,nrow(xy))
#   i<-reliable(velocities,timeDifs,options)
#
#   if (is.null(i)){
#     print('WARNING: no first reliable point found, check parameters')
#     reachedEnd=TRUE
#   }else{
#     reliablePoints[i]=TRUE
#   }
#
#   while (!reachedEnd){
#     for (j in c((i+1):nrow(xy))){
#       if (j==nrow(xy)){
#         reachedEnd=TRUE;
#       }
#
#       timeDifference=round(time[j]-time[i])/1000
#       if (timeDifference>=TIME_BOUND){
#         reliableRslt<-reliable(velocities[j:end.ix],timeDifs[j:end.ix],options)
#         if (is.null(reliableRslt)){
#           reachedEnd=TRUE
#         } else{
#           i<-j-1+reliableRslt[1]
#           #print(sprintf("next reliable ix: %d", i))
#           reliablePoints[i]=TRUE
#         }
#         break
#       } else{
#
#         distance<-calcDist(xy[i,],xy[j,])
#         velocity<-distance/timeDifference
#         if (velocity<VELOCITY_UPPER_BOUND){
#           i<-j
#           reliablePoints[i]=TRUE
#           break
#         }
#       }
#     }
#   }
#   stats.fractionAfterVelocityFilter[i]=sum(reliablePoints)/nrow(xy)
#   out=out[reliablePoints,];
#
#   t2<-Sys.time()
#   print(sprintf("%s filterByVelocity(): calculte doration %.2f sec",
#                 Sys.time(),
#                 (as.double(t2-t1, units="secs"))))
#
#
#   return(out)
# }
#
# calcDist<-function(vec2, vec1){
#   return(sqrt((vec2$X-vec1$X)^2+(vec2$Y-vec1$Y)^2))
# }
#
# reliable<-function(relevantVelocities,
#                    relevantTimeDifs,
#                    options){
#   VELOCITY_UPPER_BOUND<-options[["v_filter_max_v"]]
#   NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY=options[["min_time_dif_for_stats"]] #5;
#   TIME_BOUND=options[["v_filter_time_dif"]];
#
#   if (length(relevantVelocities)<NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY){
#     reliableInd=NULL
#   } else{
#     inds<-rep(TRUE,length(relevantVelocities))
#
#     end.ix<-length(relevantVelocities)
#     for (k in c(1:NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY)){
#       velframe<-c(relevantVelocities[k:end.ix],rep(VELOCITY_UPPER_BOUND,k-1))
#       timedifFrame<-c(relevantTimeDifs[k:end.ix],rep(TIME_BOUND,k-1))
#       inds<-inds & (velframe<VELOCITY_UPPER_BOUND) & (inds<=TIME_BOUND)
#     }
#     if (length(which(inds))>0){
#       reliableInd=which(inds)[1];
#     } else{
#       reliableInd<-NULL
#     }
#   }
#   return(reliableInd)
# }
