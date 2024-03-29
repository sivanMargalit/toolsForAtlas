#' Adaptive fixed point
#'
#' AdpFixedPoint is a function to perform track segmentation utilizing a
#' first-passage algorithm to determine fixed points where the agent/animal
#' has spent a minimum number of observations (obs_min) within a a certain
#' range (adp_rng) of a fixed point that is continuously reavaluated.
#'
#' @param time.vec absolute time e.g. in ms (ATLAS timestamp)
#' @param x projected longitude in meters
#' @param y projected latitude in meters
#' @param adp_rng adaptive range defining fixed points
#' @param smp_rte sampling rate e.g. in ms (ATLAS)
#' @param obs_min minimum nuber of observations defining a fixed position
#' @param p_lim point limit for leaving current fixed point
#' @param time_gap minimal time gap of missing values refere as exit of adp area (default 5 minutes)
#'
#'
#' @return The function returns an array containing information about each fixed
#'         point including (in order) start time, end time, duration, number of locations,
#'         position quality, median x, median-y, lower x quantile, upper x quantile, lower y quantile, upper y quantile
#
#'
#'
#' Original Code by Ingo Schiffner 2017 (Matlab version)
#' Converted to R by Emanuel Lourie 2018
#'
#' @export

AdpFixedPoint <- function(time.vec,x,y,
                          adp_rng,
                          smp_rte,
                          obs_min,
                          p_lim,
                          time_gap=5*60*1000)
{
  n_x <- which(!is.na(x))
  max_pos <- trunc(length(n_x)/(obs_min+p_lim))
  time.vec <- as.double(time.vec[n_x]) # fix potential problems with integer overlow

  AFPList<-data.frame(start=rep(NA,max_pos),
                      end=rep(NA,max_pos),
                      duration=rep(NA,max_pos),
                      num_loc=rep(NA,max_pos),
                      position_qlt=rep(NA,max_pos),
                      medX=rep(NA,max_pos),
                      medY=rep(NA,max_pos),
                      dummy=rep(NA,max_pos))

  si <- n_x[1]                    # start index
  ei <- n_x[length(n_x)]          # end index
  cfp <- si                       # set initial fixed point

  cfp_i <- 1                      # counter to check if we have a valid fixed point (i.e. exceeds obs_min)
  l_cnt <- 0                      # counter to check if agent is leaving current fixed point (i.e. exceeds p_lim)
  fp_cnt <- 0                     # fixed point counter

  #set startpoint as first point (we do this for those cases were the animal is imidiatley moving)
  # AFPList$start[fp_cnt] <- time.vec[si] # start time
  # AFPList$end[fp_cnt] <- time.vec[si+1] # end time
  # AFPList$duration[fp_cnt] <- 1         # duration (ms)
  # AFPList$num_loc[fp_cnt] <- 2          # number of localizations defining fixed point
  # AFPList$position_qlt[fp_cnt] <- 0     # estimate of position quality
  # AFPList$medX[fp_cnt] <- x[si]         # median x position
  # AFPList$medY[fp_cnt] <- y[si]         # median-y position

  cfp_x <- NULL # current fixed point x
  cfp_y <- NULL # current fixed point y
  cfp_t <- NULL # current fixed point time

  #move through the data
  i <- 1
  while (i<length(n_x))
  {
    # for (i in n_x[-1])
    i <- i+1
    #make sure its a valid position
    if (is.na(x[i]) | is.na(y[i])){
      print(sprintf("AdpFixedPoint: invalid value in x or y at line %d",i))
      next
    }

    #get distance from current fixed point
    dx <- x[i]-x[cfp]
    dy <- y[i]-y[cfp]
    e_dst <- (dx^2 + dy^2)^0.5
    dt<-time.vec[i]-time.vec[i-1]

    if ((e_dst > adp_rng) | (dt>time_gap))# check if agent is out of range
    {
      # increase leaving counter
      l_cnt <- l_cnt + 1
      if ((l_cnt >= p_lim) | (dt>time_gap)) # check if agent has left current fixed point
      {
        if (cfp_i >= obs_min) # check if fixed point has sufficient observations
        {
          #evaluate fixed point
          fp_cnt <- fp_cnt + 1
          AFPList$start[fp_cnt] <-   time.vec[cfp]      #cfp_t[1]                         # start time
          AFPList$end[fp_cnt] <- cfp_t[cfp_i-1]                                # end time
          AFPList$duration[fp_cnt] <- (cfp_t[cfp_i-1]- time.vec[cfp]) #cfp_t[1])                     # duration
          AFPList$num_loc[fp_cnt] <- cfp_i;                                      # number of localizations
          AFPList$position_qlt[fp_cnt] <- round((AFPList$num_loc[fp_cnt]/(1+AFPList$duration[fp_cnt]/smp_rte)),3) # position quality
          AFPList$medX[fp_cnt] <- median(c(x[cfp],cfp_x))                                 # median x position
          AFPList$medY[fp_cnt] <- median(c(y[cfp],cfp_y))                                # median y position
          cfp <- i-l_cnt
        }

        # set new fixed point
        cfp <- cfp+1
        i <- cfp
        cfp_i <- 1
        #reset leaving counter
        l_cnt <- 0

        # reset temp data
        cfp_x <- NULL
        cfp_y <- NULL
        cfp_t <- NULL
      }
    }
    else
    {

      #add data to tmp fixed point list
      cfp_x[cfp_i] <- x[i]
      cfp_y[cfp_i] <- y[i]
      cfp_t[cfp_i] <- time.vec[i]
      cfp_i <- cfp_i +1

      #reset leaving counter
      l_cnt <- 0
    }
  }
  #in case that the last point is part of a stop point, add it to AFPList as the final stop
  if (cfp_i >= obs_min) # check if fixed point has sufficient observations
  {
    #evaluate fixed point
    fp_cnt <- fp_cnt + 1
    AFPList$start[fp_cnt] <-   time.vec[cfp]      #cfp_t[1]                         # start time
    AFPList$end[fp_cnt] <- cfp_t[cfp_i-1]                                # end time
    AFPList$duration[fp_cnt] <- (cfp_t[cfp_i-1]- time.vec[cfp]) #cfp_t[1])                     # duration
    AFPList$num_loc[fp_cnt] <- cfp_i;                                      # number of localizations
    AFPList$position_qlt[fp_cnt] <- round((AFPList$num_loc[fp_cnt]/(1+AFPList$duration[fp_cnt]/smp_rte)),3) # position quality
    AFPList$medX[fp_cnt] <- median(c(x[cfp],cfp_x))                                 # median x position
    AFPList$medY[fp_cnt] <- median(c(y[cfp],cfp_y))                                # median y position
    cfp <- i-l_cnt
  } else if ((fp_cnt+1)<=max_pos){  #set end point to last position (in case track stops mid flight)
    fp_cnt <- fp_cnt + 1
    AFPList$start[fp_cnt] <- time.vec[ei-1]  # start time
    AFPList$end[fp_cnt] <- time.vec[ei]    # end time
    AFPList$duration[fp_cnt] <- 1           # duration
    AFPList$num_loc[fp_cnt] <- 2           # number of localization
    AFPList$position_qlt[fp_cnt] <- 0           # position quality
    AFPList$medX[fp_cnt] <- x[ei]       # median x position
    AFPList$medY[fp_cnt] <- y[ei]       # median y position
  }
  #trunctate output matrix
  AFPList<-AFPList[1:fp_cnt,]
  return(AFPList)
}






#' Mark localizations as perch or move segment
#'
#' MarkStopLoc can be used to combine the perching data (from AdpFixedPoint function)
#' into the localization data. each localization within a perching period mark as
#' assigned to ADP point. and all the othr localizations are movements
#'
#' @param locs.df localizatins data frame (after filtering) with m rows
#' @param ADP.df ADP dataframe - retreived by AdpFixedPoint with n rows
#'
#' @return The function returns a data frame with m rows (as in locs.df),
#'        but numbrt of columns enhaced with information from ADP.df
#'
#' @import dplyr
#' @export
MarkStopLoc<-function(locs.df, ADP.df){
  TagList<-unique(ADP.df$TAG)

  # verify loc.df does not have yet ADP/ Seg.ID fields
  if (!('ADP.ID' %in% colnames(locs.df))){
    locs.df$ADP.ID<-NA
    locs.df$ADP.X<-NA
    locs.df$ADP.Y<-NA
    locs.df$ADP.start<-NA
    locs.df$ADP.end<-NA
    locs.df$ADP.duration<-NA
    locs.df$ADP.qlt<-NA

    locs.df$seg.ID<-NA
  }

  ADP.df<-ADP.df%>%arrange(TAG, start)

  for (aTag in TagList){

    segID<-1
    ADP.of.tag.ix<-which(ADP.df$TAG==aTag)
    print(sprintf("MarkStopLoc(): TAG %s has %d ADP",
                  as.character(aTag), length(ADP.of.tag.ix)))
    ADP.ix.1<-min(ADP.of.tag.ix)
    # mark localizations in movement segment befor first AOP
    seg.ix<-which((locs.df$TAG==aTag) & locs.df$TIME<ADP.df$start[ADP.ix.1])
    if (length(seg.ix)>5){
      locs.df$seg.ID[seg.ix]<-segID
    }

    for (i in ADP.of.tag.ix){

      startTime<-ADP.df$start[i]
      endTime<-ADP.df$end[i]

      loc.in.ADP<-which((locs.df$TAG==aTag) & (between(locs.df$TIME, startTime, endTime)))
      #if (length(loc.in.ADP)!=ADP.df$num_loc[i]){
      #print(sprintf("markStopLoc() ADP %d : nloc=%d =?= %d ",
      #      i, length(loc.in.ADP), ADP.df$num_loc[i]))
      #}
      locs.df$ADP.ID[loc.in.ADP]<-segID
      locs.df$ADP.X[loc.in.ADP]<-ADP.df$medX[i]
      locs.df$ADP.Y[loc.in.ADP]<-ADP.df$medY[i]
      locs.df$ADP.start[loc.in.ADP]<-ADP.df$start[i]
      locs.df$ADP.end[loc.in.ADP]<-ADP.df$end[i]
      locs.df$ADP.duration[loc.in.ADP]<-ADP.df$duration[i]
      locs.df$ADP.qlt[loc.in.ADP]<-ADP.df$position_qlt[i]

      # mark next movement segments after ADP
      segID<-segID+1
      if (i<nrow(ADP.df)){
        seg.ix<-which(between(locs.df$TIME,ADP.df$end[i]+1,ADP.df$start[i+1]-1))
      }
      else{
        seg.ix<-which(locs.df$TIME>ADP.df$end[i])
      }

      if (length(seg.ix)>0){
        locs.df$seg.ID[seg.ix]<-segID
      }
    }
  }
  return (locs.df)
}





