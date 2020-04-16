#' filter by velocity
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
filterByVelocity<-function(locs,
                           dataLegend=c("X","Y","TIME"),
                           options=c("v_filter_max_v"=20,
                                     "min_time_dif_for_stats"=10,
                                     "v_filter_time_dif"=10)) {
  stats.timeDifs <- NULL;
  stats.fractionAfterVelocityFilter<-NULL;
  t1<-Sys.time()

  out=locs;

  VELOCITY_UPPER_BOUND<-options[["v_filter_max_v"]]
  NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY=options[["min_time_dif_for_stats"]] #5;
  TIME_BOUND=options[["v_filter_time_dif"]];
  X.col<-which(colnames(locs) %in% dataLegend[1])
  Y.col<-which(colnames(locs) %in% dataLegend[2])
  time.col<-which(colnames(locs) %in% dataLegend[3])
  xy=locs[,c(X.col,Y.col)];
  colnames(xy)<-c("X","Y")
  time=locs[,time.col]
  end.ix<-nrow(xy)

  dists=calcDist(xy[2:end.ix,],xy[1:(end.ix-1),])
  timeDifs<-round((time[2:end.ix]-time[1:(end.ix-1)])/1000)


  stats.timeDifs=c(stats.timeDifs,
                       timeDifs[which(timeDifs>NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY)]);
  if (length(dists)==length(timeDifs)){
    velocities<-dists/timeDifs
  } else {
    velocities=0
  }
  if (sum(velocities[which(velocities<0)])>1){
      print('WARNING: time seems to not be in ascending order, some negative velocities found.');
      print(paste0('# such points: ',sum(velocities[which(velocities<0)])));
  }

  reachedEnd<-FALSE;
  reliablePoints<-rep(FALSE,nrow(xy))
  i<-reliable(velocities,timeDifs,options)

  if (is.null(i)){
    print('WARNING: no first reliable point found, check parameters')
    reachedEnd=TRUE
  }else{
    reliablePoints[i]=TRUE
  }

  while (!reachedEnd){
    for (j in c((i+1):nrow(xy))){
      if (j==nrow(xy)){
        reachedEnd=TRUE;
      }

      timeDifference=round(time[j]-time[i])/1000
      if (timeDifference>=TIME_BOUND){
        reliableRslt<-reliable(velocities[j:end.ix],timeDifs[j:end.ix],options)
        if (is.null(reliableRslt)){
          reachedEnd=TRUE
        } else{
          i<-j-1+reliableRslt[1]
          #print(sprintf("next reliable ix: %d", i))
          reliablePoints[i]=TRUE
        }
        break
      } else{

        distance<-calcDist(xy[i,],xy[j,])
        velocity<-distance/timeDifference
        if (velocity<VELOCITY_UPPER_BOUND){
          i<-j
          reliablePoints[i]=TRUE
          break
        }
      }
    }
  }
  stats.fractionAfterVelocityFilter[i]=sum(reliablePoints)/nrow(xy)
  out=out[reliablePoints,];

  t2<-Sys.time()
  print(sprintf("%s filterByVelocity(): calculte doration %.2f sec",
                Sys.time(),
                (as.double(t2-t1, units="secs"))))


  return(out)
}

calcDist<-function(vec2, vec1){
  return(sqrt((vec2$X-vec1$X)^2+(vec2$Y-vec1$Y)^2))
}

reliable<-function(relevantVelocities,
                   relevantTimeDifs,
                   options){
  VELOCITY_UPPER_BOUND<-options[["v_filter_max_v"]]
  NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY=options[["min_time_dif_for_stats"]] #5;
  TIME_BOUND=options[["v_filter_time_dif"]];

  if (length(relevantVelocities)<NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY){
    reliableInd=NULL
  } else{
    inds<-rep(TRUE,length(relevantVelocities))

    end.ix<-length(relevantVelocities)
    for (k in c(1:NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY)){
      velframe<-c(relevantVelocities[k:end.ix],rep(VELOCITY_UPPER_BOUND,k-1))
      timedifFrame<-c(relevantTimeDifs[k:end.ix],rep(TIME_BOUND,k-1))
      inds<-inds & (velframe<VELOCITY_UPPER_BOUND) & (inds<=TIME_BOUND)
    }
    if (length(which(inds))>0){
      reliableInd=which(inds)[1];
    } else{
      reliableInd<-NULL
    }
  }
  return(reliableInd)
}
