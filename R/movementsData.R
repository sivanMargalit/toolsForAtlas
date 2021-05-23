#TODO:
#  add user interface with swril
#  define dynamic categories: covXYGroup , traceNormGroup

#options(digits=14)

#' add indexes uses for filtering by loclization quality
#'
#' TODO...
#'
#' @param .df raw atlas data.frame
#' @param locAttributs vectore of attibutes per localizations record
#'        * "distanceSpeed" or "distance" or "speed" -  add 3 attibutes: dT, distances and speed</li>
#'        * "angle" - calculte angle between 2 successive path (-180..180 degrees)</li>
#'        * "locQuality" - calculate 2 localization quality attibutes
#'                       ** stdVarXY - based on COVX, VARX, VARY</li>
#'                       ** traceNorm - based on, VARX and VARY</li></li>
#'
#'        default : all attributes
#' @return data.frame with added columns
#'
#' @examples
#' addLocAttribute(df)
#' addLocAttribute(df, c("distanceSpeed", "locQuality"))
#'
#' @export
addLocAttribute<-function(.df,
                          locAttributs=c("distanceSpeed","locQuality")){

  if (length(which(colnames(.df) %in% c("TIME","X","Y")))<3){
    Er <- simpleError(paste("addLocAttribute: missing one of the columns TIME,X,Y\n",
                            "  TIME expected to be 13 length integer for epoch time in millisecond",
                            "  X,Y coordinats in local mercator projection (metric units)"))
    stop(Er)
  }

  # # get summaries of each TAG

  #add dateTime field
  .df$dateTime<-as.POSIXct((.df$TIME/1000),tz="UTC",origin="1970-01-01")
  .df$date<-as.Date(.df$dateTime)

  # add spd value
  .df<-.df%>%arrange(TIME)
  if (length(which(c("speed", "distance", "distanceSpeed") %in% locAttributs))) {
    .df<-addDistanceSpeed(.df)
  }

  if ("locQuality" %in% locAttributs){
    if (length(which(c("VARX", "VARY","COVXY") %in% colnames(.df)))<3){
      warning("addLocAttribute: missing one of the columns VARX, VARY, COVXY which are requires for quality attributes\n")
    } else{

      # add traceNorm values (as quality iindex for the tags localization)
      .df$traceNorm<-sqrt(.df$VARX+.df$VARY)

      # err index by Emmanuels formula
      .df$stdVarXY<-stdevFilt(.df$VARX,.df$VARY,.df$COVXY)
      }
  }


  if ("angle" %in% locAttributs){
    # angle between each 3 localizations
    #.df$angl<-TrackAngles(.df$X,.df$Y)
    # angle between each 3 localizations
    .df <- .df %>% group_by(TAG) %>%
            mutate(angl=(180- abs(TrackAngles(X,Y))))
  }
  # # add ErrVarXY index
  # raw.df$ErrVarXY<-stdevFilt(raw.df$VARX,raw.df$VARY,raw.df$COVXY)


  return(.df)
}

#' mark sections of sequence localizations
#'
#' TODO...
#'
#' @param df localization data frame
#' @param dtThreshold timethreshold (in seconds)
#'
#' @return data.frame with added columns sectionID
#'
#' @examples
#' defineSectionsByTime(df, 600) #10 mintes gap seperate sections
#'
#' @export
defineSectionsByTime<-function(.df, dtThreshold=600){
  tags.lst<-unique(.df$TAG)
  new_df<-NULL

  #df1<-addDistanceCol(df)
  df1<-as.data.frame(.df%>%group_by(TAG)%>%arrange(TIME))
  output.df<-NULL
  for (i in length(tags.lst):1){
    tg<-tags.lst[i]
    tmp.df<-df1%>%filter(TAG==tg)
    bigDistanceIx<-which(tmp.df$dT>dtThreshold)
    tmp.df$sectionID=NA
    if(length(bigDistanceIx)==0){
      tmp.df$sectionID<-1
    } else {
      secID=1
      j1<-1
      for (j in 1:length(bigDistanceIx)){
          j2<-bigDistanceIx[j]
          tmp.df$sectionID[j1:j2]<-secID
          secID<-secID+1
          j1<-j2
      }
      j2<-nrow(tmp.df)
      tmp.df$sectionID[j1:j2]<-secID
    }
    output.df<-rbind(tmp.df,output.df)
  }
  return(output.df)
}
