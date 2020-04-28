Colors.map<-data.frame(hexCode=c("#800080","#DC143C","#0000CD","#2E8B57","#3CB371","#D2691E","#696969","#008B8B"),
                       row.names = c("purple","crimson","mediumblue","lightseagreen","mediumseagreen","chocolate","gray","darkcyan"))


#' create leaflet map for filters
#'
#' create maps with filters layers to select the best filter for you
#'
#' TODO...
#'
#' @param t_dat.spdf spatial point data frame with your data (should contains covXYGroup)
#' @param filters.df data fame define some filters to investigate
#' @param KalmanFilter.spdf dpatial point for kalman smothing (default NULL)
#'
#' @return leaflet map
#'
#' @examples
#' TODO
#'
#' @import leaflet
#' @import dplyr
#'
#' @export
tag.filters.ll.map<-function(t_dat.spdf,
                      filters.df,
                      KalmanFilter.spdf=NULL){

  if (length(grep("covXYGroup", colnames(t_dat.spdf@data)))<1){
    print("tag.filters.ll.map(): missing covXYGroup field. did you run addQualityData() before?");
    return(NULL)
  }
  covXYGroup.lst<-unique(t_dat.spdf@data$covXYGroup)
  factpal.err <- colorFactor(heat.colors(length(covXYGroup.lst)), covXYGroup.lst)

  aTag<-as.numeric(unique(t_dat.spdf@data$TAG))
  llix<-leaflet() %>% addTiles(options=tileOptions(opacity=0.5))
  cntRows<-nrow(t_dat.spdf@data)
  layerName<-sprintf("All localizations [%d rows]",cntRows)
  llix<-llix%>%addCircleMarkers(
    data = t_dat.spdf,  #  points data from spdf
    radius = 3,  # cycle radios
    opacity = 1,  # cycle transparency
    fillOpacity = 0.5,
    color = ~factpal.err(covXYGroup),  # point colour
    stroke = FALSE,
    label = ~TAG,  # id is the TAG
    popup = ~(sprintf("DateTime=%s<br>stdErrXY: %3.4f<br>COVXY: %4.4f<br>norm Trace: %3.4f<br>speed: %3.2f<br>NBS: %d<br>angel: %2.2f",
                      dateTime,stdErrXY,COVXY,traceNorm,spd,NBS,angl)),
    group=layerName)
  llix<-llix%>%addPolylines(
    data=t_dat.spdf@coords,
    color=Colors.map$hexCode[1],
    weight = 0.7,
    dashArray = "1,1",
    opacity = 0.7,
    group="All localizations lines")
  layersList<-c(layerName,"All localizations lines")
  if (!is.null(filters.df)){
    for (i in c(1:nrow(filters.df))){
      layer.data<-t_dat.spdf
      layerName<-""
      colorIx<-(i%%(nrow(Colors.map)-1))+1

      if (!is.na(filters.df$stdErr[i])){
        layer.data<-subset(layer.data,stdErrXY<filters.df$stdErr[i])
        layerName<-sprintf("%sErrVarXY<%d ",layerName,filters.df$stdErr[i])
      }
      if (!is.na(filters.df$COVXY[i])){
        layer.data<-subset(layer.data,abs(COVXY)<filters.df$COVXY[i])
        layerName<-sprintf("%s|COVXY|<%d ",layerName,filters.df$COVXY[i])
      }
      if (!is.na(filters.df$spd[i])){
        layer.data<-subset(layer.data,spd<filters.df$spd[i])
        layerName<-sprintf("%sspd<%d ",layerName,filters.df$spd[i])

      }
      if (!is.na(filters.df$traceNorm[i])){
        layer.data<-subset(layer.data,traceNorm<filters.df$traceNorm[i])
        layerName<-sprintf("%snorm trace<%d ",layerName,filters.df$traceNorm[i])

      }
      layerName<-sprintf("%s [%d rows , %2.1f%%]", layerName, nrow(layer.data), (nrow(layer.data)/cntRows)*100)
      print(sprintf("going to create layer for %s",layerName))
      llix<-llix%>%addPolylines(
        data=layer.data@coords,
        color=Colors.map$hexCode[colorIx],
        weight = 1,
        opacity = 1,
        group = layerName)

      llix<-llix%>%addCircleMarkers(
        data =  layer.data,
        radius = 3,  # cycle radios
        opacity = 0.8,  # cycle transparency
        color = ~factpal.err(covXYGroup),  # point colour
        fillOpacity = 0.5,

        stroke = FALSE,
        label = ~stdErrXY,
        popup = ~(sprintf("DateTime=%s<br>stdErrXY: %3.4f<br>COVXY: %4.4f<br>norm Trace: %3.4f<br>speed: %3.2f<br>NBS: %d",
                          dateTime,stdErrXY,COVXY,traceNorm,spd,NBS)),
        group=layerName)

      layersList<-c(layersList,layerName)

    }
  }

  if (!is.null(KalmanFilter.spdf)){
    # lowStd.Path<-subset(KalmanFilter.spdf, Std<100)
    lowStd.Path<-KalmanFilter.spdf
    contpal <- colorNumeric(
      palette = "Blues",
      domain = lowStd.Path@data$Std)
    llix<-llix%>%addCircleMarkers(
      data =  lowStd.Path,
      radius = 3,  # cycle radios
      fillColor = ~contpal(Std),  # point colour
      fillOpacity = 0.9,
      color = "blue",
      opacity = 1,  # cycle transparency
      weight=0.3,
      stroke = TRUE,
      label = ~sprintf("Time: %s",as.POSIXct((TIME/1000), tz="UTC", origin="1970-01-01")),
      popup = ~sprintf("Time: %s<br>std: %2.2f",as.POSIXct((TIME/1000), tz="UTC", origin="1970-01-01"),
                       Std),
      group="Kalman Filter Path")

    layersList<-c(layersList,"Kalman Filter Path")

    llix%>% addLegend(pal=contpal,
                      title = "Kalman STD",
                      values= lowStd.Path@data$Std,
                      group="Kalman Filter Path")

  }

  # add layers controller
  llix<-llix%>%addLayersControl(
    overlayGroups = layersList,
    options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)
  ) %>%
    # add layers legend
    addLegend(position = "bottomright", labels=covXYGroup.lst,
              title = sprintf("TAG %13.0f<br>COVXY group",aTag),
              colors= factpal.err(covXYGroup.lst)) %>%
    # add scale bar
    addScaleBar(position = c("bottomleft"),
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) %>%
    hideGroup(layersList[3:length(layersList)])


  return(llix)

}
