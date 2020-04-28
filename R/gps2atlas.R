#' convert from ITM to GWS84 (data frame)
#'
#' convert data.frame with ITM coordinates to spatiall data.frame for WGS84
#'
#'
#' @param df        input data frame
#' @param xyColNames vector with the nameof the X, Y column (by ITM)
#'
#' @return spatial data frame for WGS84
#'
#' @examples
#' TODO
#'
#' @import sp
#'
#' @export
convertSpatial.ITM2WGS84<-function(df, xyColNames=c("X","Y")){

  itm.spdf<-df
  itm.spdf$LON<-df[,xyColNames[1]]
  itm.spdf$LAT<-df[,xyColNames[2]]
  #generate SpatialPointsDataFrame for ITM coordinates
  coordinates(itm.spdf)<-c("LON","LAT")

  #projection string for Israeli New Grid (ITM) format
  # from: http://spatialreference.org/ref/epsg/2039/proj4/
  itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
  proj4string(itm.spdf) <- CRS(itm)

  # CRS = Coordinates Reference Sysytem
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  # spTransform() - function that convert from one CRS to another
  #generate SpatialPointsDataFrame for WGS84 coordinates
  WGS84.spdf <- spTransform(itm.spdf, WGS84)



  return(WGS84.spdf)
}

#' convert data.frame with WGS coordinates to spatiall data.frame for ITM
#'
#'
#' @param wgs84.df        input data frame
#' @param xyColNames      vector with the name of the LON, LOT column (by WGS84). default c("LON","LAT")
#'
#' @return spatial data frame for ITM
#'
#' @examples
#' convertSpatial.WGS842ITM(wgs84.df=gps.df, xyColNames=c("gps.X","gps.Y"))
#'
#' @import rgdal
#' @import sp
#'
#' @export
convertSpatial.WGS842ITM<-function(wgs84.df, xyColNames=c("LON","LAT")){

  wgs84.spdf<-wgs84.df
  #generate SpatialPointsDataFrame for ITM coordinates
  coordinates(wgs84.spdf)<-xyColNames

  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  #projection string for WGS84 format
  # from: TODO
  proj4string(wgs84.spdf) <- CRS("+proj=longlat")

  #projection string for Israeli New Grid (ITM) format
  # from: http://spatialreference.org/ref/epsg/2039/proj4/
  itm<- CRS("+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs")


  # spTransform() - function that convert from one CRS to another
  #generate SpatialPointsDataFrame for WGS84 coordinates
  itm.spdf <- spTransform(wgs84.spdf, itm)

  return(itm.spdf)
}
#---------------------------------------------------------
# same functionality for sf structure

#' convert data.frame with ITM coordinates to sf for WGS84
#' using crs=2039
#'
#' @param df        input data frame
#' @param xyColNames vector with the nameof the X, Y column (by ITM)
#'
#' @return spatial data frame for WGS84
#'
#' @examples
#' TODO
#'
#' @import sf
#' @import rgdal
#'
#' @export
convert.sf.ITM2WGS84<-function(df, xyColNames=c("X","Y")){

  # itm.sf<-df
  # itm.spdf$LON<-df[,xyColNames[1]]
  # itm.spdf$LAT<-df[,xyColNames[2]]

  #generate sf for ITM coordinates (epsg 2039)
  itm.sf <- st_as_sf(df, coords = c(xyColNames[1], xyColNames[2]), crs = 2039)



  # transform to WGS84
  WGS84.sf<-st_transform(x = itm.sf, crs = 4326)

  return(WGS84.sf)
}


#'
#' convert.GPS2atlas.df
#'
#' converting GPS coordinates and time to ATLAS format.
#' Ths function should be call to every GPS device seperatly, since it does not assume identifier column (yet)
#'
#' @param gps.df     input data frame with GPS data
#' @param gps.X      name of X column (default "lon")
#' @param gps.Y      name of Y column (default "lat")
#' @param gps.TIME   name of TIME column (default "time"). expected to be text field
#' @param tryTimeFormats      the time format text to parse  of X column (default "lon")#'
#'
#' @return data frame new data frame only with X, Y, TIME field - adapted to ATLAS table:
#' X, Y are coordinates in ITM marceture
#' TIME is epoch time in ms (14 digits number)
#' Notice the result does not included any identifier of the GPS.
#'
#' @examples
#' TODO
#'
#' @import sf
#'
#' @export
convert.GPS2atlas.df<-function(gps.df = NULL,
                               gps.X="lon",
                               gps.Y="lat",
                               gps.TIME="time",
                               tryTimeFormats = "%d/%m/%Y %H:%M"){
  # check parameters sanity
  #-------------------------------------------------------------------------------
  # check gps.time field
  time.ix<-grep(gps.TIME, colnames(gps.df))
  if (length(time.ix)==0){
    print(sprintf("convert.GPS2atlas.df(): missing time columnn '%s' in colnames of gps.df",
                  gps.TIME))
    return (NULL)
  }

  # check gps.X field
  X.ix<-grep(gps.X, colnames(gps.df))
  if (length(X.ix)==0){
    print(sprintf("convert.GPS2atlas.df(): missing X columnn '%s' in colnames of gps.df",
                  gps.X))
    return (NULL)
  }

  # check gps.Y field
  Y.ix<-grep(gps.Y, colnames(gps.df))
  if (length(Y.ix)==0){
    print(sprintf("convert.GPS2atlas.df(): missing Y columnn '%s' in colnames of gps.df",
                  gps.Y))
    return (NULL)
  }

  # #assuming the names "gps.time", "gps.X", "gps.Y" not used for othe fields in the gps.df
  # origNames<-c("gps.time"=colnames(gps.df)[time.ix],
  #              "gps.X"=colnames(gps.df)[X.ix],
  #              "gps.Y"=colnames(gps.df)[Y.ix])
  colnames(gps.df)[time.ix]="gps.time"
  colnames(gps.df)[X.ix]="gps.X"
  colnames(gps.df)[Y.ix]="gps.Y"


  # create epoch TIME column in ms
  #-------------------------------------------------------------------------------
  gps.df$TIME<-as.numeric(as.POSIXct(gps.df$gps.time,
                                     tryFormats=tryTimeFormats,
                                     tz="UTC",
                                     origin="1970-01-01"))*1000

  # convert X,Y columns for ITM
  # use the function convertSpatial.WGS84 which return a SpatialPointDataFrame structure.
  # Therefore - convert it back to normal data frame
  #-------------------------------------------------------------------------------
  gps.spdf<-convertSpatial.WGS842ITM(wgs84.df=gps.df,
                                     xyColNames=c("gps.X","gps.Y"))
  rslt.gps.df<-data.frame("X"=coordinates(gps.spdf)[,1],
                          "Y"=coordinates(gps.spdf)[,2],
                          "TIME"=gps.spdf@data$TIME)

  # return back the original column names
  #-------------------------------------------------------------------------------
  # colnames(gps.df)[time.ix]=origNames["gps.time"]
  # colnames(gps.df)[X.ix]=origNames["gps.X"]
  # colnames(gps.df)[Y.ix]=origNames["gps.Y"]
  return(rslt.gps.df)
}
