#'
#' Embedding ground level from DEM raster into atlas data frame
#' @param atlas.df atlas data frame. or other dataframe with columns X, Y as ITM units
#' @param demPath path to DEM raster(s) files. default path is "data/DEM/"
#'
#' @return list with 2 items:
#'         (1) raster_lst - is list of rasters loaded from DEM files
#'         (2) input dataframe with addition column GL, which hold the Ground level above see for each localization
#'
#' details:
#'  <none>
#' @import sp
#' @import raster
#' @export
getGLFromDEM<-function(atlas.df=NULL,
                        demPath="data/DEM/"){

  if(is.null(atlas.df)){
    message("getGLFromDEM: atlas.df should not be null")
    return()
  }

  #--------------------------------------
  # convert coordinates to WGS
  #--------------------------------------
  atlas.spdf<-convertSpatial.ITM2WGS84(atlas.df,
                      xyColNames=c("X","Y"))

  #--------------------------------------
  # read rasters
  #--------------------------------------
  str_names_lst=list.files(demPath)
  imported_raster_lst <-lapply(sprintf("%s/%s",demPath,str_names_lst),  raster)

  #--------------------------------------
  # assign ASL from DEM to each antena
  #--------------------------------------
  atlas.spdf@data$GL<-NA
  for (i in 1:length(imported_raster_lst)){
    imported_raster<-imported_raster_lst[[i]]
    GL <- extract(imported_raster,atlas.spdf)
    atlas.spdf@data$AGL<-ifelse(is.na( atlas.spdf@data$GL), GL,  atlas.spdf@data$GL)
  }

  atlas.df<- atlas.spdf%>%
    as.data.frame()%>%
    dplyr::select(-LON,-LAT)

  return(list("raster_lst"=imported_raster_lst,
              "atlas.df"=atlas.df))
}

