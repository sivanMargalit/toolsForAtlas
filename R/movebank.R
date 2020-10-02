
#' creates atlas queries by movebank referenc data (metaData)
#'
#'
#' @param refData data frame loaded from movebank reference
#' @param queryArguments arguments for atlas queries
#' refData (mandatory) : reference data frame downloded from movebank
#' queryArguments (mandatory) : vector contains your specific setup: 2.1. input.db.str - which input database you use 2.2. output.pattern - pattern for output 2.3. atlasPath - path to your atlas-distribution folder 2.4. atlasSystem - your atlas system name (if it not defined than it set default system “hula”)
#' grouping.df (optional) - data frame defines group of tags per output file. If it ommitted, than tags will be group by deployment on and deployment off time
#' defaultEndTime (optional) - end time for deployments without off time information. The default is current time
#' @param grouping.df optional data frame which define output files and tags
#'       It should be a data frame with two column:
#'       * AtlasTAG - is TAG number as it saved in movebank
#'       * outputFile - output filne name (without the suffix)
#'       (default NULL)
#' @param defaultEndTime optional summply default tinal date Time for case referene data does not contains end time
#'    default is NULL. when NUL  set thisTime as "to" time
#'
#' @return The function returns a list of atlas queries
#'
#' @import tidyverse
#' @export
atlasQueries.by.movebank<-function(refData,
                                   queryArguments=NULL,
                                   grouping.df=NULL,
                                   defaultEndTime=Sys.time()){
  # validate the input data
  if (is.null(queryArguments)){
    print(sprintf("gen.Grouped.Atlas.Queries(): missing input queryArguments"))
    return(NULL)
  }
  input.db.str<-queryArguments["input.db.str"]
  output.pattern<-queryArguments["output.pattern"]
  atlasPath<-queryArguments["atlasPath"]
  atlasSystem<-queryArguments["atlasSystem"]
  if (is.na(input.db.str)){
    print(sprintf("gen.Grouped.Atlas.Queries(): missing input database"))
    return(NULL)
  }
  if(is.na(output.pattern)){
    print(sprintf("gen.Grouped.Atlas.Queries():missing output.pattern"))
    return(NULL)
  }
  if(is.na(atlasPath)){
    print(sprintf("gen.Grouped.Atlas.Queries():missing atlasPath"))
    return(NULL)
  }
  if(is.na(atlasSystem)){
    print(sprintf("gen.Atlas.Query():missing atlasSystem, set default = hula"))
    atlasSystem<-"hula"
  }


  # set system variable that control the month name format : English names
  Sys.setlocale("LC_TIME","C")  # change local date to english names

  # set default time for data without deploy off information
  defaultEndTime.str<-as.character(defaultEndTime, format="%Y-%b-%d-%H-%M-%S")

  # define fromqto values for atlas input format
  refData<-refData%>%
    mutate("fromDateTime"=as.POSIXct(deploy_on_timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"),
           "toDateTime"=as.POSIXct(deploy_off_timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))%>%
    mutate("fromDateTime"=as.character(fromDateTime,format="%Y-%b-%d-%H-%M-%S"),
           "toDateTime"=ifelse(is.na(toDateTime),
                               defaultEndTime.str,
                               as.character(toDateTime,format="%Y-%b-%d-%H-%M-%S")))
  if(is.null(grouping.df)){
    print(sprintf("gen.Grouped.Atlas.Queries():missing grouping.df"))


    tag.groups<-refData%>%
      group_by(fromDateTime, toDateTime)%>%
      summarise("tags"=paste(AtlasTAG, collapse = ","))

    tag.groups$outputFile<-gsub("^([0-9]+)(,?.*)","\\1",tag.groups$tags)
  } else{
    grouped.refData<-merge(x=refData,
                           y=grouping.df,
                           by="AtlasTAG",
                           all=FALSE)

    tag.groups<-grouped.refData%>%
      group_by(fromDateTime, toDateTime, outputFile)%>%
      summarise("tags"=paste(AtlasTAG, collapse = ","))
  }


  queriesList<-list()
  for (i in 1:nrow(tag.groups)){

    firstTagName<-tag.groups$outputFile[i]
    output.str<-sprintf(output.pattern,firstTagName)
    querystr<-sprintf("%s\\atlas.bat query system=%s from=%s to=%s tags=%s input=%s output=%s what=l",
                      atlasPath,
                      atlasSystem,
                      tag.groups$fromDateTime[i],
                      tag.groups$toDateTime[i],
                      tag.groups$tags[i],
                      input.db.str,
                      output.str)
    queriesList[[i]]<-querystr
  }

  return(queriesList)

}



