#' plot histograms for list of parameters - each figure has place for 4 plots
#'
#' @param df atlas data.frame with parameters for histogram
#' @param histList list of field for histogram
#'        default : spd, traceNorm, dT
#' @param groupBy list of categories field to group by
#' @param txt for annotation title [not works yet]
#' @return list of figures
#'
#' @examples
#' showHistograms(df, histList=c("spd","traceNorm","dT"), groupBy=c("TAG"))
#'
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#'
#' @export
showHistograms<-function(df,
                         histList=c("spd","traceNorm","dT"),
                         groupBy=c("TAG"),
                         txt="without filter"){

  groupInPage=4
  figure.lst<-list()
  plots.in.page<-list()
  fig.cnt=1
  plt.cnt=0
  for (i in 1:length(histList)) {
    if (length(groupBy)==0){
      groupBy="blue"
    }
    p<-ggplot(df, aes_string(x=histList[i], color=groupBy)) +
          geom_histogram(fill="white", position="dodge")
    plt.cnt=plt.cnt+1
    plots.in.page[[plt.cnt]]<-p
    if (i%%groupInPage == 0){
      figure <- ggarrange(plotlist=plots.in.page,
                          ncol = 2, nrow = 2,
                          common.legend = TRUE, legend = "bottom")
      (figure)
      annotate_figure((figure),
                      top = text_grob(txt, color="black", face = "bold", size = 14))
      figure.lst[[fig.cnt]]=figure
      fig.cnt=fig.cnt+1
      plt.cnt=0
      plots.in.page<-list()
    }
  }
  if (i%%groupInPage > 0){
    figure <- ggarrange(plotlist=plots.in.page,
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom")
    figure.lst[[fig.cnt]]=figure
    fig.cnt=fig.cnt+1
  }
  return(figure.lst)

}

#' return statistics describe parameter disrtribution
#'
#' @param df atlas data.frame with parameters for histogram
#' @param parameter parameer's field name
#'        default : NULL
#' @param groupBy list of categories field to group by
#'        default : NULL
#' @return data frame of statistic information
#'
#' @examples
#' statistic.desc(df, parameter="spd", groupBy=c("TAG"))
#'
#' @import dplyr
#'
#' @export
statistic.desc<-function(df,
                         parameter=NULL,
                         groupBy=NULL){

  if (is.null(parameter)){
    print(sprintf("statistic.desc(): missing parameter"))
    return (NULL)
  }

  if (!is.null(groupBy) & length(groupBy)>0){
    groupBy=groupBy[which(groupBy %in% colnames(df))]
  } else{
    groupBy=NULL
  }

  if (!is.null(groupBy)){
    desc.df<-df%>%
      group_by_at(groupBy) %>%
      summarise(
        N=n(),
        mean=mean(get(parameter)),
        sd=sd(get(parameter)),
        median=median(get(parameter)),
        quantile95=quantile(get(parameter), c(0.95))
      ) %>%
      mutate( se=sd/sqrt(N))  %>%
      mutate( ic=se * qt((1-0.05)/2 + .5, N-1))
  }else{
    desc.df<-df%>%
      summarise(
        N=n(),
        mean=mean(get(parameter)),
        sd=sd(get(parameter)),
        median=median(get(parameter)),
        quantile95=quantile(get(parameter), c(0.95))
      ) %>%
      mutate( se=sd/sqrt(N))  %>%
      mutate( ic=se * qt((1-0.05)/2 + .5, N-1))
  }
  return(desc.df)
}
