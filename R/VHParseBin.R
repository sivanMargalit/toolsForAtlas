#'
#' read_ACC extract ACC data from vildehaye ACC binary file (BMI160)
#'
#'
#' @param fileName binary file name
#'
#' @return list with 2 data frame:
#' \itemize{
#'    \item ACC.data - describe rate and time of ACC burst
#'    \item ACC.burst - summary of ACC measurements of al bursts
#'  \}
#'
#' based on matlab code from ATLAS UserGuide Oct2020
#'
#' @import dplyr
#' @export
read_ACC<-function(fileName){
  f <- file(fileName, "rb")

  sensorId = readBin(con=f, what='double', n=1, endian ='big')

  eof_flag<-FALSE
  ACC.data.df<-c()
  ACC.burst.df<-c()
  cntBrst=0
  while (!eof_flag)  {
    # read time stamp
    t<-readBin(con=f, what='double', n=1, endian ='big')
    if (length(t)==0) {
      # past the end of the file
      eof_flag<-TRUE
      break
    }
    # sampling rate
    r = readBin(con=f,what='double', n=1, endian ='big')

    # read number of measurements
    n.col = readBin(con=f,what='double', n=1, endian ='big')

    # dimension of each measurement
    n.row = readBin(con=f, what='double', n=1, endian ='big')
    if (t < 1.5e9) next   # incorrect time stamp
    cntBrst<-cntBrst+1
    ACC.burst.df<-rbind(ACC.burst.df,
                        data.frame("TIME"=t, "rate"=r, "nRows"=n.col))
    message(sprintf("read_ACC: burst %d: %d x %d", cntBrst,n.row, n.col))
    bmi160burst = readBin(con=f, what='double', n=n.col*n.row, endian ='big')
    bmi160burst_mx<-matrix(bmi160burst,
                      nrow=n.row,
                      ncol=n.col)
    bmi160burst_t = t(bmi160burst_mx) # row order, so transpos
    ACC.data.df<-rbind(ACC.data.df,
                  as.data.frame(bmi160burst_t))
  }
  close(f)
  return(list("ACC.data"=ACC.data.df,
              "ACC.burst"=ACC.burst.df))
}

#'
#' read_BME extract Barometric data from vildehaye sensor file (BME280)
#'
#'
#' @param fileName binary file name
#'
#' @return data frame of TIME and barometric presure (units hPa)
#'
#' based on matlab code from ATLAS UserGuide Oct2020
#'
#' @import dplyr
#' @export
read_BME<-function(fileName){
  f <- file(fileName, "rb")

  sensorId = readBin(con=f,
                     what='double',
                     n=1,
                     endian ='big')
  n.col = readBin(con=f,
                  what='double',
                  n=1,
                  endian ='big')
  n.row = readBin(con=f,
                  what='double',
                  n=1,
                  endian ='big')
  BMEData<- readBin(con=f,
                    what='double',
                    n=n.row*n.col,
                    endian ='big')
  BMEData_mx<-matrix(BMEData,
                    nrow=n.row,
                    ncol=n.col)
  BMEData_t<-t(BMEData_mx)
  BMEData.df<-as.data.frame(BMEData_t)
  names(BMEData.df)<-c("TIME", "Pressure")
  BMEData.df = BMEData.df %>%
    filter(TIME> 1.5e9)%>%                 # filter incorrect times
    filter(between(Pressure, 300, 1100))  # filter incorrect pressure
  close(f)
  return(BMEData.df)

}

#'
#' read_BME extract Battery data from vildehaye sensor file (BAT)
#'
#'
#' @param fileName binary file name
#'
#' @return data frame of TIME, Temperature and power
#'
#' based on matlab code from ATLAS UserGuide Oct2020
#'
#' @import dplyr
#' @export
read_BAT<-function(fileName){
  f <- file(fileName, "rb")

  sensorId = readBin(con=f,
                     what='double',
                     n=1,
                     endian ='big')
  n.col = readBin(con=f,
              what='double',
              n=1,
              endian ='big')
  n.row = readBin(con=f,
              what='double',
              n=1,
              endian ='big')
  BatData<- readBin(con=f,
                    what='double',
                    n=n.row*n.col,
                    endian ='big')
  BatData_1<-matrix(BatData,
                  nrow=n.row,
                  ncol=n.col)
  BatData_t<-t(BatData_1)
  BatData.df<-as.data.frame(BatData_t)
  names(BatData.df)<-c("TIME", "temp", "V")
  BatData.df = BatData.df %>%
    filter(TIME> 1.5e9)%>%                   # filter incorrect times
    filter(between(temp, -40, 85)) %>%     # filter incorerct temerature (celsius)
    filter(between(V, 0, 5))               # filter incorerct voltage
  close(f)
  return(BatData.df)

}


