pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  curpwd<-getwd()
  setwd(directory)
  goodpol<-vector()
  
  for (idx in id) {
    fname<-paste0(formatC(idx,width=3,flag="0"),".csv")
    data<-read.csv(fname)
    goodpol<-c(goodpol,data[!is.na(data[pollutant]),pollutant])
  }
    
  m<-round(mean(goodpol),3)
  setwd(curpwd)
  
  ## return m
  m
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}