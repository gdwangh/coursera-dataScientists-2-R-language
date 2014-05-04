complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  curpwd<-getwd()
  setwd(directory)
  nobs<-vector()

  for (idx in id) {
    fname<-paste0(formatC(idx,width=3,flag="0"),".csv")
    data<-read.csv(fname)
    good<-data[complete.cases(data),]
    nobs<-c(nobs,nrow(good))
  }

  setwd(curpwd)
  data.frame(id=id, nobs=nobs)
}