corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  comp_list<-complete(directory)
  id_list<-comp_list[comp_list["nobs"]>threshold,"id"]
  
  curpwd<-getwd()
  setwd(directory)
  cr_list<-vector()
  
  for (idx in id_list) {
    fname<-paste0(formatC(idx,width=3,flag="0"),".csv")
    data<-read.csv(fname)
    cr<-cor(data[,"sulfate"],data[,"nitrate"], use="complete.obs")
    cr_list<-c(cr_list,cr)
  }
  setwd(curpwd)
  ## Return a numeric vector of correlations
  cr_list
  
}