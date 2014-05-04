best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## state should be the 2-character abbreviated name of a state
  if  (nrow(outcomes[outcomes$State==state,])==0) {
    stop("invalid state")
  }
    
  ## outcome should  The outcomes should be one of "heart attack", "heart failure", or "pneumonia"
  if (outcome =="heart attack") 
     x<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  else 
    if  (outcome =="heart failure") 
      x<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
    else 
      if (outcome =="pneumonia") 
        x<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
      else {
        stop("invalid outcome")
      }
   
   outcomes[,x]<-as.numeric(outcomes[,x])
  
   # get rows of state and min outcome 
   min_outcome<-min(outcomes[outcomes$State==state,x],na.rm=TRUE)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # note the difference between & and &&
  min(outcomes[outcomes$State==state & outcomes[x]==min_outcome,"Hospital.Name"],na.rm=TRUE)
}
