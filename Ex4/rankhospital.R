rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## state should be the 2-character abbreviated name of a state
  if  (nrow(outcomes[outcomes$State==state,])==0) {
    stop("invalid state")
  }
  
  if (outcome =="heart attack") 
    x<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  else 
    if  (outcome =="heart failure") 
      x<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
  else 
    if (outcome =="pneumonia") 
      x<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  else 
    stop("invalid outcome")
  
  outcomes[,x]<-as.numeric(outcomes[,x])
 
  ## order by rate,Hospital.Name
  st<-outcomes[outcomes$State==state & !is.na(outcomes[,x]),c(x,"Hospital.Name")]
  idx<-do.call(order,st)
  
  if (num == "best") 
    num<-1
  else
    if (num=="worst")
      num<-max(idx)
    else
      if (!is.numeric(num))
        stop("invalid rank")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate                   
  st[idx,][num,"Hospital.Name"]
      
}