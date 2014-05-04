
rankall <- function(outcome, num) {
  
  rankState <- function(state_outcome,num) {
      # clean NA in rate
      st_cl<-state_outcome[!is.na(state_outcome[,x]),c(x,"Hospital.Name","State")]
      idx<-do.call(order,st_cl)
            
      if (num == "best") 
        num<-1
      else
        if (num=="worst")
          num<-max(idx)
      else
        if (!is.numeric(num))
          stop("invalid rank")
    
    # there is 2 way 
    # way 1, return hospital, and add the state outside
    # st_cl[idx,][num,"Hospital.Name"]
    
    # way 2, return hospital,state. 
    # Here, because rank n hospital is maybe N/A, so return the state in the 1st row
    c(hospital=st_cl[idx,][num,"Hospital.Name"], state=state_outcome[1,"State"])
  }
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check outcome are valid
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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  st<-split(outcomes[,c(x,"Hospital.Name","State")], outcomes$State)
  
  #  there is 2 ways
  # way 1, return hospital in rankState, and add the state here
  #data.frame(hospital=sapply(st, rankState, num), state=names(st))
  
  # way 2, return hospital,state
  as.data.frame(do.call(rbind,lapply(st, rankState, num)))
}