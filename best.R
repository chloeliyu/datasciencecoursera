best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  possible_state<-(unique(data$State)==state)
  possible_outcome<-(c("heart attack","heart failure","pneumonia")==outcome)
  
  if (sum(possible_state)!=1){
    stop(print("invalid state"))
  }else if (sum(possible_outcome)!=1){
    stop(print("invalid outcome"))
  }else{
    state_filter<-data[data$State==state,]
    state_filter[state_filter=="Not Available"]=NA
    state_subset<-data.frame(as.character(state_filter$Hospital.Name),
                             as.character(state_filter$State),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    colnames(state_subset)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
    rm(state_filter)
    
    if(outcome=="heart attack"){
      min_outcome<-min(state_subset$heart_attack,na.rm=TRUE)
      row<-which(state_subset$State==state
                 &state_subset$heart_attack==min_outcome
                 &complete.cases(state_subset$heart_attack)==T)
      col<-which(colnames(state_subset)=="Hospital")
      best_hospital<-sort(as.vector(state_subset[row,col]))
    }
    else if (outcome=="heart failure"){
        min_outcome<-min(state_subset$heart_failure,na.rm=TRUE)
        row<-which(state_subset$State==state
                   &state_subset$heart_failure==min_outcome
                   &complete.cases(state_subset$heart_failure)==T)
        col<-which(colnames(state_subset)=="Hospital")
        best_hospital<-sort(as.vector(state_subset[row,col]))
      }else if (outcome=="pneumonia"){
          min_outcome<-min(state_subset$pneumonia,na.rm=TRUE)
          row<-which(state_subset$State==state
                     &state_subset$pneumonia==min_outcome
                     &complete.cases(state_subset$pneumonia)==T)
          col<-which(colnames(state_subset)=="Hospital")
          best_hospital<-sort(as.vector(state_subset[row,col]))
        } else {
          stop(print("Not valid input for outcome."))
        }
        print(best_hospital[1])
        }
  
}