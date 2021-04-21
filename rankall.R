rankall<-function(outcome,num="best") {
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  possible_outcome<-(c("heart attack","heart failure","pneumonia")==outcome)
  hospital<-c()
  state<-c()
  rank_all<-data.frame()
  #create state list c(AZ,CA,CO,CT....)
  state_list<-sort(c(unique(data$State)))
  #check if arguments are valid
  if (sum(possible_outcome)!=1){
    stop(print("invalid outcome")) 
    #subset relevant columns
  }else{
    
    for (i in state_list){
    
    state_filter<-data[data$State==i,]
    state_filter[state_filter=="Not Available"]=NA
    state_subset<-data.frame(as.character(state_filter$Hospital.Name),
                             as.character(state_filter$State),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                             as.numeric(state_filter$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    colnames(state_subset)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
    rm(state_filter)
   
    
    #rank and return hospital name
    if(outcome=="heart attack"){
      ranked_outcome<-state_subset[order(state_subset$heart_attack,state_subset$Hospital,na.last=NA),1]
    }else if (outcome=="heart failure"){
      ranked_outcome<-state_subset[order(state_subset$heart_failure,state_subset$Hospital,na.last=NA),1]
    }else if (outcome=="pneumonia"){
      ranked_outcome<-state_subset[order(state_subset$pneumonia,state_subset$Hospital,na.last=NA),1] 
    }else{
      stop(print("Not valid input for outcome."))
    }
    #find out the num-ranked hospital in a state
    if (num=="best"){
      real_outcome<-as.vector(ranked_outcome)[1]
    }else if (num=="worst"){
      real_outcome<-as.vector(ranked_outcome)[length(ranked_outcome)]
    }else if (num<=length(state_subset[!is.na(state_subset)])){
      real_outcome<-as.vector(ranked_outcome)[num]
    }else {
      print(NA)
    }
    #real_outcome
    hospital<-c(hospital,real_outcome)
    state<-c(state,i)
    rank_all<-data.frame(hospital,state)
    }
    rank_all
  }
} 
    