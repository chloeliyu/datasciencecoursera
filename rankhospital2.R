rankhospital<-function(state,outcome,num="best"){
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  possible_state<-(unique(data$State)==state)
  possible_outcome<-(c("heart attack","heart failure","pneumonia")==outcome)
  
  #check if arguments are valid
  if (sum(possible_state)!=1){
    stop(print("invalid state"))
  }else if (sum(possible_outcome)!=1){
    stop(print("invalid outcome")) 
    #subset relevant columns
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
    #check if num > number of hospitals in a state
      if (num=="best"){
        real_outcome<-as.vector(ranked_outcome)[1]
      }else if (num=="worst"){
        real_outcome<-as.vector(ranked_outcome)[length(ranked_outcome)]
      }else if (num<=length(state_subset$Hospital)){
        real_outcome<-as.vector(ranked_outcome)[num]
      }else {
        print(NA)
      }
  
    print(real_outcome)
    
    }
  }

