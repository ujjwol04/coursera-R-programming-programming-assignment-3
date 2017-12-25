setwd("C:/UJJWOL/Downloads/Other R files/R- Coursera/rprog%2Fdata%2FProgAssignment3-data")

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankhospital <- function(state, outcome, num="best") {
  
  all.states <- data[,7]
  allowed.outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #checking if state and outcome are valid and only proceeding if both true
  
  if (sum(state==unique(all.states))==1 & sum(outcome==allowed.outcomes)==1) {
    
    data.new <- data[data$State== state, c(2,11,17,23)]
    #return(data.new)
    
    if(outcome==allowed.outcomes[1]) {
      rate <- data.new[,c(1,2)]
    }
    else if(outcome==allowed.outcomes[2]) {
      rate <- data.new[,c(1,3)]
    }
    else {
      rate <- data.new[,c(1,4)]
    }
    
    #sorting hospital names alphabetically
    rate <- rate[order(rate[,1]),]
    
    #sorting the rates from lowest to highest
    rate2 <- rate[order(as.numeric(rate[,2])),]
    
    #giving ranks to the hospitals
    rate1 <- cbind(rate2, Rank= c(1:nrow(rate)))
    
    if (num=="best") num=1
    
    else if(num=="worst") num=sum(!is.na(as.numeric(rate[,2])))
    
    #to remove hospital with rates Not Available 
    else if(num>sum(!is.na(as.numeric(rate[,2])))) num=nrow(rate)+1
    
    else num=num
    
    #getting result as per the rank mentioned in num argument. This handles even if character data is inputted in num
    rate1[as.numeric(num),1]
    
  }
  
  else if (sum(state==unique(all.states))==0) {
    stop("Invalid State")
  }
  
  else {
    stop("Invalid Outcome")
  }
}    
