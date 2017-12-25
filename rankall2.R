setwd("C:/UJJWOL/Downloads/Other R files/R- Coursera/rprog%2Fdata%2FProgAssignment3-data")

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankall <- function(outcome="heart attack", num="best") {
  
  allowed.outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  state <- unique(data[,7])
  
  #df <- data.frame(NA,NA)
  
  #checking if outcome is valid and only proceeding if true
  
  if (sum(outcome==allowed.outcomes)==1) {
    
    for (i in 1:length(state)) {
      
    data.new <- data[data$State== state[i], c(2,7,11,17,23)]
    
    if(outcome==allowed.outcomes[1]) {
      rate <- data.new[,c(1,2,3)]
    }
    else if(outcome==allowed.outcomes[2]) {
      rate <- data.new[,c(1,2,4)]
    }
    else if(outcome==allowed.outcomes[3]) {
      rate <- data.new[,c(1,2,5)]
    }
    
    #sorting hospital names alphabetically
    rate <- rate[order(rate[,1]),]
    
    #sorting the rates from lowest to highest
    rate2 <- rate[order(as.numeric(rate[,3])),]
    
    #giving ranks to the hospitals
    rate1 <- cbind(rate2, Rank= c(1:nrow(rate)))
    
    if (num=="best") num=1
    
    else if(num=="worst") num=sum(!is.na(as.numeric(rate[,2])))
    
    #to remove hospital with rates Not Available 
    else if(num>sum(!is.na(as.numeric(rate[,2])))) num=nrow(rate)+1
    
    else num=num
    
    #getting result as per the rank mentioned in num argument. This handles even if character data is inputted in num
    (rate1[as.numeric(num),c(1,2)])
    
    }
    
  }
  
  else {
    stop("Invalid Outcome")
  }
}    
