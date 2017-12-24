setwd("C:/UJJWOL/Downloads/Other R files/R- Coursera/rprog%2Fdata%2FProgAssignment3-data")

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

best <- function(state,outcome) {
        
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
    
          #Ordering the new dataframe and selecting the 1,1 data to get hospital with smallest death rate
          
          rate[order(as.numeric(rate[,2])),][1,1]
          
        }
        
        else if (sum(state==unique(all.states))==0) {
          stop("Invalid State")
        }
        
        else {
          stop("Invalid Outcome")
        }
}    
