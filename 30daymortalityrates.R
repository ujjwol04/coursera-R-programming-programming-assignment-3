outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
death.rates <- as.numeric(outcome[,11])
hist(death.rates)

