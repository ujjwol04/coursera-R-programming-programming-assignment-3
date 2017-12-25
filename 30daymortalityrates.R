setwd("C:/UJJWOL/Downloads/Other R files/R- Coursera/rprog%2Fdata%2FProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
death.rates <- as.numeric(outcome[,11])
hist(death.rates)

