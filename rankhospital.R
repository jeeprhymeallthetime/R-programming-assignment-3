setwd("~/GitHub/R-programming-assignment-3")
rankhospital <- function(state, outcome, num = "best"){
  options(warn=-1)
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])
  statedat <- df[df$State==state,]
  if(length(statedat[,1])==0){
    stop("invalid state")
  }
  if(outcome == "heart attack"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, Rate = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
  }
  else{  if(outcome == "heart failure"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, Rate = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
  }   else {
  if(outcome == "pneumonia"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, Rate = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
  } 
  else {stop("invalid outcome")} }}
  
  if(num == "best"){
    here <- here[1,1]
  }
  else {  if(num == "worst"){
    here <- here[length(here[,1]),1]
  } 
  else {  if(num > length(here[,1])){return(NA)}
  else {          
    here <- here[num,]
  }}}    
  here
}