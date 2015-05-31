setwd("~/GitHub/R-programming-assignment-3")
best <- function(state, outcome){
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
    here <- statedat[statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(statedat[,11], na.rm = TRUE),2]
    here <- here[!is.na(here)]
  }
  else{  if(outcome == "heart failure"){
    here <- statedat[statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(statedat[,17], na.rm = TRUE),2]
    here <- here[!is.na(here)]
  }   else {
  if(outcome == "pneumonia"){
    here <- statedat[statedat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(statedat[,23], na.rm = TRUE),2]
    here <- here[!is.na(here)]
  } 
  else {stop("invalid outcome")} }}
  sort(here)[1]
}