setwd("~/GitHub/R-programming-assignment-3")
rankall <- function(outcome, num = "best"){
  options(warn=-1)
  if(num == "best"){
    num <- 1
  }
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])
  df$State <- factor(df$State)
  st <- levels(df$State)
  for(i in 1:length(st)){
    statedat <- df[df$State==st[i],]
    if(outcome == "heart attack"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, state = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
    }
    else{  if(outcome == "heart failure"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, Rate = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
    }    else {
    if(outcome == "pneumonia"){
    here <- data.frame(Hospital.Name = statedat$Hospital.Name, Rate = statedat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stringsAsFactors = FALSE)
    here <- here[order(here[,2], na.last= NA),]
    here <- data.frame(here, rank = 1:length(here[,2]))
    }
    else {stop("invalid outcome")} }}
    if(i==1){
      if(num == "worst"){ 
        use=length(here[,1])        
      }
      else {use=num}
      staterank <- data.frame(hospital = here[use,1], state = st[i], stringsAsFactors = FALSE)
    }
    else {
      if(num == "worst"){ 
        use=length(here[,1])
      }
      else {use=num}
      staterank <- rbind(staterank, data.frame(hospital = here[use,1], state = st[i], stringsAsFactors = FALSE))
    }
  }
  staterank
}