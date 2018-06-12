#The num argument can take values "best", "worst", or an integer indicating the ranking
#(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
#state, then the function should return NA. Hospitals that do not have data on a particular outcome should
#be excluded from the set of hospitals when deciding the rankings.


#test args
state = 'VT'
outcome = "heart attack"
num = 3

#determines the best (lowest 30 day mortality) hospital in a given state for an outcome
rankhospital <- function(state, outcome, num='best') {
     
     ## Read outcome data, set column types, rename columns, subset outcome column
     ## return smaller df with only outcome column
     oPick <- function(outcome){
          
          fullDf <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
          
          #named vector that matches outcome arg and relates to col indicies
          id <<- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23) 
          
          #outcome cols to num, warning suppressed
          suppressWarnings(fullDf[,id] <- as.numeric(unlist(fullDf[,id]))) 
          names(fullDf)[id]<- names(id) #rename outcome cols
          
          outDf <<- fullDf[, names(fullDf) %in% c( "Hospital.Name", "State", outcome )]
          
     }
     
     #declare NULL to bind to best() environment
     id<-NULL
     outDf<-NULL
     oPick(outcome)
     
     ## Check that state and outcome args are valid
     stateslist <- unique(outDf$State) 
     if (!(state %in% stateslist)){
          stop("invalid state")
     }
     if (!(outcome %in% names(id))){ #re-using named id vector from above, assigned to parent env
          stop("invalid outcome")
     }
     
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
     
     #sort by outcome value and alphabetize 
     outDf <- outDf[order(outDf[[outcome]], outDf$Hospital.Name),]
     #subset by state arg  
     stateDf <- (outDf[outDf$State == state,])
     #NA removal and re-index
     stateDf <- stateDf[complete.cases(stateDf),]
     rownames(stateDf) <- NULL
     
     ##rank handling
     #translate 'best' and 'worst' values for num arg into row indexes
     if (num == 'best'){
          i <- 1
     }else if (num == 'worst') {
          i <- nrow(stateDf)
     }else{
          i <- num
     }
     theRank <-stateDf[i,'Hospital.Name']
     print(theRank)
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack")
rankhospital("MN", "heart attack", 5000)




