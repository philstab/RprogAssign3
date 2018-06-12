#dirs and libs
newwd <- file.path(getwd(), "Course2Wk4")
setwd(newwd)

#test args
#state = 'VT'
#outcome = "heart attack"

#determines the best (lowest 30 day mortality) hospital in a given state for an outcome
best <- function(state, outcome) {
     
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

     ## Return hospital name in that state with lowest 30-day death rate for outcome
     
     #sort by outcome value and name
     outDf <- outDf[order(outDf[[outcome]], outDf$Hospital.Name),]
     #subset by state arg  
     stateDf <- (outDf[outDf$State == state,])
     theBest <- stateDf[1,'Hospital.Name']
     print(theBest)
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

