#test args
#outcome = "heart attack"
#num = '20'


rankall <- function(outcome, num = "best") {
     
     #read data
     oPick <- function(outcome){
     
          fullDf <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
          id <<- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23) 
          suppressWarnings(fullDf[,id] <- as.numeric(unlist(fullDf[,id]))) 
          names(fullDf)[id]<- names(id) #rename outcome cols
          outDf <<- fullDf[, names(fullDf) %in% c( "Hospital.Name", "State", outcome )]
     }
     id<-NULL
     outDf<-NULL
     oPick(outcome)
     
     #arg checks & flags
     if (!(outcome %in% names(id))){ 
          stop("invalid outcome")
     }
    
     if (num == 'worst'){
          i <- numeric()
          wFlag <- TRUE
          }else if (num == 'best') {
          i <- 1
          wFlag <- FALSE
          }else{
          i <- num
          wFlag <- FALSE
     }
     
     #process
     outDf <- outDf[order(outDf[[outcome]], outDf$Hospital.Name),]
     splDf <- split(outDf, outDf$State)
    
     rPick <- function(x, ...){
               rownames(x)<-NULL
               if(wFlag==TRUE){
                    x<-x[complete.cases(x),]
                    pickeDf<-x[nrow(x),]
               }
               else{pickeDf<- x[i,]
               }
               return(pickeDf)
     }
   
     pickList <- lapply(splDf, rPick, i, wFlag)
     pickList <- (do.call(rbind.data.frame, pickList))
     pickList[,1:2]
}     

#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)
