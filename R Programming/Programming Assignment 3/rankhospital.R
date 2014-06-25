## Rank Hospital by State
rankhospital <- function(state, outcome, num = "best") {

        ## Validate outcome values: heart attack, heart failure or pneumonia
        switch(outcome,
               "heart attack" = col<-11,
               "heart failure" = col<-17, 
               "pneumonia" = col<-23 )
        
        ## If no column is identified lets exit and tell the user
        if (is.numeric(col) == FALSE)  stop("invalid outcome")
        
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state is valid
        states<-unique(data[,7])
        if(state %in% states == FALSE) stop("invalid state")   
             
        ## Subset state of interest        
        study<-subset(data,data$State==state)
        
        ## Transform to numeric. NAs are introduced by coercion
        study[,col]<-suppressWarnings(as.numeric(study[,col]))
        
        ## Remove NA's
        nas<-is.na(study[,col])        
        study<-study[!nas,]
        
        # Order the column of study
        n<-order(study[,col],study$Hospital.Name)     
        
        ## Rank for use of words best and worst
        rank<-num
        switch(num,
               "best" = rank<-1,
               "worst" = rank<-length(n))        
        
        ## Return Top hospital name
        if(num > length(n)) stop("NA")
        
        return(study$Hospital.Name[n[rank]])
        
}
