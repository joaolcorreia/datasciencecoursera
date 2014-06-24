# The best function finds the best hospital in a state given an outcome of heart attack, heart failure or pneumonia
best<- function(state, outcome) {

        # Read the data file
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Validate State
        states<-unique(data[,7])
        if(state %in% states == FALSE) stop("invalid state")
        
        ## Validate outcome values: heart attack, heart failure or pneumonia
        switch(outcome,
               "heart attack" = col<-11,
               "heart failure" = col<-17, 
               "pneumonia" = col<-23 )

        # If no column is identified lets exit and tell the user
        if (is.numeric(col) == FALSE)  stop("invalid outcome")
        
        # Subset state of interest        
        study<-data[data$State==state,]

        # Transform to numeric and order by column of interest. I know NAs are introduced by coercion
        study[,col]<-suppressWarnings(as.numeric(study[,col]))
        n<-order(study[,col], na.last=TRUE)     
        
        # Return Top hospital name
        return(study$Hospital.Name[n[1]])
        
}
