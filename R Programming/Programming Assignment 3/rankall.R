# Rankall lists all the hospitals for a given outcome and rank
rankall <- function(outcome, num = "best") {
        
        ## Validate outcome values: heart attack, heart failure or pneumonia
        switch(outcome,
               "heart attack" = col<-11,
               "heart failure" = col<-17, 
               "pneumonia" = col<-23 )
        
        ## If no column is identified exit and tell the user
        if (is.numeric(col) == FALSE) stop("invalid outcome")
        
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        df <- data.frame(hospital=character(),state=character(), stringsAsFactors=FALSE) 
        
        ## Get all States 
        states <- unique(data$State)
        
        for (state in states) {    
                
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
                ##if(num > length(n)) stop("NA")
                
                hospital<-study$Hospital.Name[n[rank]]                
                if (is.na(hospital) == TRUE) hospital<-"<NA>"
                
                t<-data.frame('hospital'=hospital,'state'=state)
                df<-rbind(df,t)            
                        
        }
        
        ## Add row names and order by State
        row.names(df)<-df$state
        n<-order(df$state)     
        
        #Return the ordered dataframe
        return(df[n,]) 
        
}
