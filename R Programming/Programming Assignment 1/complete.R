# complete read a directoyy full of files and reports the number of completely observed cases 
# in each data file. 

complete <- function(directory, id=1) {
  
  # Initialize the data.frame
  df<-data.frame(id = numeric(), nobs = numeric())
  
  # Read the monitor files from the specified directory
  for (i in id) {
    id_fs<-formatC(i,width=3, flag="0")   
    tmp<-read.csv(paste(directory,"/",as.character(id_fs),".csv", sep=''))
    nrows <- nrow(tmp[complete.cases(tmp),])    
    t<-data.frame('id'=i,'nobs'=nrows)
    
    df<-rbind(df,t)  
    
  }  
  
return(df)
  
# End
}
