# pollutantmean calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors

pollutantmean <- function(directory, pollutant, id=1:2){

# Initialize the data frame
df<-data.frame(date = character(), sulfate = numeric(), nitrate = numeric(), ID = numeric())

  # Read the monitor files from the specified directory. Hack! Leading zeros
  for (i in id) {
    id_fs<-formatC(i,width=3, flag="0")   
    tmp<-read.csv(paste(directory,"/",as.character(id_fs),".csv", sep=''))
    df<-rbind(df,tmp)      
  }  
  
# Calculate the mean of the pollutant
res<-round(mean(df[,pollutant],na.rm=TRUE),3)

return(res)

# End
}
