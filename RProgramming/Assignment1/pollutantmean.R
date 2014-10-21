pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ##generate path
  path<-paste(directory,"",sep="/")
  
  ##load data files
  rawdata<-lapply(paste(path, formatC(id, 2, flag=0), ".csv", sep=""), read.csv, header=TRUE,  dec = ".")
  
  ##combine and bind
  data <- call.do(rbind,rawdata)

  ##clean and calculate mean
  mean(data[!is.na(data[,pollutant]),pollutant])
}