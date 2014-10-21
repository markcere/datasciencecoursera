complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ##generate path
  path<-paste(directory,"",sep="/")
  
  ##load data files
  rawdata<-lapply(paste(path, formatC(id, 2, flag=0, mode="integer"), ".csv", sep=""), read.csv, header=TRUE,  dec = ".")
  
  ##combine and bind
  data <- do.call(rbind,rawdata)
  
  ##spli data by ID
  sdata<-split(data,data$ID)
  ##calculate occurence and id
  mdata<-t(sapply(sdata, function (x)  c(max(x$ID),nrow(x[!is.na(x[,2])&!is.na(x[,3]),]))
    ))
  colnames(mdata)= c("id","nobs") 
  row.names(mdata) <- NULL
  
  if (id[1]>id[length(id)]) data.frame(mdata[order(mdata[,1],decreasing = TRUE),]) else data.frame(mdata)
 
}
