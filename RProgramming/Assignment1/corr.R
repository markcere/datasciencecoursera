corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## 
  
  ##generate path
  path<-paste(directory,"",sep="/")
  
  datalist<-complete(directory)
  datalist<-datalist[datalist$nobs>threshold,]
  
  if(dim(datalist)[1]==0) return(numeric(0))
  
  ##sapply(datalist[datalist$nobs>threshold,], function(x) cor(x$nitrate,x$sulfate))
  ##load data files
  rawdata<-lapply(paste(path, formatC(datalist[,1], 2, flag=0, mode="integer"), ".csv", sep=""), read.csv, header=TRUE,  dec = ".")
  ##combine and bind
  data <- do.call(rbind,rawdata)
  data<-data[!is.na(data[,2])&!is.na(data[,3]),]
  ##spli data by ID
  sdata<-split(data,data$ID)
  ##calculate occurence and id
  mdata<-sapply(sdata, function (x)  cor(x$sulfate, x$nitrate))
  mdata
               
}