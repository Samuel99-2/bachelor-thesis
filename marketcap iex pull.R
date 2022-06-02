library(httr)
library(tidyverse)
library(tools)
library(dplyr)
library(quanteda)

nasdaq1 <- read.csv("C:/Users/hvida/Documents/finansdata.csv")

tickers1 <- nasdaq1 %>% select(symbol)
tickers1

#secret token used to serve http request 
token <- ("?token=pk_e2856436c01e4b91bc84908c2083cb5f")

#format we wish to recieve out request in, set to csv here 
format <- ("&format=csv")

#specification of what we would like to request, set this up as a for loop that goes through the same info for all stocks   
main <- ("https://cloud.iexapis.com/stable/time-series/financials/AAPL/period=annual")

#Define a loop that goes through all the listed stocks in the us an extacts them as http request


#define main with paste function 
for (i in tickers1){
  bird <- c(i)
  main <- paste("https://cloud.iexapis.com/stable/stock/", bird,"/stats",sep = "")
  res <- paste(main, token, format,sep = "")
  res
}



#Retrieve all data from iex cloud and parse it 
for (i in res){  
  r <- GET(i)
  #  if (file_ext(r)=="No encoding supplied: defaulting to UTF-8."){
  #    next
  #  }
  try(res2 <- content(r, "parsed"))
  #typeof(res2)
  res1 = res2[names(res2) %in% c("CityAreaCode","EntityAddressPostalZipCode","irsNumber","LesseeOperatingLeaseOptionToExtend") == FALSE] 
  #res1 = res2[,!grepl("^City",names(res2))]
  try(if (exists("datamodel") == TRUE){
    datamodel <- bind_rows(res1,datamodel)
  } else if(exists("datamodel") == FALSE){
    datamodel = res1
  }
  )
  
}
print("done")


View(datamodel)

#write.csv(datamodel, "C:/Users/hvida/Documents/basicstats.csv", row.names = TRUE)



