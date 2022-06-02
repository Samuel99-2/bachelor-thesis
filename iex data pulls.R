library(httr)
library(tidyverse)
library(tools)
library(dplyr)
library(quanteda)
library(bizdays)
library(RQuantLib)
library(lubridate)

ferie <- holidayList(calendar="UnitedStates/NYSE", from=("2001-09-01"), to=anydate("2025-09-30"))
print(holidaysANBIMA)

cal <- create.calendar("UnitedStates/NYSE",holidays=holidaysANBIMA, weekdays=c("saturday", "sunday"))
adjust.next("2013-01-05", "UnitedStates/NYSE")


#secret token used to serve http request 
token <- ("token=pk_e2856436c01e4b91bc84908c2083cb5f")

#format we wish to recieve out request in, set to csv here 
format <- ("&format=csv")

#specification of what we would like to request, set this up as a for loop that goes through the same info for all stocks   
main <- ("https://cloud.iexapis.com/stable/time-series/financials/AAPL/period=annual")

#Define a loop that goes through all the listed stocks in the us an extacts them as http request

#get nasdaq listings 
nasdaq1 <- read.csv("C:/Users/hvida/Documents/nasdaq1.csv")
nasdaq1
ticker1 <- nasdaq1 %>% select(Symbol)
test <- as.vector(ticker1)
test2 <- as.character(test)

View(test)
str(test2)

#split the dataframe into smaller dataframes
n <- 4
splits4 <- split(test, factor(sort(rank(row.names(test))%%n)))
View(splits4$`0`)


#test code, no longer relevent but might need it later
t1 = paste(test2, collapse = ",")
str(t1)

tickers <- c("AAPL", "MSFT")
t3 = paste(tickers, sep = ",")
str(t3)
ticker1

t4 = paste(tickers, collapse = ",")
str(t4)

#define the 4 runs 
run1 <- splits4$`0`

run2 <- splits4$`1`

run3 <- splits4$`2`

run4 <- splits4$`3`
run4

#define main with paste function 
for (i in run4){
  bird <- c(i)
  main <- paste("https://cloud.iexapis.com/stable/time-series/financials/", bird,"/annual?from=2016-01-01&to=2021-01-01",sep = "")
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

#write.csv(datamodel, "C:/Users/hvida/Documents/20162021fourth.csv", row.names = TRUE)

#combine the four data sets into one coheret dataset 
dataone <- read.csv("C:/Users/hvida/Documents/20162021first.csv")
datatwo <- read.csv("C:/Users/hvida/Documents/20162021second.csv")
datathree <- read.csv("C:/Users/hvida/Documents/20162021third.csv")
datafour <- read.csv("C:/Users/hvida/Documents/20162021fourth.csv")
nasdaqstocks <- rbind(dataone,datatwo,datathree,datafour)



#get list of tickers and the dates and remove "-" so that is can be used in the for function 
tickers1 <- nasdaqstocks %>% select(symbol,fiscalDate)
format1 <- as.data.frame(tickers1)

format1$fiscalDate <- as.Date.character(format1$fiscalDate)
format1$fiscalDate <- format1$fiscalDate %m+% years(1)
format1$fiscalDate <- adjust.next(format1$fiscalDate, "UnitedStates/NYSE")
format1$fiscalDate <- gsub("-","",as.character(format1$fiscalDate))
format1$fiscalDate

#
#dato <- c(format1$fiscalDate)
#dato
#for (i in dato) {
#  entry <- (i)
#  res5 <- adjust.next(entry,"UnitedStates/NYSE")%>%
#    set_names(c("a"))
#  if (exists("datoer") == TRUE){
#    datoer <- bind_rows(res5,datoer)
#  } else if(exists("datoer") == FALSE){
#    datoer = res5 %>%
#     set_names(c("a"))
#    
#  
#}
#}
datoer

#format1$fiscalDate <-  datoer
format1$fiscalDate <- gsub("-","",as.character(format1$fiscalDate))
format1$fiscalDate <- as.numeric(as.character(format1$fiscalDate))
View(format1)
format1$fiscalDate + 10000
format1$fiscalDate

#Iex doesnt provide historical marketcap, but it does provide historical split adjusted prices
#By multiplying the historical split adjusted price with the current float we can calculate the historical marketcap 
#first we get split adjusted price for the current day

format1
for (i in format1){
  bird <- tolower(c(format1$symbol))
  dato <- format1$fiscalDate
  main <- paste("https://cloud.iexapis.com/stable/stock/", bird,"/chart/date/", dato,"?chartByDay=true",sep = "")
  res4 <- paste(main, token,format,sep = "")
  res4
}


#this for loop retrives all the prices of the day of the data 
for (i in res4){  
  r1 <- GET(i)
  #  if (file_ext(r)=="No encoding supplied: defaulting to UTF-8."){
  #    next
  #  }
  try(res2 <- content(r1, "parsed"))
  res2
  #typeof(res2)
  res1 = as.data.frame(res2)
  #res1 = res2[,!grepl("^City",names(res2))]
  try(if (exists("datamodel12") == TRUE){
    datamodel12 <- bind_rows(res1,datamodel12)
  } else if(exists("datamodel12") == FALSE){
    datamodel12 = res1
  }
)
}
print("done")
View(datamodel12)
work

#some of the data failed to fetch the way around this is by eliminating the data entries from the other if one of them is missing
#first we convert dataformat back
datamodel12 <- read.csv("C:/Users/hvida/Documents/closingprices.csv")

datamodel12$date <- gsub("-","",as.character(datamodel12$date))
datamodel12$date <- as.numeric(datamodel12$date)
datamodel12$data <- datamodel12$date - 10000
View(datamodel12)
stat2 <- rev(datamodel12)
stat3 <- stat2 %>% map_df(rev)
View(stat3)


nasdaqstocks$fiscalDate <- gsub("-","",as.character(nasdaqstocks$fiscalDate))
nasdaqstocks$fiscalDate <- as.numeric(nasdaqstocks$fiscalDate)
nasdaqstocks$fiscalDate


View(nasdaqstocks)
nasdaqcopy <- nasdaqstocks
View(nasdaqcopy)

#there are somehow dubplicates so we delete those
stat6 <- subset(stat3,select = -c(X))
stat6
stat7 <- distinct(stat3)
View(stat7)


stat3copy <- stat3
View(stat3copy)
# afunction that makes sure they are the same length
nasdaqstocks[1,"fiscalDate"]
rowsy <- 1
for (i in nasdaqstocks$symbol) {
  if(nasdaqstocks[rowsy,"key"]!=stat7[rowsy,"symbol"]){
   nasdaqstocks <-nasdaqstocks[-c(rowsy),]
   # rownames(nasdaqstocks) <- 1:nrow(nasdaqstocks)
  } else if(nasdaqstocks[rowsy,"key"]==stat7[rowsy,"symbol"]){
    rowsy <- rowsy+1
  }
}
View(nasdaqstocks)
View(stat3)

#write.csv(stat7, "C:/Users/hvida/Documents/finalprice.csv", row.names = TRUE)
#write.csv(nasdaqstocks, "C:/Users/hvida/Documents/finalfinance.csv", row.names = TRUE)


#nasdaqstocks01 <- nasdaqstocks[-c(5540),] 
View(nasdaqstocks01)
#write.csv(nasdaqstocks01, "C:/Users/hvida/Documents/nasdaqstocks100.csv", row.names = TRUE)
#write.csv(stat7, "C:/Users/hvida/Documents/stat70future.csv", row.names = TRUE)




#a function that adds 10000 to format 1, because of the way its formated adding 10000 is tha same as one year
format1$fiscalDate <- as.numeric(format1$fiscalDate)
format1$fiscalDate <- format1$fiscalDate + 10000  
format1$fiscalDate




#for (i in format1$fiscalDate) {
#  try(row1 <- row1 + 1)
#  if(exist("row1"))== FALSE){
#    row1 = 1
#  }
#
#  entryselecter <- [row1, 2]
#  format1$fiscalDate + 1
#}
#trying


for (i in format1){
  bird <- tolower(c(stat7$symbol))
  main <- paste("https://cloud.iexapis.com/stable/stock/", bird,"/stats?",sep = "")
  res4 <- paste(main, token,format,sep = "")
  res4
}


#this for loop gets the outstanding number of shares
for (i in res4){  
  r1 <- GET(i)
  #  if (file_ext(r)=="No encoding supplied: defaulting to UTF-8."){
  #    next
  #  }
  try(res2 <- content(r1, "parsed"))
  res2
  #typeof(res2)
  res1 = as.data.frame(res2)
  #res1 = res2[,!grepl("^City",names(res2))]
  try(if (exists("datamodel13") == TRUE){
    datamodel13 <- bind_rows(res1,datamodel13)
  } else if(exists("datamodel13") == FALSE){
    datamodel13 = res1
  }
  )
}
View(datamodel13)
#datamodel13 <- read.csv("C:/Users/hvida/Documents/advancedstats.csv")

datamodel14 <- datamodel13[order(nrow(datamodel13):1),] 
View(datamodel14)

rowsy <- 1
for (i in stat7$symbol) {
  if(stat7[rowsy,"symbol"]!=datamodel14[rowsy,"symbol"]){
    stat7 <- stat7[-c(rowsy),]
    # rownames(nasdaqstocks) <- 1:nrow(nasdaqstocks)
  } else if(stat7[rowsy,"symbol"]==datamodel14[rowsy,"symbol"]){
    rowsy <- rowsy+1
  }
}
View(stat7)

#write.csv(datamodel13, "C:/Users/hvida/Documents/advancedstats.csv", row.names = TRUE)

unique(datamodel13$symbol) 
unique(stat7$symbol)

#write.csv(datamodel14, "C:/Users/hvida/Documents/numberofsahres02.csv", row.names = TRUE)
#write.csv(stat7, "C:/Users/hvida/Documents/closingprice02.csv", row.names = TRUE)
#write.csv(nasdaqstocks, "C:/Users/hvida/Documents/advancedfinance02.csv", row.names = TRUE)



#res <- paste(main, token, format,sep = "") 
#res
#http1 <- (res)
#r <- GET(http1)
#res1 <- content(r, "parsed")
#View(res1)


#List of data point to keep,
#Assets: accountsReceivable, assetsCurrentCash, assetsFixed, assetsCurrentOther, inventory,  
#liabilities short term: liabilitiesCurrent
#liabilities long term: liabilitiesNonCurrentDebt, liabilitiesNonCurrentLongTerm
#Sales/exspeness: salesCost, revenue, researchAndDevelopmentExpense, expensesSga
#other: incomeNet 

#newdata<- datamodel %>% select(accountsReceivable, currentCash, assetsFixed, otherCurrentAssets, inventory, 
#liabilitiesCurrent,liabilitiesNonCurrentDebt, liabilitiesNonCurrentLongTerm,salesCost, revenue,
#researchAndDevelopmentExpense, expensesSga, incomeNet)
#View(newdata)
#rm(list = ls())
