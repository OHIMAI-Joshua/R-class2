#10 Advanced data import options
#Skip rows, change informat, change variable names

TN<- read.table("https://research.stlouisfed.org/fred2/data/TNUR.txt", 
                skip=10, #skip rows
                header=TRUE, 
                colClasses = c("Date",NA) ,#varible types
                col.names = c("Date","Unemp")
                
)

#20 Alternatively, read data first and then tweak it...
TN<- read.table("https://research.stlouisfed.org/fred2/data/TNUR.txt", skip=10, header=TRUE)
TN$DATE<-as.Date(TN$DATE)
months(TN$DATE[1:12])

#25 Side point - dates:
#R date format is number of days since Jan 1 1970
#  https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Dates
head(as.numeric(TN$DATE))


# There is also POSIXct that stores date and time in seconds with the number of seconds beginning at 1 January 1970. 
# Negative numbers are used to store dates prior to 1970. 
# Storing the data this way, optimizes use in data.frames and speeds up computation, processing and conversion to other formats.
# source: https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r

head(as.POSIXct(TN$DATE))
head(as.numeric((as.POSIXct(TN$DATE))))


#30 Merge Data
#Download data from FRED, rename columns and merge by Date
TN<- read.table("https://research.stlouisfed.org/fred2/data/TNUR.txt", 
                skip=10,header=TRUE, colClasses = c("Date",NA), 
                col.names = c("Date","TN_UR"))

KY<- read.table("https://research.stlouisfed.org/fred2/data/KYUR.txt", 
                skip=10,header=TRUE, colClasses = c("Date",NA),
                col.names = c("Date","KY_UR"))

merged <- merge(KY,TN,by="Date")
head(merged)
#change variable names so that names are the same for both variables 


#40 Vertically Combine Data
#Download data from FRED, rename columns and stack vertically. 
#Note: The variable names have to be the same

TN<- read.table("https://research.stlouisfed.org/fred2/data/TNUR.txt", 
                skip=10,header=TRUE, colClasses = c("Date",NA), 
                col.names = c("Date","UR"))
TN$state<-"TN"
head(TN)
KY<- read.table("https://research.stlouisfed.org/fred2/data/KYUR.txt", 
                skip=10,header=TRUE, colClasses = c("Date",NA),
                col.names = c("Date","UR"))
KY$state<-"KY"

stacked <- rbind(KY,TN)  #this is the command to stack the data)




#50 Calculate Summary Stats by Group - by()
# Several options - lapply, sapply, by 
means<-by(stacked$UR, stacked$state,mean) 
means
str(means)


#60 Calculate Summary Stats by Group - lapply()
# lapply() is almost like a loop that repeats the same operation over a list of objects and returns results as a list
# lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X.
step1<-split(stacked$UR,stacked$state)
str(step1)
#OR start with a list in a first place: 
step1a<-list(KY$UR,TN$UR)
step2<-lapply(step1,function(x) mean(x)) #apply mean by list
step2
step2a<-lapply(step1a,function(x) mean(x)) #apply mean by list
step2a
# this is a shorthand for easy-to-define functions
step2c<-lapply(step1,mean)
step2c

#70 Calculate Summary Stats by Group - sapply()
#saplly() loops  over a list of objects and returns results as a  vector

means<-sapply(step1a,function(x) mean(x)) 
str(means)

#75 unlist - 
means2<-unlist(lapply(step1,mean))
str(means2)



#80 "Macro"" to download and stack multiple files in R 
#1 Create state list
statelist<- list("KY","TN","GA","TX")  #create a state list 
data_s<-lapply(statelist,function(x) read.table(
                                    paste("https://research.stlouisfed.org/fred2/data/",x,"UR.txt", sep=""),
                                    skip=10,header=TRUE, colClasses = c("Date",NA), 
                                    col.names = c("Date","UR")
                                                )
              )
str(data_s)
names(data_s)<-statelist
str(data_s)
# what can we do with this list? 
sapply(data_s,function(x) mean(x$UR)) 
lapply(data_s,function(x) coefficients(lm(UR~Date,data=x)))
sapply(data_s,function(x) coefficients(lm(UR~Date,data=x)))




######################## Bulk Betas with Yahoo Data ############################################
#In this example, we will download stock data for multiple stocks from Yahoo finance,         ##
#combine that data with returns on SP500,(which serves a proxy for market returns, and get data#
#for the t-bill, which is a proxy for the risk-free rate. Using those variables, we will      ##
#compute Risk Premia for each stock defined as RP=(ret-rf_rate) and we will do the same for   ##
#the SP500 return. Then we run regression RP_Stock=a+b*RP_SP500 for each stock in the sample  ##
#and collect the results                                                                      ##
################################################################################################

#10 list of stocks - these are the stocks for which we will estimate betas 
asset.names <- c(	 'BA',	 'CMCSA', 'COP', "CSX", "CVX",  "NHI", "PSX", "RTX", "T", "VOO", "WAB", 'WBD')

#20 specify the dates - this converts "normal" dates into Unix dates used by Yahoo finance
date1=as.numeric(as.POSIXct("2017-01-01", format="%Y-%m-%d"))
date2=as.numeric(as.POSIXct("2024-02-01", format="%Y-%m-%d"))

#30 read sp500
sp500<-read.csv(paste("https://query1.finance.yahoo.com/v7/finance/download/^GSPC?period1=",date1,"&period2=",date2,"&interval=1mo&events=history&includeAdjustedClose=true",sep = ""), header = TRUE)   
n=nrow(sp500)

#40 compute SP500 return
SP_return=sp500$Adj.Close[2:n]/sp500$Adj.Close[1:(n-1)]-1

#50 convert date from char into R date
date=as.Date(sp500$Date[2:n])

#60 combine the date and SP_return into a dataframe
SP=data.frame(date, SP_return)

#65 read t-bill from FRED 
tbill = read.table("https://research.stlouisfed.org/fred2/data/TB3MS.txt", 
                   sep="", skip=10,header=TRUE, colClasses = c("Date",NA),            
                   col.names = c("date","rate"))

#67 convert it into monthyl rates 
tbill$rate = tbill$rate/1200 

#68 combine with SP data and compute RP_SP500 - this is the market RP
SP = merge(SP,tbill,by="date")
SP$SP_rp=SP$SP_return-SP$rate


#70 use lapply to pull stocks in bulk - i.e you can easily expand it to however many stocks you wanted to use. 

all_yahoo_data<-(lapply(asset.names,function(i) 
                read.csv(paste
                            ("https://query1.finance.yahoo.com/v7/finance/download/",
                               i,
                               "?period1=",
                               date1,
                               "&period2=",
                               date2,
                               "&interval=1mo&events=history&includeAdjustedClose=true",
                               sep = ""
                            ), 
                          header = TRUE)
                      )      
                )

str(all_yahoo_data)

#80 compute returns for all stocks, put results into a martix (sapply) and convert it into a dataframe
stock_rp=as.data.frame(sapply(all_yahoo_data, function(i) (i$Adj.Close[2:n]/i$Adj.Close[1:(n-1)]-1)- SP$rate   ))

#name the columns
names(stock_rp)<-asset.names

#85 SIde point - quick descriptive stats Returns 
returns=stock_rp+SP$rate # add back risk free- rate to get return from RP
library(stargazer)
stargazer(returns, type="text")


#90 run regressions for all stocks - note, lapply loops over the columns in the dataframe! 
lapply(stock_rp, function(i) summary(lm(i~SP$SP_rp))   )

#100 or put them into results instead
results<-lapply(stock_rp, function(i) (lm(i~SP$SP_rp))   )

#105 this collects SUMMARIES - not same 
results2<-lapply(stock_rp, function(i) summary(lm(i~SP$SP_rp)))   


#110 now  extract coefficients ando other stats 
coeffs=sapply(results, coefficients)
#or 
coeffs=sapply(results,function(x) coefficients(x))
coeffs
# or just betas
coeffs[2,]

#120 can get p-vals too: 
# First take a look at regression output
summary(lm(stock_rp$BA~SP$SP_rp))
# then see what's inside this coeffs2 vector
coeffs2=sapply(results,function(x) coefficients(summary(x)))
coeffs2
# p-value for beta is the eight element
coeffs2[8,]


#130 Can also collect R-squares from summaries 
sapply(results2, function(i) i$r.squared)

