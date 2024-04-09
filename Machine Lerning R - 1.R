# PART 1 - DOWNLOAD ALL THE DATA #
#10 list of stocks - these are the stocks for which we will estimate betas 
asset.names <- c(	 'BA',	 'CMCSA', 'COP', "CSX", "CVX",  "NHI", "PSX", "RTX", "T", "VOO", "WAB", 'WBD', 'MMM', "GOOG", "AZO", "ATO")

#20 specify the dates - this converts "normal" dates into Unix dates used by Yahoo finance
date1=as.numeric(as.POSIXct("2017-01-01", format="%Y-%m-%d"))
date2=as.numeric(as.POSIXct("2024-02-01", format="%Y-%m-%d"))

#30 use lapply to pull stocks in bulk - i.e you can easily expand it to however many stocks you wanted to use. 
all_yahoo_data<-(lapply(asset.names,function(i) 
  read.csv(paste
           ("https://query1.finance.yahoo.com/v7/finance/download/",
             i,
             "?period1=",
             date1,
             "&period2=",
             date2,
             "&interval=1d&events=history&includeAdjustedClose=true",
             sep = ""
           ), 
           header = TRUE)
)      
)

# check how data were downloaded 
str(all_yahoo_data)
# get number of observations into n - will need it in the next step
n=nrow(all_yahoo_data[[1]])
# m will be needed later


#40 compute returns for all stocks, put results into a martix (sapply) and convert it into a dataframe
stock_rets=as.data.frame(sapply(all_yahoo_data, 
                              function(i) 
                                (i$Adj.Close[2:n]/i$Adj.Close[1:(n-1)]-1)   
                              )
                       )

#name the columns (sapply "loses" names)
names(stock_rets)<-asset.names


#45 Side point - quick descriptive stats Returns 
library(stargazer)
stargazer(stock_rets, type="text")


#50 Create a separate vector for the stock we want to predict


Stock_to_predict=stock_rets$BA[2:(n-1)]
Lagged_data=stock_rets[1:(n-2),]

#60 Split data into training and test subsets
# set the random seed
set.seed(1)
# create a vector of random numbers - these will be the observations chosen for the training dataset
# this will sample 80% of the data 
train=sample(n-2,(n-2)*0.8)

# create the actual subsets 
y_train=Stock_to_predict[train]
x_train=Lagged_data[train,]

y_test=Stock_to_predict[-train]
x_test=Lagged_data[-train,]


#70 This is a package that will do search over multiple models 
if (!require(leaps)) install.packages('leaps')
library(leaps)

####################################################################
# side point - computational cost
# different ways to combine numbers 1:5 into sets of 2
combn(5,2)
# number of ways to combine numbers 1:5 into sets of 2 
ncol(combn(5,2))

# for i=i to 20, how many ways are there to have i combinations from a set of integeres 1:20 
sapply(seq(1:20),function(x) ncol(combn(20,x)))
# total number of ways to combine 1:20 into
sum(sapply(seq(1:20),function(x) ncol(combn(20,x))))
# Sum of numbers 1:20
sum(seq(1:20))
####################################################################

#80  full subset search - every combination of 10 predictors 
regfit.full=regsubsets(y_train~.,x_train, nvmax=10, method="exhaustive")
# which predictors are used
summary(regfit.full)
# the best model based on five predictors 
coef(regfit.full,5)

#90 create summaries of all regressions
reg.summary.full=summary(regfit.full)
# what's in that object? 
names(reg.summary.full)
reg.summary.full$which

# 100 plot results 
#create a matrix of graphs 
par(mfrow=c(2,2))

# 110 plot RSS for each model 
plot(reg.summary.full$rss,xlab="Number of Variables",ylab="RSS", type="l")

# 120 plot adjusted R-square for each model, find the best one 
plot(reg.summary.full$adjr2,xlab="Number of Variables",ylab=" Adjusted RSq",type="l")
#which function can be used to identify the location of the maximum point of a vector. Here it's highest Adjusted R^2
max_adjr=which.max(reg.summary.full$adjr2)
# add that point to the graph. 
points(max_adjr,reg.summary.full$adjr2[max_adjr], col ="red",cex =2, pch =20)


# 130 plot Mallow's Cp
plot(reg.summary.full$cp,xlab="Number of Variables",ylab="Cp",    type='l')
min_cp=which.min(reg.summary.full$cp)
points(min_cp,reg.summary.full$cp[min_cp],col="red",cex=2,pch=20)

# 140 plot Bayesian informaiton criterion
min_bic=which.min(reg.summary.full$bic)
plot(reg.summary.full$bic,xlab="Number of Variables",ylab="BIC",     type='l')
points(min_bic,reg.summary.full$bic[min_bic],col="red",cex=2,pch=20)

# 150 Is the model any good? 
# re-estimate the regress for one of the selected models 
# [reg.summary.full$which[3,-1]] pulls the variable names from the best model with 3 variables. -1 is to ignore the interecept
results<-lm(y_train~.,data=x_train[reg.summary.full$which[8,-1]])
results

# compute predicted values for the test set 
y_pred=predict(results, x_test)
# compute the MSE for the difference between predicted and actual values for the test set
mean((y_test-y_pred)^2)
# does this model do better than just a basic average return? 
mean((y_test-mean(y_train))^2)

# Is it at least better than the training datset? 
y_pred=predict(results, x_train)
mean(((y_train-y_pred)^2))
mean(((y_train-mean(y_train))^2))

     