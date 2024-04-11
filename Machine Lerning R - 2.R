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



#############################################################################
# Ridge Regression                                                          #
#############################################################################

#############################################################################

#10 install packages. glmnet is the library for ridge, lasso and other stuff. 
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)


#20 set up data
x=model.matrix(Stock_to_predict~.,Lagged_data)[,-1]
y=Stock_to_predict


#30 Approach 1 - use all data, estimate ridge for a bunch of lambdas 
# create a grid of a bunch of lambdas and run ridge regressions on them 
# pick a couple of lambdas to see if any of them do better than OLS. 
grid=10^seq(10,-2,length=100)
head(grid)
tail(grid)

#40 run the ridge regressions for the grid and stuff the results into ridge.mod
#alpha=0 runs a ridge regression, alpha=1 runs a lasso
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

#50 check to see how many results we have and what they look like 
dim(coef(ridge.mod))

#60 what lambda was used for regression #50 and what results where produced 
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#l-2 norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#70 same for regression number 80
ridge.mod$lambda[80]
coef(ridge.mod)[,80]
sqrt(sum(coef(ridge.mod)[-1,80]^2))

#80 Lambda plot vs. coefficients 
plot(ridge.mod, xvar = "lambda", label = TRUE)


#90 predict method is available for ridge regression 
predict(ridge.mod,s=50,type="coefficients")[1:17,]
#why s and not lambda? In case later we want to allow one to specify the model size in other ways.) 


#100 Approach 2 split data into training and validation, 
# see if we can find a lambda that does better than OLS
# to see if it does better, compute validation MSE for OLS and Ridge

# the above example - whole data - now let's cross-validate
# use a validation dataset - single split, no folds 
set.seed(1)
train=sample(1:nrow(x), nrow(x)*0.8)
test=(-train)
y.test=y[test]

#110  compare validation MSE with a simple model with just a constant
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=2,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

#120 compare ridge with OLS
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)

# lamda=2 is good. Can we do better? Cross-validate  to find out!

#130  Approach 3 - Use k-fold CV to find the best Lambda 
#glmnet makes it easy - this is the code for 10-fold CV. can change with nfolds option
# to be clear: we use only the training set, split it in ten folds, 
# for each fold we do the ridge regression (e.g. use 1-9 as training and 10 as test) and compute the MSE 
# for the fold, average the MSE across folds and produce the average of MSE across the folds - that's
# what's in the graph. 
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

#140 this is the best lambda - it picks the smallest MSE in test data 
bestlam=cv.out$lambda.min
bestlam

#150 predictions using the best lambda 
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

