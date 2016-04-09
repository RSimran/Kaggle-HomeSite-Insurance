library(readr)
library(xgboost)
library(extraTrees)
library(lubridate)

set.seed(10)
# read in the files
train <- read_csv("train.csv")
test <- read_csv("test.csv")

#exploratory analysis
cat("Number of rows:", nrow(train), "Number of columns:", ncol(train))
cat("The percentage of conversion is", ((sum(train$QuoteConversion_Flag==1))/nrow(train))*100)

hist(train$QuoteConversion_Flag,main="Target", xlab="Converstions")

#unique number of columns in each predictor
col_count <- sapply(train, function(x) length(unique(x)))
col_countdf <- data.frame(colNames=names(train),colCount=col_count)

#The number of variable with constant values
cat("The number of variable with a contant values is", sum(col_count==1))
cat("Names of the constant values are", names(train)[col_count==1])

#So we can remove those constant columns 
feat<- setdiff(names(train),c("PropertyField6","GeographicField10A"))
train <- train[,feat]

#The two constant columns are PropertyField6 and GeographicField10A
#The number of character and numerical variables
train_num <- train[,names(train)[which(sapply(train, is.numeric))]]
train_char <- train[,names(train)[which(sapply(train, is.character))]]

cat("The number of numerical predictors is", dim(train_num)[2])
cat("The number of character predictors is", dim(train_char)[2])

# Check for NA
cat("The porportion of missing values is", length(train[is.na(train)])/(ncol(train)*nrow(train)))

#Check for duplicate columns
cat("The number of diplicate columns are", nrow(train)-nrow(unique(train)))

# Check out the character feactures
str(lapply(train_char,unique))

#It seems like missing numbers are coded by blanks, replace them by -1
train[is.na(train)] <- -1
test[is.na(test)] <- -1


train$month <- month(train$Original_Quote_Date)
train$day <- day(train$Original_Quote_Date)
train$year <- year(train$Original_Quote_Date)

test$month <- month(test$Original_Quote_Date)
test$day <- day(test$Original_Quote_Date)
test$year <- year(test$Original_Quote_Date)

test$Original_Quote_Date <- NULL
train$Original_Quote_Date <- NULL


for (f in feat) {
 if (class(train[[f]])=="character") {
 slevels <- unique(train[[f]])
 train[[f]] <- as.integer(factor(train[[f]], levels=slevels))
 test[[f]] <- as.integer(factor(test[[f]], levels=slevels))
 }
}


parameter <- list("objective"="binary:logistic", "eval_metric"="auc",
 max_depth=12, subsample=0.8, colsample_bytree=0.8, eta=0.005,min_child_weight = 10)

xgba <- xgboost(params = parameter, data=as.matrix(train[,feat]), label=train$QuoteConversion_Flag,nrounds=1900 ,verbose = 1)
