library(tidyr)
library(dplyr)
library(readr)
library(tree)
h.train <- read_csv("Desktop/Projects/Kaggle/House Prices/train.csv")
h.test <- read_csv("Desktop/Projects/Kaggle/House Prices/test.csv")



### check which columns have na values

colnames(h.train) [ apply(h.train, 2, anyNA) ]

### Check the number of NAs per column
sapply(h.train, function(x) sum(is.na(x)))

h.train <- as.data.frame(h.train)

### Give missing Numerical Values the Median value

h.train  <- h.train %>% mutate_at(vars(LotFrontage), ~ifelse(is.na(.), median(., na.rm = TRUE), .))

h.train  <- h.train %>% mutate_at(vars(MasVnrArea), ~ifelse(is.na(.), median(., na.rm = TRUE), .))

h.train  <- h.train %>% mutate_at(vars(GarageYrBlt), ~ifelse(is.na(.), median(., na.rm = TRUE), .))



######


#non.reg.NA <-  ('MasVnrType','Electrical')

#### Delete  observations with missing categorical variables 
h.train <- h.train[complete.cases(h.train$MasVnrType), ]

h.train  <- h.train[complete.cases(h.train$Electrical), ]


##### select the categorical variables 


h.train[is.na(h.train)] <- 'None'


colnames(h.train) [ apply(h.train, 2, anyNA) ]




######### training set 

set.seed(1)
library(randomForest)

names(h.train)[names(h.train) == '1stFlrSF'] <- 'FstFlrSF'
names(h.train)[names(h.train) == '2ndFlrSF'] <- 'SndFlrSF'
names(h.train)[names(h.train) == '3SsnPorch'] <- 'ThirSsnPorch'

h.train <- as.data.frame(h.train)

### make all categorical variables factors
#h.train[sapply(h.train, is.character)] <- lapply(h.train[sapply(h.train, is.character)], as.factor)

h.train.I <- h.train[,-1]
#rf.house=randomForest(SalePrice~.,data=h.train.I,mtry=26,importance=TRUE)


############# deal with test set

### check which columns have na values

colnames(h.test) [ apply(h.test, 2, anyNA) ]

### Check the number of NAs per column
sapply(h.test, function(x) sum(is.na(x)))



### make all na numerical variables into median
h.test[sapply(h.test, is.numeric)] <- lapply(h.test[sapply(h.test, is.numeric)], function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x )})

##### 


h.test$MSZoning[is.na(h.test$MSZoning)] <- "RL"

h.test$Utilities[is.na(h.test$Utilities)] <- "NoSeWa"

h.test$Exterior1st[is.na(h.test$Exterior1st)] <- "VinylSd"

h.test$Exterior2nd[is.na(h.test$Exterior2nd)] <- "Other"


h.test$MasVnrType[is.na(h.test$MasVnrType)] <- "None"

h.test$KitchenQual[is.na(h.test$KitchenQual)] <- "TA"


h.test$Functional[is.na(h.test$Functional)] <- "Typ"

h.test$SaleType[is.na(h.test$SaleType)] <- "WD"

#### change the variables to the names from the training set
names(h.test)[names(h.test) == '1stFlrSF'] <- 'FstFlrSF'
names(h.test)[names(h.test) == '2ndFlrSF'] <- 'SndFlrSF'
names(h.test)[names(h.test) == '3SsnPorch'] <- 'ThirSsnPorch'






##### chane all legit NA observations to None

h.test[is.na(h.test)] <- 'None'


### check which columns have na values

colnames(h.test) [ apply(h.test, 2, anyNA) ]

#remove Id collumn
h.test.I <- h.test[,-1]

### attach both sets together, factorize and then seperate again, then train rf on training set and predict test set
#remove SalePrice

SalePrice <- h.train.I$SalePrice

h.train.I$SalePrice <- NULL

h.set <- bind_rows(h.train.I, h.test.I)

h.set[sapply(h.set, is.character)] <- lapply(h.set[sapply(h.set, is.character)], 
                                                 as.factor)
h.train.I <- slice(h.set,1:1451)

h.test.I <- slice(h.set, 1452:2910)

h.train.I$SalePrice <- SalePrice




###  train rf on training set
rf.house=randomForest(SalePrice~.,data=h.train.I,mtry=26,importance=TRUE)



#### predict test test
yhat.rf = predict(rf.house,newdata=h.test.I)

##### create a multiple linear model 


fit1 <- lm(SalePrice~.,data=h.train.I)

#### predict test set 
yhat.lm = predict(fit1,newdata=h.test.I)


################################################################# Lasso


x=model.matrix(SalePrice~.,h.train.I)[,-1]

y=h.train.I$SalePrice
library(glmnet)


set.seed(1)

train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

grid=10^seq(10,-2,length=100)

lasso.mod=glmnet(x,y, alpha=1,lambda=grid)
plot(lasso.mod)


set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)


bestlam=cv.out$lambda.min
x.test=model.matrix(~.,h.test.I)[,-1]

yhat.las.2=predict(lasso.mod,s=bestlam,newx=x.test)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:260,]
lasso.coef[lasso.coef!=0]


##########
library(xgboost)


x=as.matrix(h.train.I)[,-80]

y=h.train.I$SalePrice

yhat.xgb <- xgboost(data=x, label=y, eta=0.3, max_depth=20,nrounds = 25, subsample = 0.5, eval_metric = "rmse",objective = "reg")






#########################



yhat.mix <- (yhat.rf+yhat.lm+yhat.las)/3

yhat.mix <- round(yhat.mix/500)*500





######## attach prediction to test set

h.test$SalePrice<- yhat.mix


Kaggle.submit <- h.test %>% select("Id", "SalePrice")

write.csv(Kaggle.submit, file = "Desktop/Projects/Kaggle/House Prices/kagglesub6.csv", row.names=FALSE)

