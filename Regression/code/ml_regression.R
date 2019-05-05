# ML for continuous target variable

# 1. Tree Based Models
# 2. Regression
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/load_libraries.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')

whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_house_ready.csv'),
                        test_proportion = 0.1,
                        seed = 872367823)


str(whole_data)

whole_data<-lapply(whole_data, function(x){
  return(x[, which(sapply(x, is.integer)):=lapply(.SD, as.numeric), .SDcols=sapply(x,is.integer)])
})

str(whole_data)


# we start defining a formula
formula<-as.formula(price~.)   # price against all other variables



#### 1.1 Base R Partitioning Tree 
library(rpart)
library(rpart.plot)
tree_0<-rpart(formula = formula, data = whole_data$train, method = 'anova', model=TRUE)

print(tree_0)
summary(tree_0)

rpart.plot(tree_0, digits = 4,type = 2,box.palette = 'Gn')

test_tree<-predict(tree_0, newdata = whole_data$test,type = 'vector')

df_pred<-whole_data$test[, .(id=1:.N,price, test_tree)]
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Regression Tree - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red'))


rmse_tree<-rmse(real=whole_data$test$price, predicted = test_tree)
mae_tree<-mae(real=whole_data$test$price, predicted = test_tree)
mape_tree<-mape(real=whole_data$test$price, predicted = test_tree)
mape_tree


#### 1.2 Random Forest
library(randomForest)

rf_0<-randomForest(formula=formula, data=whole_data$train)
print(rf_0)

test_rf<-predict(rf_0, newdata = whole_data$test, type='response')

df_pred<-cbind(df_pred, test_rf)
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Random Forest - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red','blue'))


rmse_rf<-rmse(real=whole_data$test$price, predicted = test_rf)
mae_rf<-mae(real=whole_data$test$price, predicted = test_rf)
mape_rf<-mape(real=whole_data$test$price, predicted = test_rf)
mape_rf

#### 1.3 Boosting Tree
library(xgboost)

# for this algorithm we need to convert data to a matrix first

xgb_0<-xgboost(booster='gbtree',
               data=as.matrix(whole_data$train[, !'price', with=F]),
               label=whole_data$train$price,
               nrounds = 50,
               objective='reg:linear')
print(xgb_0)

test_xgb<-predict(xgb_0, newdata = as.matrix(whole_data$test[, !'price', with=F]), type='response')

df_pred<-cbind(df_pred, test_xgb)
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Boosted Tree - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red','blue','forestgreen'))


rmse_xgb<-rmse(real=whole_data$test$price, predicted = test_xgb)
mae_xgb<-mae(real=whole_data$test$price, predicted = test_xgb)
mape_xgb<-mape(real=whole_data$test$price, predicted = test_xgb)
mape_xgb



#### 2.1 Regression with StepWise feature selection 
library(MASS)

lm_0<-stepAIC(lm(formula = formula, 
                 data=whole_data$train),
              trace=F)

summary(lm_0)

summary(stepAIC(lm(formula = formula, 
                   data=data.frame(scale(whole_data$train))),
                trace=F))


test_lm<-predict(lm_0, newdata = whole_data$test)

df_pred<-cbind(df_pred, test_lm)
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Linear Regression - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red','blue','forestgreen','orange'))


rmse_lm<-rmse(real=whole_data$test$price, predicted = test_lm)
mae_lm<-mae(real=whole_data$test$price, predicted = test_lm)
mape_lm<-mape(real=whole_data$test$price, predicted = test_lm)
mape_lm

#### 2.2 Regression with regularization
library(glmnet)

glmnet_cv<-cv.glmnet(x = data.matrix(whole_data$train[, !'price']),
                     nfolds = 5,
                     y = whole_data$train[['price']],
                     alpha=1,
                     family = 'gaussian',
                     standardize = T)
plot.cv.glmnet(glmnet_cv)

glmnet_cv$lambda.min

glmnet_0<-glmnet(x = data.matrix(whole_data$train[, !'price']), 
                 y = whole_data$train[['price']],
                 family = 'gaussian',
                 alpha=1, lambda = glmnet_cv$lambda.min)

glmnet_0

print(glmnet_0)
glmnet_0$beta

test_glmnet<-predict(glmnet_0, newx = as.matrix(whole_data$test[, !'price']))
test_glmnet

df_pred<-cbind(df_pred, test_glmnet=test_glmnet[,1])
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Lasso Regression - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red','blue','forestgreen','orange','gray'))


rmse_glmnet<-rmse(real=whole_data$test$price, predicted = test_glmnet)
mae_glmnet<-mae(real=whole_data$test$price, predicted = test_glmnet)
mape_glmnet<-mape(real=whole_data$test$price, predicted = test_glmnet)
mape_glmnet


#### 2.3 Boosting Regression
library(xgboost)

xgb_reg_0<-xgboost(booster='gblinear',
                   data=as.matrix(whole_data$train[, !'price', with=F]),
                   label=whole_data$train$price,
                   nrounds = 100,
                   objective='reg:linear')
print(xgb_reg_0)

test_xgb_reg<-predict(xgb_reg_0, newdata = as.matrix(whole_data$test[, !'price', with=F]), type='response')

df_pred<-cbind(df_pred, test_xgb_reg)
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Boosted Regression - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red','blue','forestgreen','orange','gray','palegreen'))


rmse_xgb_reg<-rmse(real=whole_data$test$price, predicted = test_xgb_reg)
mae_xgb_reg<-mae(real=whole_data$test$price, predicted = test_xgb_reg)
mape_xgb_reg<-mape(real=whole_data$test$price, predicted = test_xgb_reg)
mape_xgb_reg




# model evaluation

result<-data.table(method=c('tree','rf','xgb','lm','glmnet','xgb_reg'),
                   rmse=sapply(df_pred[,!c('price','id')],function(x) return(rmse(real=df_pred$price, predicted=x))),
                   mae=sapply(df_pred[,!c('price','id')],function(x) return(mae(real=df_pred$price, predicted=x))),
                   mape=sapply(df_pred[,!c('price','id')],function(x) return(mape(real=df_pred$price, predicted=x))))


result


result[which.min(result$mape)]

# plotting results metrics

ggplot(result, aes(x=method, y=mape))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=rmse))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=mae))+geom_bar(stat='identity')
