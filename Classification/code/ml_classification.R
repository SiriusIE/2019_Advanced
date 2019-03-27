# ML for continuous target variable

# 1. Tree Based Models
# 2. Regression
# 3. Neural Networks
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/carga_librerias.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
# source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')

whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/data_bank_ready.csv'),
                        test_proportion = 0.2,
                        seed = 872367823)


str(whole_data)


# binary variables are read as integer, not necessary, but we'll turn them to numeric 
whole_data<-lapply(whole_data, function(x){
  return(x[, names(x)[sapply(x,is.integer)]:=lapply(.SD, as.numeric), .SDcols=sapply(x,is.integer)])
})

str(whole_data)


# we start defining a formula that will serve us for most methods.
formula<-as.formula(factor(y_yes==1)~.)   # price against all other variables


#### 1.1 Base R Partitioning Tree 
library(rpart)
library(rpart.plot)

ini<-now()
tree_0<-rpart(formula = formula, data = whole_data$train, method = 'class', model = TRUE)
print(now()-ini)

print(tree_0)
summary(tree_0)

rpart.plot(tree_0, digits = 4,type = 2,box.palette = 'Gn')

test_tree<-predict(tree_0, newdata = whole_data$test,type = 'class')

df_pred<-whole_data$test[, .(id=1:.N,output=factor(y_yes==1), test_tree)]
str(df_pred)

table(predicted=df_pred$test_tree, real=df_pred$output)

library(pROC)

roc_tree<-roc(response=factor(whole_data$train$y_yes==1),
              predictor=predict(tree_0, data = whole_data$train,type = 'prob')[, 'TRUE']); grid()

roc_tree<-roc(response=df_pred$output,
              predictor=predict(tree_0, newdata = whole_data$test,type = 'prob')[, 'TRUE']); grid()


plot.roc(roc_tree)

library(caret)
confusionMatrix(data = df_pred$test_tree, reference=df_pred$output,positive='TRUE')


#### 1.2 Random Forest
# library(randomForest)
library(ranger)

ini<-now()
rf_0<-ranger(formula=formula, data=whole_data$train,num.trees = 500, probability = T)
print(now()-ini)
print(rf_0)

test_rf<-predict(rf_0, whole_data$test)$predictions[, 'TRUE']>0.5
test_rf

df_pred<-cbind(df_pred, test_rf)
str(df_pred)

table(df_pred[,c('output','test_tree')])
table(df_pred[,c('output','test_rf')])

roc_rf<-roc(response=factor(whole_data$train$y_yes==1),
            predictor=predict(rf_0, data = whole_data$train)$predictions[, 'TRUE']); grid()
roc_rf<-roc(response=df_pred$output,
            predictor=predict(rf_0, data = whole_data$test)$predictions[, 'TRUE']); grid()


plot.roc(roc_rf)
roc_rf$auc


#### 1.3 Boosting Tree
library(xgboost)

# for this algorithm we need to convert to a matrix first

ini<-now()
xgb_0<-xgboost(booster='gbtree',
               data=as.matrix(whole_data$train[, !'y_yes', with=F]),
               label=whole_data$train$y_yes==1,
               nrounds = 50,
               objective='binary:logistic')
print(now()-ini)
print(xgb_0)

test_xgb<-factor(predict(xgb_0, newdata = as.matrix(whole_data$test[, !'y_yes', with=F]))>0.5)

df_pred<-cbind(df_pred, test_xgb)
str(df_pred)

table(df_pred[,c('output','test_tree')])
table(df_pred[,c('output','test_rf')])
table(df_pred[,c('output','test_xgb')])

roc_xgb<-roc(response=factor(whole_data$train$y_yes==1),
             predictor=predict(xgb_0, newdata = as.matrix(whole_data$train[, !'y_yes', with=F]))); grid()

roc_xgb<-roc(response=df_pred$output,
             predictor=predict(xgb_0, newdata = as.matrix(whole_data$test[, !'y_yes', with=F]))); grid()


plot.roc(roc_xgb)


#### 2.1 Logistic Regression

ini<-now()
glm_0<-glm(formula = formula, 
           data=whole_data$train,
           family=binomial)
print(now()-ini)

summary(glm_0)


test_glm<-factor(predict(glm_0, newdata = whole_data$test, type='response')>0.5)

df_pred<-cbind(df_pred, test_glm)
str(df_pred)

table(df_pred[,c('output','test_glm')])

roc_glm<-roc(response=factor(whole_data$train$y_yes==1),
             predictor=predict(glm_0, newdata = whole_data$train))

roc_glm<-roc(response=df_pred$output,
             predictor=predict(glm_0, newdata = whole_data$test))


plot.roc(roc_glm); grid()

#### 2.2 Regression with regularization
library(glmnet)

glmnet_0<-cv.glmnet(x = data.matrix(whole_data$train[, !'y_yes']), 
                    y = factor(whole_data$train[['y_yes']]==1),
                    family = 'binomial',
                    alpha=1)
glmnet_0<-glmnet(x = data.matrix(whole_data$train[, !'y_yes']), 
                 y = factor(whole_data$train[['y_yes']]==1),
                 family = 'binomial',
                 alpha=1, lambda = glmnet_0$lambda.min)

glmnet_0



test_glmnet<-factor(predict(glmnet_0, newx = as.matrix(whole_data$test[, !'y_yes']), type='response')>0.5)

df_pred<-cbind(df_pred, test_glmnet=test_glmnet)
str(df_pred)

table(df_pred[,c('output','test_glmnet')])


roc_glmnet<-roc(response=factor(whole_data$train$y_yes==1),
                predictor=predict(glmnet_0, newx = as.matrix(whole_data$train[, !'y_yes']), type='response')[, 1],
                plot=T); grid()

roc_glmnet<-roc(response=df_pred$output,
                predictor=predict(glmnet_0, newx = as.matrix(whole_data$test[, !'y_yes']), type='response')[, 1],
              plot=T); grid()


#### 2.3 Boosting Logistic Regression
library(xgboost)

# for this algorithm we need to convert to a matrix first
# 

xgb_reg_0<-xgboost(booster='gblinear',
                   data=as.matrix(whole_data$train[, !'y_yes', with=F]),
                   label=whole_data$train[['y_yes']]==1,
                   nrounds = 50,
                   objective='binary:logistic')
print(xgb_reg_0)

test_xgb_reg<-factor(predict(xgb_reg_0, newdata = as.matrix(whole_data$test[, !'y_yes', with=F]))>0.5)

df_pred<-cbind(df_pred, test_xgb_reg)
str(df_pred)


roc_xgb_reg<-roc(response=factor(whole_data$train$y_yes==1),
                predictor=predict(xgb_reg_0, newdata = as.matrix(whole_data$train[, !'y_yes'])),
                plot=T); grid()

roc_xgb_reg<-roc(response=df_pred$output,
                predictor=predict(xgb_reg_0, newdata = as.matrix(whole_data$test[, !'y_yes'])),
                plot=T); grid()


#### 3. Neural Networks
library(nnet)  # simple feed-foward neural network

nnet_0<-nnet(formula = formula, 
             data=whole_data$train,
             size=4,skip=F,
             linout = FALSE)


print(nnet_0)
summary(nnet_0)

test_nnet<-factor(predict(nnet_0, newdata = whole_data$test)>0.5)

df_pred<-cbind(df_pred, test_nnet=test_nnet)
str(df_pred)


roc_nnet<-roc(response=factor(whole_data$train$y_yes==1),
                predictor=predict(nnet_0, newdata = whole_data$train)[,1],
                plot=T); grid()

roc_nnet<-roc(response=df_pred$output,
                predictor=predict(nnet_0, newdata = whole_data$test)[,1],
                plot=T); grid()

###############################

#### model evaluation

result<-data.table(method=c('tree','rf','xgb','lm','glmnet','xgb_reg','nnet'),
                   rmse=sapply(df_pred[,!c('price','id')], rmse, real=df_pred$price),
                   mae=sapply(df_pred[,!c('price','id')], mae, real=df_pred$price),
                   mape=sapply(df_pred[,!c('price','id')], mape, real=df_pred$price))


result


result[which.min(result$rmse)]
result[which.min(result$mae)]
result[which.min(result$mape)]


str(df_pred)



