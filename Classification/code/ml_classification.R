# ML for continuous target variable

# 1. Tree Based Models
# 2. Logistic Regression Models

source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/load_libraries.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')


df<-fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Classification/data_heart_ready.csv')

whole_data<-f_partition(df=df,
                        test_proportion = 0.2,
                        seed = 872367823)

whole_data<-lapply(whole_data,function(x) setnames(x,'target_1','target')) 

str(whole_data)

summary(whole_data$train$target)
summary(whole_data$test$target)

# formula definition
formula<-as.formula(factor(target==1)~.)


#### 1.1 Base R Partitioning Tree 
library(rpart)
library(rpart.plot)
library(partykit)

ini<-now()
tree_0<-rpart(formula = formula, data = whole_data$train, method = 'class', model = TRUE)
print(now()-ini)

print(as.party(tree_0))

rpart.plot(tree_0, digits = 4,type = 2,box.palette = 'Gn')

test_tree<-predict(tree_0, newdata = whole_data$test,type = 'prob')[, 'TRUE']

df_pred<-whole_data$test[, .(id=1:.N,output=factor(target==1), test_tree=factor(test_tree>=0.5))]
str(df_pred)

cf_tree<-table(predicted=df_pred$test_tree, real=df_pred$output)
print(cf_tree)
plot(cf_tree)

library(pROC)

roc_tree_train<-roc(response=factor(whole_data$train$target==1),
                    predictor=predict(tree_0, data = whole_data$train,type = 'prob')[, 'TRUE']); grid()
roc_tree_train

roc_tree_test<-roc(response=df_pred$output,
                   predictor=predict(tree_0, newdata = whole_data$test,type = 'prob')[, 'TRUE']); grid()

roc_tree_test

plot.roc(roc_tree_train)
plot.roc(roc_tree_test)

library(caret)
cf_performance<-confusionMatrix(data = df_pred$test_tree, reference=df_pred$output,positive='TRUE')
cf_performance

# now we compute manually the metrics from the cf_tree

TP<-cf_tree['TRUE','TRUE']
FN<-cf_tree['FALSE','TRUE']
TN<-cf_tree['FALSE','FALSE']
FP<-cf_tree['TRUE','FALSE']


sensitivity<-TP/(TP+FN)  # or true positive rate or recall
sensitivity==cf_performance$byClass[['Sensitivity']]

specificity<-TN/(TN+FP)
specificity==cf_performance$byClass[['Specificity']]

accuracy<-(TP+TN)/(TN+FP+TP+FN)
accuracy==cf_performance$overall[['Accuracy']]

precission<-TP/(TP+FP)
precission==cf_performance$byClass[['Precision']]




# using my own functions
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/classification_metrics.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/f_plot_roc.R')


f_metrics(real=df_pred$output, predicted=test_tree)
f_plot_roc(real=df_pred$output, predicted=test_tree, title='test tree')


#### 1.2 Random Forest
# library(randomForest)
library(ranger)

ini<-now()
rf_0<-ranger(formula=formula, data=whole_data$train,num.trees = 500, probability = T)
print(now()-ini)
print(rf_0)

test_rf<-predict(rf_0, whole_data$test)$predictions[, 'TRUE']

df_pred<-cbind(df_pred, test_rf=factor(test_rf>=0.5))
str(df_pred)

f_metrics(real=df_pred$output, predicted=test_rf)
f_plot_roc(real=df_pred$output, predicted=test_rf, title='test rf')


# geting roc points manually out of sensitivity and specificity
manual_roc<-t(sapply(seq(0.1,0.9,0.01),f_roc_point,real=df_pred$output, predicted=test_rf))
points(manual_roc[,'specificity'],manual_roc[,'sensitivity'], pch=20, col='red')


#### 1.3 Boosting Tree
library(xgboost)

# for this algorithm we need to convert to a matrix first

ini<-now()
xgb_0<-xgboost(booster='gbtree',
               data=as.matrix(whole_data$train[, !'target', with=F]),
               label=whole_data$train$target==1,
               nrounds = 100,
               objective='binary:logistic')
print(now()-ini)
print(xgb_0)

test_xgb<-predict(xgb_0, newdata = as.matrix(whole_data$test[, !'target', with=F]))

df_pred<-cbind(df_pred, test_xgb=factor(test_xgb>=0.5))
str(df_pred)


f_metrics(real=df_pred$output, predicted=test_xgb)
f_plot_roc(real=df_pred$output, predicted=test_xgb, title='test xgb')

#### 2.1 Logistic Regression

ini<-now()
glm_0<-glm(formula = formula, 
           data=whole_data$train,
           family=binomial)
print(now()-ini)

summary(glm_0)


test_glm<-predict(glm_0, newdata = whole_data$test, type='response')

df_pred<-cbind(df_pred, test_glm=factor(test_glm>=0.5))
str(df_pred)

f_metrics(real=df_pred$output, predicted=test_glm)
f_plot_roc(real=df_pred$output, predicted=test_glm, title='test glm')

#### 2.2 Regression with regularization
library(glmnet)

glmnet_0<-cv.glmnet(x = data.matrix(whole_data$train[, !'target']), 
                    y = factor(whole_data$train[['target']]==1),
                    family = 'binomial',
                    alpha=1,
                    lambda = seq(0,0.01,0.0001))
print(glmnet_0$lambda.min)
glmnet_0<-glmnet(x = data.matrix(whole_data$train[, !'target']), 
                 y = factor(whole_data$train[['target']]==1),
                 family = 'binomial',
                 alpha=1, lambda = glmnet_0$lambda.min)

glmnet_0
glmnet_0$beta


test_glmnet<-as.numeric(predict(glmnet_0, newx = as.matrix(whole_data$test[, !'target']), type='response'))

df_pred<-cbind(df_pred, test_glmnet=factor(test_glmnet>=0.5))
str(df_pred)

f_metrics(real=df_pred$output, predicted=test_glmnet)
f_plot_roc(real=df_pred$output, predicted=test_glmnet, title='test glmnet')


#### 2.3 Boosting Logistic Regression
library(xgboost)


xgb_reg_0<-xgboost(booster='gblinear',
                   data=as.matrix(whole_data$train[, !'target', with=F]),
                   label=whole_data$train[['target']]==1,
                   nrounds = 200,
                   objective='binary:logistic')
print(xgb_reg_0)

test_xgb_reg<-as.numeric(predict(xgb_reg_0, newdata = as.matrix(whole_data$test[, !'target', with=F])))

df_pred<-cbind(df_pred, test_xgb_reg=factor(test_xgb_reg>=0.5))
str(df_pred)

f_metrics(real=df_pred$output, predicted=test_xgb_reg)
f_plot_roc(real=df_pred$output, predicted=test_xgb_reg, title='test xgb_reg')



###############################

#### model evaluation

result<-data.table(method=c('tree','rf','xgb','glm','glmnet','xgb_reg'),
                   t(sapply(df_pred[,!c('output','id')], f_metrics_simple, real=df_pred$output)))


prob_pred<-cbind(df_pred[, .(output)],test_tree, test_rf,test_xgb, test_glm, test_glmnet, test_xgb_reg)
result<-cbind(result, auc=unlist(sapply(prob_pred[,!c('output')], f_metrics, real=df_pred$output,t=0.5)['auc',]))

print(result)


result[which.max(result$sensitivity)]
result[which.max(result$specificity)]
result[which.max(result$precission)]
result[which.max(result$accuracy)]
result[which.max(result$auc)]

saveRDS(result,'/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/heart_results.RData')



# visualization of predicted probability for each class

df_prob<-cbind(df_pred[, .(id, output)], test_tree, test_glm, test_rf, test_xgb, test_glmnet)

ggplot(df_prob, aes(x=test_rf, fill=output, colour=output))+geom_density(alpha=0.5)+
  scale_colour_manual(values = c('gray','royalblue'))+
  scale_fill_manual(values = c('gray','royalblue'))+geom_vline(xintercept = 0.5)

ggplot(df_prob, aes(x=test_xgb, fill=output, colour=output))+geom_density(alpha=0.5)+
  scale_colour_manual(values = c('gray','royalblue'))+
  scale_fill_manual(values = c('gray','royalblue'))+geom_vline(xintercept = 0.5)


df_prob_melt<-melt(df_prob, id.vars = c('id','output'))
df_prob_melt


ggplot(df_prob_melt[output==TRUE], aes(x=value, colour=variable))+geom_density(alpha=0.5)
ggplot(df_prob_melt, aes(x=value, colour=variable))+geom_density(alpha=0.5)+facet_grid(~output)




# plotting several rocs on the same graph

f_metrics(real=df_pred$output, predicted=test_rf)
f_plot_roc(real=df_pred$output, predicted=test_rf, title='test rf')


# geting roc points manually out of sensitivity and specificity

# example for one method
manual_roc<-data.table(t=seq(0.1,0.9,0.0001),
                       t(sapply(seq(0.1,0.9,0.0001),f_roc_point,real=df_pred$output, predicted=test_rf)))

ggplot(manual_roc, aes(x=1-specificity, y=sensitivity))+geom_point()


# we put it in a function...
f_manual_roc<-function(t_seq=seq(0.1,0.9,0.0001), method){
  
  manual_roc<-data.table(method=method,
                         t=t_seq,
                         t(sapply(t_seq,f_roc_point,real=df_pred$output, predicted=get(method))))
  
  return(manual_roc)
}

# testing our function
f_manual_roc(method='test_rf')


# and we apply the function to our collection of methods trained
threshold_seq<-seq(0.05,0.95,0.01)
methods<-c('test_rf','test_xgb', 'test_glm')
roc_results<-lapply(methods, function(x) f_manual_roc(method=x,t_seq=threshold_seq))

roc_results<-rbindlist(roc_results)

ggplot(roc_results, aes(x=1-specificity, y=sensitivity, colour=method))+geom_path(size=1)+
  geom_abline(slope=1, col='gray')+xlim(0,1)+ylim(0,1)

par(mfrow=c(1,2))
plot(x=threshold_seq,roc_results[method=='test_rf']$specificity, type='l', xlab='t', ylab='sensitivity'); grid()
plot(x=threshold_seq,roc_results[method=='test_rf']$sensitivity, type='l', xlab='t', ylab='specificity'); grid()
dev.off()
