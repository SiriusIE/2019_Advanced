
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/carga_librerias.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/classification_metrics.R')

df<-fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Classification/data_credit_ready.csv')

whole_data<-f_partition(df=df,
                        test_proportion = 0.2,
                        seed = 872367823)

# whole_data<-lapply(whole_data,function(x) setnames(x,'y_yes','target')) 

str(whole_data)

summary(whole_data$train$target)
summary(whole_data$test$target)



# let's load our benchmarking results

result<-readRDS('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/credit_results.RData')
print(result)



library(caret)

table(whole_data$train$target)


set.seed(9560)
down_train <- data.table(downSample(x = whole_data$train[, -'target'],
                         y = factor(whole_data$train$target==1),yname = 'target'))

table(down_train$target) 
dim(down_train)
sum(whole_data$train$target==1)*2


set.seed(9560)
up_train <- data.table(upSample(x = whole_data$train[, -'target'],
                                  y = factor(whole_data$train$target==1),yname = 'target'))                   
table(up_train$target) 
dim(up_train)
sum(whole_data$train$target==0)*2


library(DMwR)

x<-copy(whole_data$train)
x[, target:=factor(target==1)]  # we make sure the target variable is defined as a factor

set.seed(9560)
ini<-now()
smote_train <- data.table(SMOTE(target ~ ., data  = data.frame(x)))
print(now()-ini)
table(smote_train$target) 


###############

print(result)

# let's try to better up our rf algo using different resampling techniques:

library(ranger)


# formula definition
formula<-as.formula(target~.)

# benchmark
ini<-now()
rf_0<-ranger(formula=as.formula(factor(target==1)~.), data=whole_data$train,num.trees = 500, probability = T)
print(now()-ini)
print(rf_0)

test_rf<-predict(rf_0, whole_data$test)$predictions[, 'TRUE']

# undersample
ini<-now()
rf_under<-ranger(formula=formula, data=down_train,num.trees = 500, probability = T)
test_rf_under<-predict(rf_under, whole_data$test)$predictions[, 'TRUE']
print(now()-ini)

# oversample
ini<-now()
rf_over<-ranger(formula=formula, data=up_train,num.trees = 500, probability = T)
test_rf_over<-predict(rf_over, whole_data$test)$predictions[, 'TRUE']
print(now()-ini)

# smote sampling
ini<-now()
rf_smote<-ranger(formula=formula, data=smote_train,num.trees = 500, probability = T)
test_rf_smote<-predict(rf_smote, whole_data$test)$predictions[, 'TRUE']
print(now()-ini)



# performance evaluation
df_pred<-whole_data$test[, .(id=1:.N,output=factor(target==1))]
df_pred<-cbind(df_pred, test_rf=factor(test_rf>=0.5),
               test_rf_under=factor(test_rf_under>=0.5),
               test_rf_over=factor(test_rf_over>=0.5),
               test_rf_smote=factor(test_rf_smote>=0.5))


df_pred

f_metrics(real=df_pred$output, predicted = test_rf, t=0.5)
f_metrics(real=df_pred$output, predicted = test_rf_under, t=0.5)
f_metrics(real=df_pred$output, predicted = test_rf_over, t=0.5)
f_metrics(real=df_pred$output, predicted = test_rf_smote, t=0.5)


result<-data.table(method=c('rf','rf_under','rf_over','rf_smote'),
                   t(sapply(df_pred[,!c('output','id')], f_metrics_simple, real=df_pred$output)))


prob_pred<-cbind(df_pred[, .(output)], test_rf,test_rf_under, test_rf_over, test_rf_smote)
result<-cbind(result, auc=unlist(sapply(prob_pred[,!c('output')], f_metrics, real=df_pred$output,t=0.5)['auc',]))

print(result)





