# ML for continuous target variable

# 1. Tree Based Models
# 2. Regression
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/load_libraries.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')

whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_black_ready.csv'),
                        test_proportion = 0.1,
                        seed = 872367823)


str(whole_data)

whole_data<-lapply(whole_data, function(x) x[complete.cases(x)])

whole_data<-lapply(whole_data, function(x){
  return(x[, which(sapply(x, is.integer)):=lapply(.SD, as.numeric), .SDcols=sapply(x,is.integer)])
})

str(whole_data)


# we start defining a formula
formula<-as.formula(Purchase~.)   # Purchase against all other variables


df_pred<-data.table(id=seq(1:nrow(whole_data$test)),
                    Purchase=whole_data$test$Purchase)

library(ranger)

rf_1<-ranger(formula, whole_data$train,mtry=50, min.node.size = 25,num.trees = 500,
             write.forest = T)
test_rf1<-predict(rf_1,whole_data$test)$predictions
mape(real=whole_data$test$Purchase, predicted = test_rf1)
qplot(whole_data$test$Purchase, test_rf1)

df_pred<-cbind(df_pred, test_rf1)


ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Random Forest - Test Prediction on Automobile Purchase')+
  scale_colour_manual(values = c('black','red'))


#### 1.3 Boosting Tree
library(xgboost)

# for this algorithm we need to convert data to a matrix first
ini<-now()
xgb_0<-xgboost(booster='gbtree',
               data=as.matrix(whole_data$train[, !'Purchase', with=F]),
               label=whole_data$train$Purchase,
               nrounds = 500,
               objective='reg:linear')
print(now()-ini)
print(xgb_0)

test_xgb<-predict(xgb_0, newdata = as.matrix(whole_data$test[, !'Purchase', with=F]), type='response')

df_pred<-cbind(df_pred, test_xgb)
str(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Boosted Tree - Test Prediction on Automobile Purchase')+
  scale_colour_manual(values = c('black','red','blue'))


rmse_xgb<-rmse(real=whole_data$test$Purchase, predicted = test_xgb)
mae_xgb<-mae(real=whole_data$test$Purchase, predicted = test_xgb)
mape_xgb<-mape(real=whole_data$test$Purchase, predicted = test_xgb)
mape_xgb





# model evaluation

result<-data.table(method=c('rf','xgb'),
                   rmse=sapply(df_pred[,!c('Purchase','id')],function(x) return(rmse(real=df_pred$Purchase, predicted=x))),
                   mae=sapply(df_pred[,!c('Purchase','id')],function(x) return(mae(real=df_pred$Purchase, predicted=x))),
                   mape=sapply(df_pred[,!c('Purchase','id')],function(x) return(mape(real=df_pred$Purchase, predicted=x))))


result


result[which.min(result$mape)]

# plotting results metrics

ggplot(result, aes(x=method, y=mape))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=rmse))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=mae))+geom_bar(stat='identity')
