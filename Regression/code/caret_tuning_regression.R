# Hyperparameter tuning with caret
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/carga_librerias.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')



whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/data_automobile_ready.csv'),
                        test_proportion = 0.2,
                        seed = 872367823)


str(whole_data)


# binary variables are read as integer, not necessary, but we'll turn them to numeric 
whole_data<-lapply(whole_data, function(x){
  return(x[, names(x)[sapply(x,is.integer)]:=lapply(.SD, as.numeric), .SDcols=sapply(x,is.integer)])
})

str(whole_data)


library(caret)

formula<-as.formula(price~.) 


tuneGrid=data.table(expand.grid(mtry=c(5,10,15,20),
                                splitrule='variance',
                                min.node.size=c(2,5,10)))
dim(tuneGrid)
tuneGrid

ctrl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions=TRUE
)



ini<-now()
set.seed(123)
rangerFit <- train(
  formula,
  data = whole_data$train,
  method = "ranger", num.trees=500,
  preProc = NULL, #c("center", "scale"),
  tuneGrid = tuneGrid,
  trControl = ctrl,
  metric = "MAE"
)
print(now()-ini)

rangerFit

rangerFit$bestTune
rangerFit$finalModel

plot(rangerFit)

library(ranger)
finalmodel<-ranger(formula, data=whole_data$train,num.trees=500,mtry=14,min.node.size=2)

pred_test<-predict(finalmodel, whole_data$test)$predictions
mape(real = whole_data$test$price,
     predicted = pred_test)
mae(real = whole_data$test$price,
     predicted = pred_test)

# Manual CV

# tuneGrid
# 
# whole_data$train


nfold<-10

set.seed(123)
holdout <- split(sample(1:nrow(whole_data$train)), 1:nfold)
lapply(holdout, length)
length(unlist(holdout))

train_model<-list()
test_predictions<-list()
mae_predictions<-list()

training_params<-copy(tuneGrid)
training_params[, mean_mae_cv:=numeric()]





ini<-now()
for(i in 1:nrow(training_params)){
  param_mtry<-training_params[i][['mtry']]
  param_splitrule<-as.character(training_params[i][['splitrule']])
  param_min.node.size<-training_params[i][['min.node.size']]
  for(j in seq_len(nfold)){
    train_model[[j]]<-ranger::ranger(formula, data=whole_data$train[-holdout[[j]]],num.trees=500,
                                     mtry = param_mtry, 
                                     splitrule = param_splitrule, 
                                     min.node.size = param_min.node.size)
    test_predictions[[j]]<-predict(train_model[[j]],data=whole_data$train[holdout[[j]]])$predictions
    mae_predictions[[j]]<-mae(real = whole_data$train[holdout[[j]]]$price,
                              predicted = test_predictions[[j]])
  }
  training_params[i, mean_mae_cv:=mean(unlist(mae_predictions))] 
  print(training_params[i])
}
print(now()-ini)

training_params
training_params[which.min(training_params$mean_mae_cv)]

# compare it with caret optimization
rangerFit$bestTune
mae(real = whole_data$test$price,
    predicted = pred_test)
