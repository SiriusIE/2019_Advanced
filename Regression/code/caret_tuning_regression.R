# Hyperparameter tuning with caret
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/carga_librerias.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')



whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_automobile_ready.csv'),
                        test_proportion = 0.2,
                        seed = 872367823)


str(whole_data)


# check for non-contant variables
sum(sapply(whole_data[['train']],function(x) var(x)==0))
sum(sapply(whole_data[['test']],function(x) var(x)==0))

str(whole_data)

lapply(whole_data, dim)

library(caret)

formula<-as.formula(price~.) 

#### rf ####

# 1. Define our grid of hyperparameters to tune
tuneGrid=data.table(expand.grid(mtry=c(5,15),
                                splitrule='variance',
                                min.node.size=c(2,5,10)))


dim(tuneGrid)
tuneGrid


# 2. Define the validation squema
ctrl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions=TRUE
)

# 3. Train the model
ini<-now()
set.seed(123)
rangerFit <- train(
  formula,
  data = whole_data$train,
  method = "ranger", num.trees=1000,
  preProc = NULL, 
  tuneGrid = tuneGrid,
  trControl = ctrl,
  metric = "MAE"
)
print(now()-ini)


rangerFit


# inspecting the most relevant features: 
rangerFit$results
rangerFit$bestTune
rangerFit$finalModel

# we can access the K-fold validation predictions
str(rangerFit$pred)
pred_cv<-data.table(rangerFit$pred)
pred_cv[Resample=='Fold1'&mtry==5&min.node.size==5]

# and visualize a CV summary using the established metric
plot(rangerFit)

# we save the train object
saveRDS(rangerFit,'/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/rangerFit_auto.RData')


# 4. Fit the model to all train data
library(ranger)
ini<-now()
finalmodel<-ranger(formula, data=whole_data$train,num.trees=1000,
                   mtry=rangerFit$bestTune$mtry,
                   min.node.size=rangerFit$bestTune$min.node.size)

print(now()-ini)


# 5. Predict on train (fit) and test data
pred_train<-predict(finalmodel, whole_data$train)$predictions
mape(real = whole_data$train$price,
     predicted = pred_train)
mae(real = whole_data$train$price,
    predicted = pred_train)

pred_test<-predict(finalmodel, whole_data$test)$predictions
mape(real = whole_data$test$price,
     predicted = pred_test)
mae(real = whole_data$test$price,
     predicted = pred_test)

mae(real = whole_data$test$price,
    predicted = pred_test)

df_pred<-data.table(id=1:length(pred_test),
                    real=whole_data$test$price,
                    pred_test)
df<-melt(df_pred,id.vars = 'id'); df
ggplot(df, aes(x=id, y=value, colour=variable))+geom_line()





