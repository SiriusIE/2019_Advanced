
# Manual CV

source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/load_libraries.R')
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



# Define number of validation folds
nfold<-10

# Split train data row indexes on n folds
set.seed(123)
sampling_rows<-sample(1:nrow(whole_data$train))
holdout <-suppressWarnings(  split(x=sampling_rows, f=1:nfold))
holdout
lapply(holdout, length)
length(unlist(holdout))


# We define empty objects for temporal storage of the train model 
train_model<-list()
# And for predictions and performance meassure
test_predictions<-list()
mae_predictions<-list()

# The performance metric on each iteration throught the
# tunegrid will be stored on the same grid's dataframe
training_params<-data.table(expand.grid(mtry=c(5,15),
                                        splitrule='variance',
                                        min.node.size=c(2,5,10)))
training_params[, mean_mae_cv:=numeric()]
training_params


formula<-as.formula(price~.) 


# start the computation
ini<-now()
for(i in 1:nrow(training_params)){  # iterator of hyperparameter throught the grid
  
  param_mtry<-training_params[i][['mtry']]
  param_splitrule<-as.character(training_params[i][['splitrule']])
  param_min.node.size<-training_params[i][['min.node.size']]
  
  for(j in 1:nfold){                # iterator of validation fold 
    
    # training
    train_model[[j]]<-ranger::ranger(formula, data=whole_data$train[-holdout[[j]]],num.trees=1000,
                                     mtry = param_mtry, 
                                     splitrule = param_splitrule, 
                                     min.node.size = param_min.node.size)
    # predicting
    test_predictions[[j]]<-predict(train_model[[j]],data=whole_data$train[holdout[[j]]])$predictions
    
    # optional ploting of validation metrics (slow)
    # p<-qplot(x=whole_data$train[holdout[[j]]]$price,y=test_predictions[[j]], geom=c('point'))+geom_smooth(method='lm')
    # suppressMessages(plot(p))
    
    # evaluating
    mae_predictions[[j]]<-mae(real = whole_data$train[holdout[[j]]]$price,
                              predicted = test_predictions[[j]])
  }
  
  # once we have computed the n-fold validation performance metrics we average them
  training_params[i, mean_mae_cv:=mean(unlist(mae_predictions))] 
  print(training_params[i])
}
print(now()-ini)


training_params
training_params[which.min(training_params$mean_mae_cv)]


# compare it with caret optimization
rangerFit<-readRDS('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/rangerFit_auto.RData')
rangerFit$bestTune


