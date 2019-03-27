# ML for continuous target variable

# 1. Tree Based Models
# 2. Regression
# 3. Neural Networks

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


tuneGrid=data.table(expand.grid(mtry=unique(rev(floor(ncol(whole_data$train)/c(10,20)))),
                                splitrule='variance',
                                min.node.size=c(10,20)))
ctrl <- trainControl(
  method = "cv"
)



ini<-now()
set.seed(123)
rangerFit <- train(
  formula,
  data = whole_data$train,
  method = "ranger", num.trees=100,
  preProc = c("center", "scale"),
  tuneGrid = tuneGrid,
  trControl = ctrl,
  metric = "RMSE"
)
print(now()-ini)

rangerFit

rangerFit$bestTune
