
# ML for continuous target variable

# 1. Tree Based Models
# 2. Logistic Regression Models

source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/carga_librerias.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Classification/code/classification_metrics.R')

df<-fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Classification/data_bank_ready.csv')

whole_data<-f_partition(df=df,
                        test_proportion = 0.2,
                        seed = 872367823)

whole_data<-lapply(whole_data,function(x) setnames(x,'y_yes','target')) 

str(whole_data)

summary(whole_data$train$target)
summary(whole_data$test$target)

# formula definition
formula<-as.formula(factor(target==1)~.)





# mlp network

library(keras)

model_keras <- keras_model_sequential()

model_keras %>% 
  # (1) 1st Hidden Layer-------------------------------------------------
layer_dense (units              = 164, #=> Num Of Nodes
             kernel_initializer = "uniform", 
             activation         = "relu",    
             input_shape        = ncol(whole_data$train)-1) %>% 
  layer_dropout (rate = 0.1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2nd Hidden Layer-------------------------------------------------
layer_dense (units              = 16,
             kernel_initializer = "uniform", 
             activation         = "relu") %>% 
  layer_dropout (rate = 0.1) %>%  
  # (3) Output Layer-----------------------------------------------------
layer_dense (units              = 1, #=> Binary/Multi?=>That Number
             kernel_initializer = "uniform", 
             activation         = "sigmoid") %>% #=> Common for Binary
  # (4) Compile Model-----------------------------------------------------
compile (optimizer = 'adam', #=> Most Popular for Optimization Algo.
         loss      = 'binary_crossentropy', #=> Binary Classification
         metrics   = c('accuracy') ) #=> Train/Test Evaluation

# Check
model_keras


system.time ( 
  history <- fit (
    object           = model_keras,             # => Our Model
    x                = as.matrix (whole_data$train[, -'target']), #=> Matrix
    y                = whole_data$train[['target']],             #=> Numeric Vector 
    batch_size       = 50,     #=> #OfSamples/gradient update in each epoch
    epochs           = 100,     #=> Control Training cycles
    validation_split = 0.20) ) #=> Include 20% data for 'Validation' Model
print (history)


train_mlp <- as.numeric(predict_proba(object = model_keras, 
                                      x = as.matrix(whole_data$train[, -'target'])))


test_mlp <- as.numeric(predict_proba(object = model_keras, 
                                     x = as.matrix(whole_data$test[, -'target'])))

max(train_mlp)
max(test_mlp)

f_metrics(real=whole_data$train$target, predicted=train_mlp, t=0.5)
f_metrics(real=whole_data$test$target, predicted=test_mlp, t=0.5)

# plot the architechture
# devtools::install_github("andrie/deepviz")
library(deepviz)

plot_model(model_keras)


