source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/load_libraries.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')


#### the boot library  ####
# Bootstrap 95% CI for regression coefficients 
library(boot)

df<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_automobile_ready.csv'),
                test_proportion = 0,
                seed = 872367823)$train

# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=df, statistic=bs, 
                R=1000, formula=price~hp)

summary(true_model<-lm(price~hp, data=df))

# view results
results
results$t
plot(results, index=1) # intercept 
plot(results, index=2) # hp


# get 95% confidence intervals 
boot.ci(results, type="perc", index=2, conf=c(0.95)) 

# manually:
quantile(results$t[,2], prob=c(0.025,0.975))






#### creating confidence intervals for ANY! model prediction ####

whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_house_ready.csv'),
                        test_proportion = 0.0005,
                        seed = 872367823)


# we'll reduce the train sample for the example
whole_data$train<-whole_data$train[sample(1000)]


lm_0<-lm(formula = price~., 
         data=whole_data$train)
summary(lm_0)

test_lm<-predict(lm_0, newdata = whole_data$test)

mae(real=whole_data$test$price, predicted = test_lm)
mape(real=whole_data$test$price, predicted = test_lm)

df_pred<-whole_data$test[, .(id=1:.N,price, test_lm)]
head(df_pred)

ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  xlab('')+ylab('$')+
  ggtitle('lm - Test Prediction on House Price')+
  scale_colour_manual(values = c('black','red'))



# how to calculate a confidence interval for prediction?? 

# bootstraping

n=1000
m=1000

predictions<-data.table(price=whole_data$test$price,
                        prediction=predict(lm(price~.,data=whole_data$train),
                                           newdata = whole_data$test)
                        )
predictions<-cbind(predictions,
                   matrix(rep(0,n),nrow=nrow(whole_data$test),
                          ncol=n))
dim(predictions)

plot(predictions$price, type='l', lwd=1); grid()


ini<-now()
pb = txtProgressBar(min = 0, max = n, style = 3) 
for(i in seq(3,n+2)){
  # if(i%%100==0) print(i)
  set.seed(i)
  train_data<-whole_data$train[sample(m, replace = T)]
  train_model<-lm(price~.,data=train_data)
  predictions[[i]]<-suppressWarnings(
    predict(train_model,
            newdata = whole_data$test)
  )
  setTxtProgressBar(pb,i)
  
  lines(predictions[[i]], col=alpha('royalblue', 0.25), lwd=0.5)
  
}
print(now()-ini)

predictions[,-c(1,2)]

lines(predictions$price, type='l', lwd=1)
lines(predictions$prediction, col='orange', lwd=1.5)


cor(predictions$prediction,rowMeans(predictions[, -c(1,2)]))


df_ci<-predictions[,c(1:2)]
df_ci[, lower:=apply(predictions[,-c(1,2)],1,quantile,0.025)]
df_ci[, upper:=apply(predictions[,-c(1,2)],1,quantile,0.975)]
df_ci

lines(df_ci$lower, col='red', lwd=0.75)
lines(df_ci$upper, col='red', lwd=0.75)



