# Tree Based Models
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/load_libraries.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/regression_metrics.R')


whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/data_automobile_ready.csv'),
                        test_proportion = 0.2,
                        seed = 872367823)

str(whole_data)

whole_data<-lapply(whole_data, function(x){
  return(x[, which(sapply(x, is.integer)):=lapply(.SD, as.numeric), .SDcols=sapply(x,is.integer)])
})

str(whole_data)

# we start defining a formula
formula<-as.formula(price~.)   # price against all other variables


#### 1.1 Base R Partitioning Tree 
library(rpart)
library(rpart.plot)
library(partykit)
tree_0<-rpart(formula = formula, data = whole_data$train, method = 'anova', model=TRUE, cp=0)

print(tree_0)
print(as.party(tree_0))

objects(tree_0)
tree_0$frame
tree_0$control
tree_0$variable.importance

tree_0$cptable
plot(tree_0$cptable, type='b'); grid()

prune(tree_0, cp=max(tree_0$cptable[,'CP']))
prune(tree_0, cp=min(tree_0$cptable[,'CP']))


# plotting the tree
# basic plot
plot(tree_0, uniform = T,branch=0.5,compress = T)
text(tree_0, cex=0.75)

# rpart.plot from the rpart.plot library
rpart.plot(tree_0,fallen.leaves = F)
rpart.plot(tree_0,fallen.leaves = T,box.palette = 'Gn')

# interactive tree plot
library(visNetwork)

visTree(tree_0)
visTree(tree_0, fallenLeaves = T,
        edgesFontSize = 24, 
        nodesFontSize = 24, 
        legend = T,
        colorVar = RColorBrewer::brewer.pal(12,'Paired'),
        colorEdges = 'darkgray',
        colorY = c('palegreen','tomato'),
        main='Regression Tree',
        tooltipDelay = 0.001,
        digits=0,
        minNodeSize=10, 
        highlightNearest = list(enabled = TRUE, hover = TRUE, algorithm = "hierarchical"),
        collapse = list(enabled = TRUE, fit = TRUE, resetHighlight = TRUE,
                        clusterOptions = list(fixed = F, physics = F)),
        nodesPopSize=T,
        edgesFontAlign = "horizontal")


# let's generate an NA in engine_size

whole_data$train_2<-copy(whole_data$train)
whole_data$train_2[27][['engine_size']]<-NA

sum(is.na(whole_data$train))
sum(is.na(whole_data$train_2))

tree_1<-rpart(formula = formula, data = whole_data$train_2, method = 'anova', model=TRUE, cp=0)

summary(tree_1)[[1]]
print(as.party(tree_1))
print(as.party(tree_0))

whole_data$train_2[27]
predict(tree_1,whole_data$train_2[27])

# another type of partitioning algorithm: Conditional Inference Tree
library(partykit)
ctree_0<-ctree(formula, data = whole_data$train)
print(ctree_0)
plot(ctree_0)


# predicting on train and test set
train_tree<-predict(tree_0)
test_tree<-predict(tree_0, newdata = whole_data$test,type = 'vector')

df_fit<-whole_data$train[, .(id=1:.N,price, train_tree)]
str(df_fit)
df_pred<-whole_data$test[, .(id=1:.N,price, test_tree)]
str(df_pred)

p1<-ggplot(melt(df_fit, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Regression Tree - Train Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','red'))

p2<-ggplot(melt(df_pred, id.vars = 'id'), aes(x=id,y=value, colour=variable))+
  geom_point(alpha=0.65)+geom_line(alpha=0.65)+
  ylim(0,50000)+xlab('')+ylab('$')+
  ggtitle('Regression Tree - Test Prediction on Automobile Price')+
  scale_colour_manual(values = c('black','blue'))

library(gridExtra)
grid.arrange(p1,p2, ncol=1)

# Calculating the performance metrics on test set
rmse_tree<-rmse(real=whole_data$test$price, predicted = test_tree); rmse_tree
mae_tree<-mae(real=whole_data$test$price, predicted = test_tree); mae_tree
mape_tree<-mape(real=whole_data$test$price, predicted = test_tree); mape_tree

# compare with train
mape(whole_data$train$price,train_tree)




