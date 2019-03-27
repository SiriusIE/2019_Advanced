f_plot_roc<-function(real, predicted){
  library(reportROC)
  
  f_eval_xgb<-function(model=rf_0,test_data=whole_data$test,test_y){
    
    pred=predict(model, newdata = test_data, type='class')
    
    conf<-table(pred, factor(test_data$y_yes==1))
    # print(conf)
    
    pr <- prediction(pred, test_y)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    # print(auc)
    
    
    if(plot_roc==T){
      prf<-data.table(x=prf@x.values[[1]], y=prf@y.values[[1]])
      p<-qplot(x,y,data=prf, geom='line',xlab='false positive rate',ylab='true positive rate',main=paste(grupo,'Extreme Gradient Boosting',sep='-'))+theme_minimal()
      print(p+annotate('text',0.25,0.75,label=paste0('AUC = ',round(auc,3))))
    }
    
    return(list(auc=auc, conf=conf))
    
  }
  
  
  
}