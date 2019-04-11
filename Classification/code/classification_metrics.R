library(pROC)
f_metrics<-function(real,predicted, t=0.5){
  
  cf<-table(predicted=factor(predicted>=t),real=real)
  print(cf)
  
  TP<-cf['TRUE','TRUE']
  FN<-cf['FALSE','TRUE']
  TN<-cf['FALSE','FALSE']
  FP<-cf['TRUE','FALSE']
  
  
  sensitivity<-TP/(TP+FN)  
  specificity<-TN/(TN+FP)
  accuracy<-(TP+TN)/(TN+FP+TP+FN)
  precission<-TP/(TP+FP)
  
  auc<-as.numeric(pROC::roc(response=real,
                      predictor=predicted)$auc)
  
  return(list(sensitivity=sensitivity,
              specificity=specificity,
              accuracy=accuracy,
              precission=precission,
              auc=auc))
}


f_metrics_simple<-function(real,predicted){
  
  cf<-table(predicted=predicted,real=real)
  
  TP<-cf['TRUE','TRUE']
  FN<-cf['FALSE','TRUE']
  TN<-cf['FALSE','FALSE']
  FP<-cf['TRUE','FALSE']
  
  
  sensitivity<-TP/(TP+FN)  
  specificity<-TN/(TN+FP)
  accuracy<-(TP+TN)/(TN+FP+TP+FN)
  precission<-TP/(TP+FP)
  
  return(list(sensitivity=sensitivity,
              specificity=specificity,
              accuracy=accuracy,
              precission=precission))
}

f_roc_point<-function(real,predicted,t){
  
  cf<-table(predicted=factor(predicted>=t),real=real)

  TP<-cf['TRUE','TRUE']
  FN<-cf['FALSE','TRUE']
  TN<-cf['FALSE','FALSE']
  FP<-cf['TRUE','FALSE']
  
  
  sensitivity<-TP/(TP+FN)  
  specificity<-TN/(TN+FP)

  
  return(c(sensitivity=sensitivity,specificity=specificity))
}


