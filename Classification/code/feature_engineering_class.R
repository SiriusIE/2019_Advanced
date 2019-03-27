source('Classification/code/carga_librerias.R')

raw_data<-fread('Datasets/bank-full.csv', stringsAsFactors = T)
str(raw_data)


# We have character and integer variables

data_proc<-copy(raw_data)

# some integer variables must be turned into factors
data_proc[, day:=factor(day)]
# pdays...lets change the value -1 to 999 (no previous contact)
data_proc[pdays==-1, pdays:=999]

integer_variables<-names(data_proc)[sapply(data_proc, is.integer)]
count_integer_variables<-sapply(data_proc[,integer_variables, with=F], function(x) length(unique(x)))
count_integer_variables

data_proc[, (integer_variables):=lapply(.SD, as.numeric), .SDcols=integer_variables]

str(data_proc)

# Now lets check the number of categories per factor variable

factor_variables<-names(data_proc)[sapply(data_proc, is.factor)]
count_factor_variables<-sapply(sapply(data_proc[,factor_variables, with=F], summary),sort,dec=T)
count_factor_variables

# if a label weight less than x% goes into the "others" bag:
f_other<-function(var,min_presence=0.01){
  
  count_levels<-summary(var)/length(var)
  to_bag<-names(which(count_levels<min_presence))
  
  reduced_var<-as.factor(ifelse(as.character(var)%in%to_bag,'others',as.character(var)))
  
  return(reduced_var)
}

# and we apply the function to our factor variables
data_proc[, (factor_variables):=lapply(.SD, f_other), .SDcols=factor_variables]

sapply(sapply(data_proc[,factor_variables, with=F], summary),sort,dec=T)



str(data_proc)



#### NA treatment

sum(is.na(data_proc))




# Binary encoding our factor variables (needed for most algos)

data_ready<-caret::dummyVars(" ~ .", data = data.frame(data_proc), fullRank=T,sep = "_")
data_ready<-data.table(predict(data_ready, newdata = data_proc))

names(data_ready)<-gsub('-','_',names(data_ready))

str(data_proc)
str(data_ready)


fwrite(data_ready, 'Datasets/data_bank_ready.csv', row.names = F)


