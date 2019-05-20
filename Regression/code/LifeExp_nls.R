library(data.table)
library(ggplot2)
theme_set(theme_minimal(base_size = 16))
library(lubridate)
library(plotly)
library(ggrepel)

a<-fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/wb_health_spending.csv')
b<-fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/wb_life_exp.csv')

colnames(a)<-as.character(a[1])
colnames(b)<-as.character(b[1])

a<-a[-1]
b<-b[-1]


a_2016<-a[, .(name=`Country Name`, code=`Country Code`, health_spending=`2016`)]
a_2016

b_2016<-b[, .(name=`Country Name`, code=`Country Code`, life_exp=`2016`)]
b_2016


df<-merge(a_2016, b_2016, by=c('name','code'))
df<-df[complete.cases(df)]
df

fwrite(df,'/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/LifeExp.csv')


df[grep('Spain',name)]


ggplot(df, aes(x=health_spending, y=life_exp))+
  geom_point(col='royalblue', alpha=0.5, size=3)+
  geom_smooth(method = "lm", formula = y ~ I(1-exp(-x/600)),
              se = F,  col='navyblue', lwd=2)+
  # geom_label_repel(aes(x=health_spending, y=life_exp, label=code, alpha=0.2))+
  xlab('Gasto Sanitario per Cápita, 2016 (PPA$)')+
  ylab('Esperanza de Vida, 2016')


summary(lm(life_exp~health_spending,data=df))


formula<-as.formula(life_exp~beta1*(1/(1+exp(-(beta2+beta3*health_spending)))))

coef_ini<-list(beta1=0,
               beta2=0.01,
               beta3=0.5)

library(minpack.lm)

nlm1<-nlsLM(formula=formula,data=df,start=coef_ini,
             control = nls.lm.control(maxiter = 100))
summary(nlm1)
nlm1


plot(df$life_exp, fitted(nlm1), xlim=c(50,100), ylim=c(50,100))
plot(df$life_exp, type='l')
lines(fitted(nlm1), col='red')


df[, fitted:=fitted(nlm1)]

ggplot(df, aes(x=health_spending, y=life_exp))+
  geom_point(col='royalblue', alpha=0.5, size=3)+
  # geom_smooth(method = "lm", formula = y ~ I(1-exp(-x/600)),
  #             se = F,  col='navyblue', lwd=2)+
  # geom_label_repel(aes(x=health_spending, y=life_exp, label=code, alpha=0.2))+
  xlab('Gasto Sanitario per Cápita, 2016 (PPA$)')+
  ylab('Esperanza de Vida, 2016')+
  geom_line(aes(x=health_spending, y=fitted))

