library(readxl)
library(fixest)
library(ivreg)

setwd("C:/Users/vitoria.wendt/Desktop")
dados=read_xlsx("AK1991.xlsx")

####################################################
################QUESTÃO 1###########################
####################################################
#Restrict the sample only to black. 
dados_black=dados[dados$black==1,]

#Run an OLS regression of log wages on education, urban and married, as well as dummies for year-of-birth, state-of-birth 
#and region-of-residence (these dummies do not need to be reported).
#com dummies
for(j in seq(min(dados$yob),max(dados$yob)))

ols_regression=lm(logwage~edu+married+smsa+i(yob)+i(state)+i(region),data=dados_black)
summary(ols_regression)

#Sem dummies
ols_regression=feols(logwage~edu+married+smsa|(yob)+(state)+(region),data=dados_black)
summary(ols_regression)



####################################################
################QUESTÃO 2###########################
####################################################
#Pode desenhar DAG na mão



####################################################
################QUESTÃO 3###########################
####################################################
#Now run an 2SLS regression using as instruments quarter-of-birth times year-of-birth (30 instruments). 
First_stage=lm(edu~i(yob*qob),data=dados_black)
summary(First_stage)
TwoSLS=ivreg(logwage~edu+married+smsa|i(qob*yob),data=dados_black)
summary(TwoSLS)

#First stage regression


#Report the first stage, reduced form and 2SLS estimates.
