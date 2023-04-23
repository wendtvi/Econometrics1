#install.packages("readxl")
library(readxl)
#install.packages("ivreg")
library(ivreg)
#install.packages("fastDummies")
library(fastDummies)

setwd("/Users/imac/Desktop")
dados=read_xlsx("AK1991.xlsx")

####################################################
################QUESTÃO 1###########################
####################################################
#Restrict the sample only to black. 
dados_black=dados[dados$black==1,]

#Run an OLS regression of log wages on education, urban and married, as well as dummies for year-of-birth, state-of-birth 
#and region-of-residence (these dummies do not need to be reported).
#com dummies
dados_black_dummies=dummy_cols(dados_black,select_columns = c("yob","state","region"))
dados_black_dummies_reduzido=cbind(dados_black_dummies$logwage,dados_black_dummies$edu,dados_black_dummies$married,dados_black_dummies$smsa,
                                   (dados_black_dummies[,(ncol(dados)+1):ncol(dados_black_dummies)]))
names(dados_black_dummies_reduzido)=gsub("dados_black_dummies",names(dados_black_dummies_reduzido),replacement="")
ols_regression=lm(dados_black_dummies_reduzido$`$logwage`~.,data=dados_black_dummies_reduzido)
summary(ols_regression)


####################################################
################QUESTÃO 2###########################
####################################################
#Pode desenhar DAG na mão



####################################################
################QUESTÃO 3###########################
####################################################
#Now run an 2SLS regression using as instruments quarter-of-birth times year-of-birth (30 instruments). 
dados_black_exclude_qob4=dados_black[dados_black$qob!=4,]

#First stage reg
lm_1stage=lm(dados_black_exclude_qob4$`edu`~I(as.factor(dados_black_exclude_qob4$`qob`):as.factor(dados_black_exclude_qob4$`yob`)),data=dados_black_exclude_qob4)
summary(lm_1stage)

lm_2stage=lm(dados_black_exclude_qob4$`logwage`~lm_1stage$fitted.values+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`)
summary(lm_2stage)


####################################################
################QUESTÃO 4###########################
####################################################
#pode desenhar na mao


####################################################
################QUESTÃO 5###########################
####################################################
#Endogenity test (Durbin-Wu-Watson endogeneity tests)
regOrig=lm(dados_black_exclude_qob4$`logwage`~dados_black_exclude_qob4$edu+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`)
Res_regOrig=as.vector(regOrig$residuals)

regInZ=lm(dados_black_exclude_qob4$edu~I(as.factor(dados_black_exclude_qob4$`qob`):as.factor(dados_black_exclude_qob4$`yob`))+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`)
Res_regInZ=as.vector(regInZ$residuals)

Reg_resOrig=lm(Res_regOrig~dados_black_exclude_qob4$edu+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`+Res_regInZ)
## Lagrange Multiplier Statistic:
LM = length(Reg_resOrig$residuals) * summary(Reg_resOrig)$r.squared
pchisq(q = LM, df = 30, lower.tail = F)

#Overidentification test


####################################################
################QUESTÃO 6###########################
####################################################
#IV regression
IVregression=ivreg(dados_black_exclude_qob4$`logwage`~dados_black_exclude_qob4$`edu`+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`|
               (dados_black_exclude_qob4$`qob`:dados_black_exclude_qob4$`yob`)+dados_black_exclude_qob4$`smsa`+dados_black_exclude_qob4$`married`,data=dados_black_exclude_qob4)
summary(IVregression)
