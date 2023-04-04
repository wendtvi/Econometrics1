library(readxl)
#install.packages("tinytex")
#install.packages("knitr")
library(tinytex)
library(knitr)
dados=read_xlsx("C:/Users/vitoria.wendt/Downloads/cps09mar.xlsx")

###################################################
###############QUESTÃO 1###########################
###################################################
#Run a linear regression of log earnings on age, age squared, sex and education. 
age_squared=dados$age^2
linear_regression=lm(log(earnings)~age+age_squared+female+education,data=dados)
summary(linear_regression)

#verificando se os resíduos da regressão e os valores estimados são iguais aos verdadeiraos valores de y
linear_regression$residuals+linear_regression$fitted.values==log(dados$earnings)

#calculando o Std erro dos coef na mão
#linear_regression$
  
#t valor

###################################################
###############QUESTÃO 2###########################
###################################################
# Add this variable to the regression.
dados[,ncol(dados)+1]=as.numeric(dados$age-15)
names(dados)[ncol(dados)]="experience"

# Calculate (X′X).
matrix_regressors=matrix(NA,nrow = nrow(dados),ncol=6)
matrix_regressors[,1]=1
matrix_regressors[,2]=dados$age
matrix_regressors[,3]=age_squared
matrix_regressors[,4]=dados$female
matrix_regressors[,5]=dados$education
matrix_regressors[,6]=dados$experience

matrix_regressors_squared=t(matrix_regressors) %*% matrix_regressors
matrix_XY=t(matrix_regressors) %*% log(dados$earnings)

beta_hat=solve(matrix_regressors_squared)%*%matrix_XY


###################################################
###############QUESTÃO 3###########################
###################################################
matrix_regressors=matrix(NA,nrow = nrow(dados),ncol=5)
matrix_regressors[,1]=1
matrix_regressors[,2]=dados$female
matrix_regressors[,3]=dados$education
matrix_regressors[,4]=dados$experience
matrix_regressors[,5]=dados$experience^2

matrix_regressors_squared=t(matrix_regressors) %*% matrix_regressors
matrix_XY=t(matrix_regressors) %*% log(dados$earnings)

beta_hat=solve(matrix_regressors_squared)%*%matrix_XY

#Verificando se a gente acertou no calculo dos coeficientes
linear_regression=lm(log(earnings)~female+education+experience+matrix_regressors[,5],data=dados)
summary(linear_regression)

#y estimados
y_fitted=beta_hat[1]*rep(1,nrow(dados))+beta_hat[2]*matrix_regressors[,2]+beta_hat[3]*matrix_regressors[,3]+beta_hat[4]*matrix_regressors[,4]+
  +beta_hat[5]*matrix_regressors[,5]

#matriz de var/cov
e_hat_squared=(log(dados$earnings)-y_fitted)^2
D=vector()
for (i in 1:nrow(dados)){
  D[i]=t(matrix_regressors[i,])%*%(matrix_regressors[i,])*e_hat_squared[i]
}
sum_D=sum(D)
var_beta_hat=solve(matrix_regressors_squared)%*%sum_D%*%solve(matrix_regressors_squared)

#SE













###################################################
###############QUESTÃO 4###########################
###################################################







#Salvando arquivo em Rmarkdown
rmarkdown::render("analysis.R")
rmarkdown::render("analysis.R", "pdf_document")
