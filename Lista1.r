library(readxl)
#install.packages("tinytex")
#install.packages("knitr")
library(tinytex)
library(knitr)
dados=read_xlsx("C:/Users/vitoria.wendt/Downloads/cps09mar.xlsx")
options(scipen = 999)
n=nrow(dados)

###################################################
###############QUESTÃO 1###########################
###################################################
#Run a linear regression of log earnings on age, age squared, sex and education. 
age_squared=dados$age^2
linear_regression=lm(0+log(earnings)~age+age_squared+female+education,data=dados)
summary(linear_regression)

#verificando se os resíduos da regressão e os valores estimados são iguais aos verdadeiraos valores de y
linear_regression$residuals+linear_regression$fitted.values==log(dados$earnings)


###################################################
###############QUESTÃO 2###########################
###################################################
# Add this variable to the regression.
dados[,ncol(dados)+1]=as.numeric(dados$age-15)
names(dados)[ncol(dados)]="experience"

# Calculate (X′X).
matrix_regressors=matrix(NA,nrow =n,ncol=5)
matrix_regressors[,1]=dados$age
matrix_regressors[,2]=age_squared
matrix_regressors[,3]=dados$female
matrix_regressors[,4]=dados$education
matrix_regressors[,5]=dados$experience
#matrix_regressors[,6]=rep(1,n)

matrix_regressors_squared=t(matrix_regressors) %*% matrix_regressors
matrix_XY=t(matrix_regressors) %*% log(dados$earnings)

beta_hat=solve(matrix_regressors_squared)%*%matrix_XY


###################################################
###############QUESTÃO 3###########################
###################################################
matrix_regressors=matrix(NA,nrow =n,ncol=4)
matrix_regressors[,1]=dados$female
matrix_regressors[,2]=dados$education
matrix_regressors[,3]=dados$experience
matrix_regressors[,4]=dados$experience^2

matrix_regressors_squared=t(matrix_regressors) %*% matrix_regressors
matrix_XY=t(matrix_regressors) %*% log(dados$earnings)

beta_hat=solve(matrix_regressors_squared)%*%matrix_XY

#Verificando se a gente acertou no calculo dos coeficientes
linear_regression=lm(log(earnings)~0+female+education+experience+matrix_regressors[,4],data=dados)
summary(linear_regression)

# Add this variable to the regression.
dados[,ncol(dados)+1]=as.numeric(dados$age-15)
names(dados)[ncol(dados)]="experience"

matrix_regressors=matrix(NA,nrow =n,ncol=4)
matrix_regressors[,1]=dados$female
matrix_regressors[,2]=dados$education
matrix_regressors[,3]=dados$experience
matrix_regressors[,4]=dados$experience^2

matrix_regressors_squared=t(matrix_regressors) %*% matrix_regressors
matrix_XY=t(matrix_regressors) %*% log(dados$earnings)

beta_hat=solve(matrix_regressors_squared)%*%matrix_XY

#Verificando se a gente acertou no calculo dos coeficientes
linear_regression=lm(log(earnings)~0+female+education+experience+matrix_regressors[,4],data=dados)
summary(linear_regression)

#y estimados
y_fitted=beta_hat[1]*matrix_regressors[,1]+beta_hat[2]*matrix_regressors[,2]+beta_hat[3]*matrix_regressors[,3]+beta_hat[4]*matrix_regressors[,4]
#matriz de var/cov heteroscedastico
D=(log(dados$earnings)-y_fitted)%*%t(log(dados$earnings)-y_fitted)
V_beta_hat=solve(matrix_regressors_squared)%*%(t(matrix_regressors)%*%D%*%matrix_regressors)%*%solve(matrix_regressors_squared)
s2=sum(diag(D))/(n-4)

#SE heteroscedastico
SD=sqrt(diag(V_beta_hat))
  
#matriz de var/cov homoscedcastico
V_beta_hat=solve(matrix_regressors_squared)*s2
#SE homoscedastico
SD=sqrt(diag(V_beta_hat))
#confere se homoscedastico são iguais ao retorno de lm
sqrt(diag(vcov(linear_regression)))


###################################################
###############QUESTÃO 4###########################
###################################################
I=matrix(0,ncol=n,nrow = n)
diag(I)=1
#Now use the Frisch-Waugh-Lovell theorem to estimate the effect of education on wages, while partialling out the controls in (1)
M=(I-(matrix_regressors[,-2]%*%solve(t(matrix_regressors[,-2])%*%matrix_regressors[,-2])%*%t(matrix_regressors[,-2])))
X_ed_star=M%*%matrix_regressors[,2]
Y_star=M%*%log(dados$earnings)
beta_ed_tilda=solve(t(X_ed_star)%*%X_ed_star)%*%(t(X_ed_star)%*%Y_star)
#Do they differ?
summary(linear_regression)


###################################################
###############QUESTÃO 5###########################
###################################################
#Consider a 18 years-old prospective college student deciding whether to study or work. He wants to know the ratio θ
#between returns of education and returns to experience, given his age. Write θ
#as a function of the parameters β and estimate it.
#Usa teorema do mapeamento continuo

###################################################
###############QUESTÃO 6###########################
###################################################
#Write out the formula for the asymptotic variance of θˆ as a function of the variance-covariance matrix and find the standard deviation of the estimator.


###################################################
###############QUESTÃO 7###########################
###################################################
#Construct a 90% confidence interval for θˆ


###################################################
###############QUESTÃO 8###########################
###################################################
#Test whether βeduc equals βexp+6βexp2 using a Wald statistic. Interpret.
#install.packages("aod")
library(aod)
wald.test(Sigma, b, Terms)
