##### Apply new estimator to ACS (sample) and CoreLogic tax (population) data

## Read in sample ACS data
# ACS responses from Washtenaw County for single family homes with variables:
  # value: log(value) of property, includes missing values
  # property tax: log(property tax paid), includes missing values
  # outcome_np: number of people in household
  # outcome_inc: household income
  # valueImp: log(value) of property, imputed missing values as mean from CoreLogic data
  # propTaxImp: log(propoerty tax paid), imputed missing values as mean from CoreLogic data
  # lotSize1: 1 if less than 1 acre of land
  # lotSize2: 1 if between 1 and 10 acres of land
sampleData = read.csv('ACSsample.csv')


## Population means
# means from CoreLogic tax data
# we do not make all CoreLogic observations available for proprietary reasons
popMeans = data.frame('lotSize1'=0.70898472596585804, 
                      'lotSize2'=0.24307534334488512,
                      'value'=12.12629725962358229, 
                      'propTax'=8.09367937048534181)

# fit model to all sample (ACS) data
  # predict number of people in household
lm_np = lm(outcome_np~lotSize1+lotSize2+valueImp+propTaxImp, data=sampleData)
summary(lm_np)

  # predict household income
lm_inc = lm(outcome_inc~lotSize1+lotSize2+valueImp+propTaxImp, data=sampleData)
summary(lm_inc)

### Apply new estimator using OLS
N = 930+77910
n = 930

## Number of people
print('NUMBER OF PEOPLE')

X = cbind(rep(1,n), sampleData$lotSize1, sampleData$lotSize2, sampleData$valueImp, sampleData$propTaxImp)
Y = sampleData$outcome_np
betaHat = as.matrix(lm_np$coefficients, ncol=1)
H = X %*% solve(t(X) %*% X) %*% t(X)
e = (diag(n)-H) %*% Y

t_nos = (N-n)*c(1, popMeans$lotSize1, popMeans$lotSize2, popMeans$value, popMeans$propTax)

sum = 0
y_f = c()
for(i in 1:n){
  betaHat_noi = betaHat - (solve(t(X) %*% X) %*% X[i,] * e[i])/(1-H[i,i])
  sum = sum +N*Y[i] + (t_nos - (N-n)*X[i,]) %*% betaHat_noi
  y_f[i] = Y[i] - X[i,] %*% betaHat_noi
}
muHat_np = sum/(n*N)
y_f_matrix = matrix(y_f, ncol=1) %*% matrix(y_f, nrow=1)
varEst_np = (N-n)/(n^2*N) * (sum((y_f)^2) - 2/(n-1)*sum(y_f_matrix[upper.tri(y_f_matrix)]))

print(paste('Estimate for average number of individuals in a single family home:', round(muHat_np, 4)))
print(paste('With estimated variance:', round(varEst_np, 4)))

# total number of people
print(paste('This corresponds to an estimated total number of people living in single family homes:', round(N * muHat_np)))
print(paste('With estimated standard error:', round(N * sqrt(varEst_np))))

print(paste('The estimated total number of people using just the sample mean is:', round(N * mean(sampleData$outcome_np))))
print(paste('With estimated standard error:', round(N*sd(sampleData$outcome_np)/sqrt(n)*sqrt(1-n/N))))


## Household Income
print('HOUSEHOLD INCOME')

Y = sampleData$outcome_inc
betaHat = as.matrix(lm_inc$coefficients, ncol=1)
e = (diag(n)-H) %*% Y

sum = 0
y_f = c()
for(i in 1:n){
  betaHat_noi = betaHat - (solve(t(X) %*% X) %*% X[i,] * e[i])/(1-H[i,i])
  sum = sum +N*Y[i] + (t_nos - (N-n)*X[i,]) %*% betaHat_noi
  y_f[i] = Y[i] - X[i,] %*% betaHat_noi
}
muHat_inc = sum/(n*N)
y_f_matrix = matrix(y_f, ncol=1) %*% matrix(y_f, nrow=1)
varEst_inc = (N-n)/(n^2*N) * (sum((y_f)^2) - 2/(n-1)*sum(y_f_matrix[upper.tri(y_f_matrix)]))


print(paste('Estimate for average income for a single family home:', round(muHat_inc)))
print(paste('With estimated standard error:', round(sqrt(varEst_inc))))

print(paste('The average single family home income using just the sample mean is:', round(mean(sampleData$outcome_inc))))
print(paste('With estimated standard error:', round(sd(sampleData$outcome_inc)/sqrt(n)*sqrt(1-n/N))))

