### Apply new method to college tuition data
## Predict mean tuition for 2017-18 school year
## based on covariates from the 2016-2017 school year

library(randomForest)

## read in college data
collegeData = read.csv('collegeData.csv')

## read in estimator function
source('newEstimator.R')

#### apply our new method with OLS and random forest, standard OLS and random forest adjustment, and sample mean 
### for a simple random sample 
set.seed(1234)

nsims = 10000
ns = seq(100, 1000, by=100)
summaryStats = data.frame()

vars = c("admRate", "gradRate", "studStaffRatio", "doctoral", "master", "public")
mu = mean(collegeData$tuition17)
N = nrow(collegeData)

for(n in ns){
  temp = data.frame()
  for(i in 1:nsims){
    # take sample
    samp = sample(1:nrow(collegeData), n, replace=FALSE)
    # apply our estimator
    # with OLS
    newOLS = newEstimator(Xsample = collegeData[samp, vars], Ysample = collegeData$tuition17[samp],
                          Xother = collegeData[-samp, vars], pred.fun = 'lm')
    # with random forest
    newRF = newEstimator(Xsample = collegeData[samp, vars], Ysample = collegeData$tuition17[samp],
                         Xother = collegeData[-samp, vars], pred.fun = 'rf')
    # regression adjustment
    reg = lm(tuition17~., data=collegeData[samp, c(vars, 'tuition17')])
    regImp = mean(c(predict(reg, collegeData[-samp, vars]), collegeData$tuition17[samp]))
    regImpSdEst = sqrt((N-n)/(n*N*(n-2)) * sum(reg$residuals^2))
    # random forest adjustment
    rf = randomForest(tuition17~., data=collegeData[samp, c(vars, 'tuition17')])
    rfImp = mean(c(predict(rf, collegeData[-samp, vars]), collegeData$tuition17[samp]))
    rfImpSdEst = sqrt((N-n)/(n*N*(n-2)) * sum((collegeData$tuition17[samp]-rf$predicted)^2))
    # sample mean
    sampMean = mean(collegeData$tuition17[samp])
    sampMeanEstSd = (N-n)/(n*N*(n-1)) * sum((collegeData$tuition17[samp]-sampMean)^2)
    # save outcomes
    temp = rbind(temp, data.frame('n'=n, 'newOLS'=newOLS$muHat, 'newOLSVarEst'=newOLS$varEst,
                                  'newRF'=newRF$muHat, 'newRFVarEst'=newRF$varEst,
                                  'regImp'=regImp, 'regImpSdEst'=regImpSdEst,
                                  'rfImp'=rfImp, 'rfImpSdEst'=rfImpSdEst, 
                                  'sampMean'=sampMean, 'sampMeanSdEst'=sampMeanEstSd))
  }
  # save summary statistics
  summary = data.frame('n'=n, 'newMeanOLS'=mean(temp$newOLS), 'newSEOLS'=sd(temp$newOLS), 
                       'newVarEstOLS'=mean(temp$newOLSVarEst), 'newMSEOLS'=mean((temp$newOLS-mu)^2),
                       'newMeanRF'=mean(temp$newRF), 'newSERF'=sd(temp$newRF), 
                       'newVarEstRF'=mean(temp$newRFVarEst), 'newMSERF'=mean((temp$newRF-mu)^2),
                       'regImpMean'=mean(temp$regImp), 'regImpSE'=sd(temp$regImp), 
                       'regImpSdEst'=mean(temp$regImpSdEst), 'regImpMSE'=mean((temp$regImp-mu)^2),
                       'rfImpMean'=mean(temp$rfImp), 'rfImpSE'=sd(temp$rfImp),
                       'rfImpSdEst'=mean(temp$rfImpSdEst), 'rfImpMSE'=mean((temp$rfImp-mu)^2),
                       'sampMeanMean'=mean(temp$sampMean), 'sampMeanSE'=sd(temp$sampMean), 
                       'sampMeanSdEst'=mean(temp$sampMeanSdEst), 'sampMeanMSE'=mean((temp$sampMean-mu)^2))
  summaryStats = rbind(summaryStats, summary)
  #print(n)
}


## results of SRS
srs_table = with(summaryStats, cbind(n, 'sampMean_TrueSE'=sampMeanSE, 'sampMean_EstSE'=sqrt(sampMeanSdEst), 
                                     'sampMean_Bias'=mu-sampMeanMean, 'sampMean_t'=(mu-sampMeanMean)/(sampMeanSE/sqrt(nsims)),
                                     'OLSadj_TrueSE'=regImpSE, 'OLSadj_EstSE'=regImpSdEst,
                                     'OLSadj_Bias'=mu-regImpMean, 'OLSadj_t'=(mu-regImpMean)/(regImpSE/sqrt(nsims)),
                                     'NewOLS_TrueSE'=newSEOLS, 'NewOLS_EstSE'=sqrt(newVarEstOLS),
                                     'NewOLS_Bias'=mu-newMeanOLS, 'NewOLS_t'=(mu-newMeanOLS)/(newSEOLS/sqrt(nsims)),
                                     'RFadj_TrueSE'=rfImpSE, 'RFadj_EstSE'=rfImpSdEst,
                                     'RFadj_Bias'=mu-rfImpMean, 'RFadj_t'=(mu-rfImpMean)/(rfImpSE/sqrt(nsims)),
                                     'NewRF_TrueSE'=newSERF, 'NewRF_EstSE'=sqrt(newVarEstRF),
                                     'NewRF_Bias'=mu-newMeanRF, 'NewRF_t'=(mu-newMeanRF)/(newSERF/sqrt(nsims))))
print(round(srs_table, 2))


## Appendix table
srs_appendix = with(summaryStats, cbind(n,'newOLSSE'=newSEOLS, 'newOLSbias'=mu-newMeanOLS, 
                                        'adjOLSSE'=regImpSE, 'adjOLSbias'=mu-regImpMean,
                                        'nweRFSE'=newSERF, 'newRFbias'=mu-newMeanRF, 
                                        'adjRFSE'=rfImpSE, 'adjRFbias'=mu-rfImpMean,
                                        'sampMeanSE'=sampMeanSE, 'sampMeanBias'=mu-sampMeanMean))
print(round(srs_appendix, 2))


### Non-probability sample
set.seed(2345)

nsims = 10000
ns = seq(100, 1000, by=100)
summaryStats.nonprob = data.frame()

# sampling probabilities: college with highest tuition 2x as likely to be chosen
lm.prob = lm(c(1,2)~c(min(collegeData$tuition16), max(collegeData$tuition16)))
samp.probs = lm.prob$coef[1] + lm.prob$coef[2]*collegeData$tuition16


for(n in ns){
  temp = data.frame()
  for(i in 1:nsims){
    # take sample
    samp = sample(1:nrow(collegeData), n, replace=FALSE, prob=samp.probs)
    # apply our estimator
    # with OLS
    newOLS = newEstimator(Xsample = collegeData[samp, vars], Ysample = collegeData$tuition17[samp],
                          Xother = collegeData[-samp, vars], pred.fun = 'lm')
    # with random forest
    newRF = newEstimator(Xsample = collegeData[samp, vars], Ysample = collegeData$tuition17[samp],
                         Xother = collegeData[-samp, vars], pred.fun = 'rf')
    # regression adjustment
    reg = lm(tuition17~., data=collegeData[samp, c(vars, 'tuition17')])
    regImp = mean(c(predict(reg, collegeData[-samp, vars]), collegeData$tuition17[samp]))
    regImpSdEst = sqrt((N-n)/(n*N*(n-2)) * sum(reg$residuals^2))
    # random forest adjustment
    rf = randomForest(tuition17~., data=collegeData[samp, c(vars, 'tuition17')])
    rfImp = mean(c(predict(rf, collegeData[-samp, vars]), collegeData$tuition17[samp]))
    rfImpSdEst = sqrt((N-n)/(n*N*(n-2)) * sum((collegeData$tuition17[samp]-rf$predicted)^2))
    # sample mean
    sampMean = mean(collegeData$tuition17[samp])
    sampMeanEstSd = (N-n)/(n*N*(n-1)) * sum((collegeData$tuition17[samp]-sampMean)^2)
    # save outcomes
    temp = rbind(temp, data.frame('n'=n, 'newOLS'=newOLS$muHat, 'newOLSVarEst'=newOLS$varEst,
                                  'newRF'=newRF$muHat, 'newRFVarEst'=newRF$varEst,
                                  'regImp'=regImp, 'regImpSdEst'=regImpSdEst,
                                  'rfImp'=rfImp, 'rfImpSdEst'=rfImpSdEst, 
                                  'sampMean'=sampMean, 'sampMeanSdEst'=sampMeanEstSd))
  }
  # save summary statistics
  summary = data.frame('n'=n, 'newMeanOLS'=mean(temp$newOLS), 'newSEOLS'=sd(temp$newOLS), 
                       'newVarEstOLS'=mean(temp$newOLSVarEst), 'newMSEOLS'=mean((temp$newOLS-mu)^2),
                       'newMeanRF'=mean(temp$newRF), 'newSERF'=sd(temp$newRF), 
                       'newVarEstRF'=mean(temp$newRFVarEst), 'newMSERF'=mean((temp$newRF-mu)^2),
                       'regImpMean'=mean(temp$regImp), 'regImpSE'=sd(temp$regImp), 
                       'regImpSdEst'=mean(temp$regImpSdEst), 'regImpMSE'=mean((temp$regImp-mu)^2),
                       'rfImpMean'=mean(temp$rfImp), 'rfImpSE'=sd(temp$rfImp),
                       'rfImpSdEst'=mean(temp$rfImpSdEst), 'rfImpMSE'=mean((temp$rfImp-mu)^2),
                       'sampMeanMean'=mean(temp$sampMean), 'sampMeanSE'=sd(temp$sampMean), 
                       'sampMeanSdEst'=mean(temp$sampMeanSdEst), 'sampMeanMSE'=mean((temp$sampMean-mu)^2))
  summaryStats.nonprob = rbind(summaryStats.nonprob, summary)
  #print(n)
}


## results of non-SRS
nonsrs_table = with(summaryStats.nonprob, cbind(n, 'sampMean_TrueSE'=sampMeanSE, 'sampMean_EstSE'=sqrt(sampMeanSdEst), 
                                                'sampMean_Bias'=mu-sampMeanMean, 'sampMean_t'=(mu-sampMeanMean)/(sampMeanSE/sqrt(nsims)),
                                                'OLSadj_TrueSE'=regImpSE, 'OLSadj_EstSE'=regImpSdEst,
                                                'OLSadj_Bias'=mu-regImpMean, 'OLSadj_t'=(mu-regImpMean)/(regImpSE/sqrt(nsims)),
                                                'NewOLS_TrueSE'=newSEOLS, 'NewOLS_EstSE'=sqrt(newVarEstOLS),
                                                'NewOLS_Bias'=mu-newMeanOLS, 'NewOLS_t'=(mu-newMeanOLS)/(newSEOLS/sqrt(nsims)),
                                                'RFadj_TrueSE'=rfImpSE, 'RFadj_EstSE'=rfImpSdEst,
                                                'RFadj_Bias'=mu-rfImpMean, 'RFadj_t'=(mu-rfImpMean)/(rfImpSE/sqrt(nsims)),
                                                'NewRF_TrueSE'=newSERF, 'NewRF_EstSE'=sqrt(newVarEstRF),
                                                'NewRF_Bias'=mu-newMeanRF, 'NewRF_t'=(mu-newMeanRF)/(newSERF/sqrt(nsims))))
print(round(nonsrs_table, 2))


## Appendix table
nonsrs.nonprob = with(summaryStats.nonprob, cbind(n,'newOLSSE'=newSEOLS, 'newOLSbias'=mu-newMeanOLS, 
                                                  'adjOLSSE'=regImpSE,'adjOLSbias'=mu-regImpMean, 
                                                  'nweRFSE'=newSERF,'newRFbias'=mu-newMeanRF, 
                                                  'adjRFSE'=rfImpSE,'adjRFbias'=mu-rfImpMean, 
                                                  'sampMeanSE'=sampMeanSE, 'sampMeanbias'=mu-sampMeanMean))
print(round(nonsrs.nonprob, 2))



#### OLS using all data
allOLS = lm(tuition17~admRate + gradRate + studStaffRatio + doctoral + master + public, data = collegeData)
plot(allOLS)

# outliers: Brigham Young University and Inter American University of Puerto Rico
collegeData[1106:1107,]
collegeData[1233:1235,]
