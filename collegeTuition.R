### Apply new method to college tuition data
## Predict mean tuition for 2017-18 school year
## based on covariates from the 2016-2017 school year

library(ggplot2)

## read in college data
collegeData = read.csv('collegeData.csv')

## read in estimator function
source('newEstimator.R')

## apply new method and regression imputation for a simple random sample 
set.seed(1234)

nsims = 10000
ns = seq(100, 1000, by=25)
outcomes = data.frame()
summaryStats = data.frame()

for(n in ns){
  temp = data.frame()
  for(i in 1:nsims){
    # take sample
    samp = sample(1:nrow(collegeData), n, replace=FALSE)
    # apply new estimator
    new = newEstimator(Xsample = collegeData[samp,-c(1:4)], Ysample = collegeData$tuition17[samp],
                       Xother = collegeData[-samp, -c(1:4)], pred.fun = 'lm')
    # regression imputation
    reg = lm(tuition17~., data=collegeData[samp, -c(1:2,4)])
    regImp = mean(c(predict(reg, collegeData[-samp, -c(1:4)]), collegeData$tuition17[samp]))
    # sample mean
    sampMean = mean(collegeData$tuition17[samp])
    # save outcomes
    temp = rbind(temp, data.frame('n'=n, 'new'=new$muHat, 'newVarEst'=new$varEst,
                                  'regImp'=regImp, 'sampMean'=sampMean))
  }
  # save summary statistics
  summaryStats = rbind(summaryStats, data.frame('n'=n, 'newMean'=mean(temp$new), 'newSE'=sd(temp$new), 'newVarEst'=mean(temp$newVarEst),
                       'regImpMean'=mean(temp$regImp), 'regImpSE'=sd(temp$regImp),
                       'sampMeanMean'=mean(temp$sampMean), 'sampMeanSE'=sd(temp$sampMean)))
  outcomes = rbind(outcomes, temp)
  #print(n)
}


## plot results
  # bias for new method and imputation
mu = mean(collegeData$tuition17)

summaryStats$newBias = mu - summaryStats$newMean
summaryStats$regImpBias = mu - summaryStats$regImpMean
summaryStats$sampMeanBias = mu - summaryStats$sampMeanMean

summaryStats.gg = data.frame('n'=summaryStats$n, 'Bias'=summaryStats$newBias, 
                             'UpperCI'=summaryStats$newBias+2*summaryStats$newSE/sqrt(nsims),
                             'LowerCI'=summaryStats$newBias-2*summaryStats$newSE/sqrt(nsims),
                             'Method'='New')
summaryStats.gg = rbind(summaryStats.gg, data.frame('n'=summaryStats$n, 'Bias'=summaryStats$regImpBias,
                                                    'UpperCI'=summaryStats$regImpBias+2*summaryStats$regImpSE/sqrt(nsims),
                                                    'LowerCI'=summaryStats$regImpBias-2*summaryStats$regImpSE/sqrt(nsims),
                                                    'Method'='Imputation'))
pdf('college_SRSbias.pdf')
ggplot(summaryStats.gg, aes(n, Bias, colour=Method)) + 
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI), fill='grey80', alpha=.3) + geom_hline(yintercept=0) +
  geom_point(aes(colour=Method)) + geom_line() + theme_bw() + scale_colour_grey()
dev.off()

  # SE comparison for new method and sample mean
miny = with(summaryStats, min(newSE, regImpSE, sampMeanSE))
maxy = with(summaryStats, max(newSE, regImpSE, sampMeanSE))
pdf('college_SRSse.pdf')
plot(summaryStats$n, summaryStats$newSE, ylim=c(miny, maxy), pch=19, 
     main='Simulated Standard Error', xlab='Sample Size', ylab='SE')
points(summaryStats$n, summaryStats$sampMeanSE, col='grey', pch=19)
legend('topright', pch=c(19,19), col=c('black', 'grey'), legend=c('New method', 'sample mean'))
dev.off()

  # MSE
mu = mean(collegeData$tuition17)
summaryStats$newMSE = NA
summaryStats$regImpMSE = NA
summaryStats$sampMeanMSE = NA
for(n in summaryStats$n){
  nsubset = outcomes[outcomes$n==n,]
  summaryStats$newMSE[which(summaryStats$n==n)] = 1/nsims * sum((nsubset$new-mu)^2)
  summaryStats$regImpMSE[which(summaryStats$n==n)] = 1/nsims * sum((nsubset$regImp-mu)^2)
  summaryStats$sampMeanMSE[which(summaryStats$n==n)] = 1/nsims * sum((nsubset$sampMean-mu)^2)
}

pdf('college_SRSmse.pdf')
plot(summaryStats$n, summaryStats$regImpMSE - summaryStats$newMSE, pch=19,
     main="Difference in MSE between New Method and Regression Imputation", 
     xlab="Sample Size", ylab="MSE(Regression Imputation) - MSE(New)")
dev.off()



##### Non-probability sample
set.seed(2345)

nsims = 10000
ns = seq(100, 1000, by=100)
outcomes.nonprob = data.frame()
summaryStats.nonprob = data.frame()

# sampling probabilities: college with highest tuition 2x as likely to be chosen
lm.prob = lm(c(1,2)~c(min(collegeData$tuition16), max(collegeData$tuition16)))
samp.probs = lm.prob$coef[1] + lm.prob$coef[2]*collegeData$tuition16

for(n in ns){
  temp = data.frame()
  for(i in 1:nsims){
    # take sample
    samp = sample(1:nrow(collegeData), n, replace=FALSE, prob=samp.probs)
    # apply new estimator
    new = newEstimator(Xsample = collegeData[samp,-c(1:4)], Ysample = collegeData$tuition17[samp],
                       Xother = collegeData[-samp, -c(1:4)], pred.fun = 'lm')
    # regression imputation
    reg = lm(tuition17~., data=collegeData[samp, -c(1:2,4)])
    regImp = mean(c(predict(reg, collegeData[-samp, -c(1:4)]), collegeData$tuition17[samp]))
    # sample mean
    sampMean = mean(collegeData$tuition17[samp])
    # save outcomes
    temp = rbind(temp, data.frame('n'=n, 'new'=new$muHat, 'newVarEst'=new$varEst,
                                  'regImp'=regImp, 'sampMean'=sampMean))
  }
  # save summary statistics
  summaryStats.nonprob = rbind(summaryStats.nonprob, data.frame('n'=n, 'newMean'=mean(temp$new), 'newSE'=sd(temp$new), 'newVarEst'=mean(temp$newVarEst),
                                                'regImpMean'=mean(temp$regImp), 'regImpSE'=sd(temp$regImp),
                                                'sampMeanMean'=mean(temp$sampMean), 'sampMeanSE'=sd(temp$sampMean)))
  outcomes.nonprob = rbind(outcomes.nonprob, temp)
  #print(n)
}

# table of bias
print(with(summaryStats.nonprob, round(data.frame('n'=n, 'newLowerCI'=newMean-2*newSE/sqrt(nsims)-mean(collegeData$tuition17), 
                                 'newUpperCI'=newMean+2*newSE/sqrt(nsims)-mean(collegeData$tuition17),
                                 'regImpLowerCI'=regImpMean-2*regImpSE/sqrt(nsims)-mean(collegeData$tuition17),
                                 'regImpUpperCI'=regImpMean+2*regImpSE/sqrt(nsims)-mean(collegeData$tuition17),
                                 'meanLowerCI'=sampMeanMean-2*sampMeanSE/sqrt(nsims)-mean(collegeData$tuition17),
                                 'meanUpperCI'=sampMeanMean+2*sampMeanSE/sqrt(nsims)-mean(collegeData$tuition17)), 2)))

