#################################################
### Simulations of estimation method presented
### in 'Unbaised Survey Estimation with Population Auxiliary Variables'
### by Ferg, Gagnon-Bartsch
#################################################

library(randomForest)


#################################################
########## Functions ############################
#################################################

## Read in function for new estimator
source('newEstimator.R')

## Function for calculating bias, variance, etc. for population based on simulated samples
simulateFunction = function(Xpopulation, Ypopulation, Ns, ns, nsims, methods, verbose=TRUE){
  outcomes = data.frame() # data frame of each simulation
  summaryStats = data.frame() # data frame of summary stats for each N, n combination
  for(N in Ns){ # iterate through population sizes
    # population of size N
    XN = Xpopulation[1:N,] 
    YN = Ypopulation[1:N]
    muN = mean(YN)
    for(n in ns[ns<N]){ # iterate through sample sizes
      temp = data.frame() # data frame of simulations for n, N
      for(i in 1:nsims){ # nsims simulated samples
        # sample from population
        samp = sample(1:N, n, replace=FALSE)
        X.samp = data.frame(as.matrix(XN)[samp,])
        Xother = data.frame(as.matrix(XN)[-samp,])
        Y.samp = YN[samp]
        # apply various estimation methods
        newOLS=NA; newVarEstOLS=NA; newRF=NA; newVarEstRF=NA; 
        sampMean=NA; sampMeanVarEst=NA; regImp=NA; regImpEstVar=NA; rfImp=NA; rfImpEstVar=NA
        if('newOLS' %in% methods){
          newOLS_est = newEstimator(Xsample=X.samp, Ysample=Y.samp, Xother=Xother, pred.fun='lm')
          newOLS = newOLS_est$muHat
          newVarEstOLS = newOLS_est$varEst
        }
        if('newRF' %in% methods){
          newRF_est = newEstimator(Xsample=X.samp, Ysample=Y.samp, Xother=Xother, pred.fun='rf')
          newRF = newRF_est$muHat
          newVarEstRF = newRF_est$varEst
        }
        if('sampMean' %in% methods){
          sampMean = mean(Y.samp)
          sampMeanVarEst = (N-n)/(n*N) * 1/(n-1) * sum((Y.samp-sampMean)^2)
        }
        if('OLSimp' %in% methods){
          dataN = data.frame(cbind(XN, 'Y'=YN))
          lm = lm(Y~., data=dataN[samp,])
          regImp = mean(c(Y.samp, predict(lm, dataN[-samp,])))
          regImpEstVar = (N-n)/(n*N) * 1/(n-2) * sum(lm$residuals^2)
        }
        if('RFimp' %in% methods){
          dataN = data.frame(XN, 'Y'=YN)
          rfmodel = randomForest(Y~., data=dataN[samp,])
          rfImp = mean(c(Y.samp, predict(rfmodel, dataN[-samp,])))
          rfImpEstVar = (N-n)/(n*N) * 1/(n-2) * sum((dataN$Y[samp]-predict(rfmodel, dataN[samp,]))^2)
        }
        # attach to temp df
        temp = rbind(temp, data.frame('N'=N, 'n'=n, 'newOLS'=newOLS, 'newVarEstOLS'=newVarEstOLS,
                     'newRF'=newRF, 'newVarEstRF'=newVarEstRF, 'sampMean'=sampMean, 'sampMeanVarEst'=sampMeanVarEst,
                     'OLSimp'=regImp, 'OLSimpVarEst'=regImpEstVar, 
                     'RFimp'=rfImp, 'RFimpVarEst'=rfImpEstVar))
      }
      # attach to outcomes data frame
      outcomes = rbind(outcomes, temp)
      # get summary statistics
      newOLSmean=NA; newOLSbias=NA; newOLSvar=NA; newOLSvarEst=NA; newOLSmse=NA
      if('newOLS' %in% methods){
        newOLSmean = mean(temp$newOLS)
        newOLSbias = muN-mean(temp$newOLS)
        newOLSvar = var(temp$newOLS)
        newOLSvarEst = mean(temp$newVarEstOLS)
        newOLSmse = mean((muN-temp$newOLS)^2)
      }
      newRFmean=NA; newRFbias=NA; newRFvar=NA; newRFvarEst=NA; newRFmse=NA
      if('newRF' %in% methods){
        newRFmean = mean(temp$newRF)
        newRFbias = muN-mean(temp$newRF)
        newRFvar = var(temp$newRF)
        newRFvarEst = mean(temp$newVarEstRF)
        newRFmse = mean((muN-temp$newRF)^2)
      }
      sampMeanMean=NA; sampMeanBias=NA; sampMeanVar=NA; sampMeanVarEst=NA; sampMeanMSE=NA
      if('sampMean' %in% methods){
        sampMeanMean = mean(temp$sampMean)
        sampMeanBias = muN-mean(temp$sampMean)
        sampMeanVar = var(temp$sampMean)
        sampMeanVarEst = mean(temp$sampMeanVarEst)
        sampMeanMSE = mean((muN-temp$sampMean)^2)
      }
      OLSimpMean=NA; OLSimpBias=NA; OLSimpVar=NA; OLSimpVarEst=NA; OLSimpMSE=NA
      if('OLSimp' %in% methods){
        OLSimpMean = mean(temp$OLSimp)
        OLSimpBias = muN-mean(temp$OLSimp)
        OLSimpVar = var(temp$OLSimp)
        OLSimpVarEst = mean(temp$OLSimpVarEst)
        OLSimpMSE = mean((muN-temp$OLSimp)^2)
      }
      RFimpMean=NA; RFimpBias=NA; RFimpVar=NA; RFimpVarEst=NA; RFimpMSE=NA
      if('RFimp' %in% methods){
        RFimpMean = mean(temp$RFimp)
        RFimpBias = muN-mean(temp$RFimp)
        RFimpVar = var(temp$RFimp)
        RFimpVarEst = mean(temp$RFimpVarEst)
        RFimpMSE = mean((muN-temp$RFimp)^2)
      }
      tempSummaryStats = data.frame('N'=N, 'n'=n, 'newOLSmean'=newOLSmean, 'newOLSbias'=newOLSbias, 'newOLSvar'=newOLSvar, 
                                    'newOLSvarEst'=newOLSvarEst, 'newOLSmse'=newOLSmse,
                                    'newRFmean'=newRFmean, 'newRFbias'=newRFbias, 'newRFvar'=newRFvar, 
                                    'newRFvarEst'=newRFvarEst, 'newRFmse'=newRFmse,
                                    'sampMeanMean'=sampMeanMean, 'sampMeanBias'=sampMeanBias, 'sampMeanVar'=sampMeanVar,
                                    'sampMeanVarEst'=sampMeanVarEst, 'sampMeanMSE'=sampMeanMSE,
                                    'OLSimpMean'=OLSimpMean, 'OLSimpBias'=OLSimpBias, 'OLSimpVar'=OLSimpVar, 
                                    'OLSimpVarEst'=OLSimpVarEst, 'OLSimpMSE'=OLSimpMSE,
                                    'RFimpMean'=RFimpMean, 'RFimpBias'=RFimpBias, 'RFimpVar'=RFimpVar, 
                                    'RFimpVarEst'=RFimpVarEst, 'RFimpMSE'=RFimpMSE)
      summaryStats = rbind(summaryStats, tempSummaryStats)
      if(verbose) print(paste0('N=', N, ', n=', n))
    }
  }
  return(list('summaryStats'=summaryStats, 'allSims'=outcomes))
}


#################################################
########## Simulations ##########################
#################################################

#### Linear
print('LINEAR POPULATION')
set.seed(1)

Ns.lin = c(50, 500, 10000)
ns.lin = c(10, 100, 1000)

nsims.lin = 10000

X.lin = matrix(rnorm(max(Ns.lin)), ncol=1)
Y.lin = 3 + 2*X.lin[,1] + rnorm(max(Ns.lin))

linear = simulateFunction(X.lin, Y.lin, Ns.lin, ns.lin, nsims.lin, c('newOLS', 'OLSimp', 'sampMean'), verbose=FALSE)

print('New method with OLS')
with(linear$summaryStats, 
     print(round(cbind(N, n, 'trueSE'=sqrt(newOLSvar), 'estSE'=sqrt(newOLSvarEst), newOLSbias, 't'=newOLSbias/(sqrt(newOLSvar/nsims.lin))),4)))
print('OLS adjustment')
with(linear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(OLSimpVar), 'estSE'=sqrt(OLSimpVarEst), OLSimpBias, 't'=OLSimpBias/(sqrt(OLSimpVar/nsims.lin))), 4)))
print('Sample mean')
with(linear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(sampMeanVar), 'estSE'=sqrt(sampMeanVarEst), sampMeanBias, 't'=sampMeanBias/(sqrt(sampMeanVar/nsims.lin))), 4)))

#save.image('newMethodSimulationsNewENVIRONMENT.Rdata')

#### Slighlty non-linear
print('LESS LINEAR POPULATION')
set.seed(123)

Ns.lesslin = c(500)
ns.lesslin = c(25, 50, 100)

nsims.lesslin = 10000

X.lesslin = matrix(rnorm(max(Ns.lesslin)), ncol=1)
Y.lesslin = ifelse(X.lesslin[,1]< -0.5, 0, 2*X.lesslin+.5) + rnorm(max(Ns.lesslin))

plot(X.lesslin, Y.lesslin, pch=19, xlab="X", ylab="Y")

lesslinear = simulateFunction(X.lesslin, Y.lesslin, Ns.lesslin, ns.lesslin, nsims.lesslin, c('newOLS', 'OLSimp', 'sampMean'), verbose=FALSE)

print('New method with OLS')
with(lesslinear$summaryStats, 
     print(round(cbind(N, n, 'trueSE'=sqrt(newOLSvar), 'estSE'=sqrt(newOLSvarEst), newOLSbias, 't'=newOLSbias/(sqrt(newOLSvar/nsims.lesslin))),4)))
print('OLS adjustment')
with(lesslinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(OLSimpVar), 'estSE'=sqrt(OLSimpVarEst), OLSimpBias, 't'=OLSimpBias/(sqrt(OLSimpVar/nsims.lesslin))), 4)))
print('Sample mean')
with(lesslinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(sampMeanVar), 'estSE'=sqrt(sampMeanVarEst), sampMeanBias, 't'=sampMeanBias/(sqrt(sampMeanVar/nsims.lesslin))), 4)))

#save.image('newMethodSimulationsNewENVIRONMENT.Rdata')

#### Non-linear
print('NONLINEAR POPULATION')
set.seed(50)

Ns.nonlin = 500
ns.nonlin = c(25, 50, 100)

nVars = 20
X.nonlin = matrix(rnorm(nVars*max(Ns.nonlin)), ncol=nVars)
Y.nonlin = X.nonlin[,1]^3 + 2*sqrt(abs(X.nonlin[,2])) + X.nonlin[,1]*X.nonlin[,2] + 
  sin(X.nonlin[,3]) + rnorm(max(Ns.nonlin))

nsims.nonlin = 10000

nonlinear = simulateFunction(X.nonlin, Y.nonlin, Ns.nonlin, ns.nonlin, nsims.nonlin, 
                             c('newRF', 'RFimp', 'newOLS', 'OLSimp', 'sampMean'), verbose=FALSE)

print('New method with RF')
with(nonlinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(newRFvar), 'estSE'=sqrt(newRFvarEst), newRFbias, 't'=newRFbias/sqrt(newRFvar/nsims.nonlin)), 4)))
print('RF adjustment')
with(nonlinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(RFimpVar), 'estSE'=sqrt(RFimpVarEst), RFimpBias, 't'=RFimpBias/sqrt(RFimpVar/nsims.nonlin)), 4)))
print('New method with OLS')
with(nonlinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(newOLSvar), 'estSE'=sqrt(newOLSvarEst), newOLSbias, 't'=newOLSbias/sqrt(newOLSvar/nsims.nonlin)), 4)))
print('OLS adjustment')
with(nonlinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(OLSimpVar), 'estSE'=sqrt(OLSimpVarEst), OLSimpBias, 't'=OLSimpBias/sqrt(OLSimpVar/nsims.nonlin)), 4)))
print('Sample mean')
with(nonlinear$summaryStats,
     print(round(cbind(N, n, 'trueSE'=sqrt(sampMeanVar), 'estSE'=sqrt(sampMeanVarEst), sampMeanBias, 't'=sampMeanBias/(sqrt(sampMeanVar/nsims.nonlin))),4)))

#save.image('newMethodSimulationsNewENVIRONMENT.Rdata')

