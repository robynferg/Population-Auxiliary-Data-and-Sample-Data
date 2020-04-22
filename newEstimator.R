library(randomForest)

newEstimator = function(Xsample, Ysample, Xother, pred.fun = 'lm'){
  # Xsample = X variable(s) for sample -- data frame
  # Ysample = outcome for sample -- vector
  # Xother = X variable(s) for rest of the population w/ unknown Ys -- data frame with same variables as Xsample
  # pred.fun = function for predicting Y from X using sample:
    # 'lm' for OLS
    # 'rf' for random forest
    # 'mean' for sample mean
  
  # convert Xsample and Xother to data frames if vectors
  if(class(Xsample)=='numeric'){
    Xsample = data.frame('x'=matrix(Xsample, ncol=1))
  }
  if(class(Xother)=='numeric'){
    Xother = data.frame('x'=matrix(Xother, ncol=1))
  }
  
  n = nrow(Xsample) # sample size
  N = nrow(Xsample) + nrow(Xother) # population size
  
  # M = data frame with columns: 
    # xlink: index
    # Y: response (if observed)
    # s: 0/1 indicator for being in the sample
    # f: predicted leave-one-out response = f(x_i; X_s\i, Y_s\i)
  M = data.frame('xlink' = c(rownames(Xsample), rep(NA, N-n)),
                 'Y' = c(Ysample, rep(NA, N-n)),
                 's' = c(rep(1, n), rep(0, N-n)),
                 'f' = rep(NA, N))
  # G = data frame of predicted responses for each left out observation
  G = data.frame('Mrow' = rownames(M)[(n+1):N])
  
  # OLS for prediction function
  if(pred.fun=='lm'){
    OLS = lm(Ysample~., data=cbind(Ysample, Xsample))
    # beta-hat for entire sample
    betahat = OLS$coefficients 
    # beta-hat when dropping each individual observation
    betahatDropped = -1*lm.influence(OLS, do.coef=TRUE)$coefficients + matrix(rep(betahat, each=n), nrow=n)
    # f_i's
    M$f = c(rowSums(cbind(rep(1, n), Xsample) * betahatDropped), rep(NA, N-n))
    # G matrix: estimates when leaving out individual observations
    G = cbind(G, as.matrix(cbind(rep(1, N-n), Xother)) %*% t(betahatDropped))
    names(G) = c(names(G)[1], paste0('leftOutObs', names(G)[2:length(names(G))]))
  }
  
  # random forest for prediction function
  # using out-of-bag predictions/trees
  else if(pred.fun=='rf'){
    f = randomForest(Ysample~., data=cbind(Ysample, Xsample), keep.inbag=TRUE)
    M$f = c(f$predicted, rep(NA, N-n))
    predictions = predict(f, newdata=Xother, predict.all=TRUE)
    # G matrix
    fis_other = matrix(NA, nrow=N-n, ncol=n)
    for(i in 1:nrow(fis_other)){
      for(j in 1:ncol(fis_other)){
        fis_other[i,j] = mean(predictions$individual[i,f$inbag[j,]==0])
      }
    }
    G = cbind(G, fis_other)
    names(G) = c(names(G)[1], paste0('leftOutObs', 1:n))
  }
  
  # sample mean for prediction function
  else if(pred.fun=='mean'){
    M$f = c(n/(n-1)*mean(Ysample) - 1/(n-1)*Ysample, rep(NA, N-n))
    G = cbind(G, matrix(rep(n/(n-1)*mean(Ysample) - 1/(n-1)*Ysample, each=N-n), nrow=N-n, ncol=n))
    names(G) = c(names(G)[1], paste0('leftOutObs', 1:n))
  }
  
  # g_i (average f's) for observations not in s
  M$g = c(rep(NA, n), apply(G[,-which(names(G)=='Mrow')], 1, mean))
  
  # h_i: f_i if i in sample, g_i if i not in sample
  M$h = ifelse(M$s==1, M$f, M$g)
  
  # Y-hat formula
  M$Yhat = ifelse(M$s==1, M$Y + (N-n)/n * (M$Y-M$h), M$h)
  
  muHat = mean(M$Yhat)
  
  # variance estimation
  tempMatrix = as.matrix(na.omit((M$Y-M$f))) %*% t(as.matrix(na.omit(M$Y-M$f)))
  varEst = (N-n)/(n^2*N) * (sum(diag(tempMatrix)) - 2/(n-1) * sum(tempMatrix[upper.tri(tempMatrix)]))
  
  return(list('muHat' = muHat, 'M' = M, 'G' = G, 'varEst'=varEst))
}
