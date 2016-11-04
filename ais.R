beta.check<- function(beta){
  if (!beta[1] == 1){
    stop("first element of beta should be equal to 1")
  }
  
  if (!beta[length(beta)] == 0){
    stop("last element of beta should be equal to 0")
  }
}

logf.gen<- function(beta, logf_0, logf_n){
  # beta: a seqence of length n + 1 from 0 to 1 where 1 = beta_{0} > beta_{1} > ... > beta_{n} = 0
  # logf_0: log density of target distribution
  # logf_n: log density of an samplable distribution

  beta.check(beta)
  
  function(j){
    function(x){
      beta[j+1]*logf_0(x) + (1 - beta[j+1])*logf_n(x)
    }
  }
}

ais.sampler<- function(beta, logf, trans, rprior){
  # trans: transitional function
  # prior: a sampling algorithm generate initial value
  beta.check(beta)
  n<- length(beta) - 1
  
  ## generate x_{n-1}
  x<- rprior()
  logw<- logf(n-1)(x) - logf(n)(x)
  
  ## from n-2 to 0
  for(i in (n-2):0){
    ## generate x_{i}
    x<- trans(x, logf(i+1))
    
    ## update logw
    logw<- logw + logf(i)(x) - logf(i + 1)(x)
  }
  
  logw
}

ais<- function(k, beta, logf, trans, rprior){
  logw<- replicate(k, ais.sampler(beta, logf = logf, trans = trans, rprior = rprior))
  logw
}