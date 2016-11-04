## True value: 0.2506628
## p_n: N(1, 1)

source(AIS.R)
## beta setting
beta<- rev(c(0, sort(unique(runif(40 - 1, 0, 1))), 1))

## f setting
logf_0<-function(x){
  -0.5 * (x-1)^2/(0.1^2)
}

## f_n setting (其實就是Prior啦！)
logf_n<- function(x){
  dnorm(x, 0, 1, log = T)
}

## transition
trans<- function(x, logd.target){
  # x: current state
  # logf: target distribution
  
  x.prop<- x + rnorm(1, 0, 1)
  
  loga<- logd.target(x.prop) - logd.target(x)
  
  if (log(runif(1)) < loga){
    x<- x.prop
  }
  x
}

## AIS
logf = logf.gen(beta, logf_0, logf_n)
logw<- ais(1000, beta, logf = logf, trans = trans, rprior = function(x) rnorm(1, 0, 1))
mean(exp(logw))