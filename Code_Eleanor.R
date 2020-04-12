###################
## packages
####################

## library for using the Sterling number function
library(gmp)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(spatstat)
library(dplyr)
library(data.table)
library(ggpmisc)
library(plyr)




# Model -------------------------------------------------------------------


#####################
## global parameters
####################
Nmax <- 350   # maximum number of worms in an individual
nsim <- 100   # number of simulations

###################
## functions
####################

# falling factorial
fallingfac <- function(N,n){
  if (N<=170) {
    exp( lfactorial(N) - lfactorial(N-n))
  } else { # Stirling approximation: its an equation
    #that allows you to calculate the natural logarithm for large factorials;
    exp ( (N*log(N)-N) - ((N-n)*log(N-n)-(N-n)) )
  }
}

fallingfac <- Vectorize(fallingfac, "N")


# unique items distribution (UID, from Mendelsen et al 2016)
#N(k)/N^A*Stirling approximation    # gives the probability of finding n unique items from a sample of m miracidia obtained from
# an original number N of female worms inside a host.
prob <- function(n, N, m) {
  fallingfac(N,n)/N^m*as.numeric(Stirling2(m, n))
}

# expectation of UID
expect <- function(N, m) {
  (N^m - (N-1)^m)/(N^(m-1))
}

## variance of UID
vari <- function(N, m) {
  N*(N-1)*(1-2/N)^m + N*(1-1/N)^m - N^2*(1-1/N)^(2*m)
}

## posterior approximation by sampling importance resampling
f <- function(par)
{
  n <- par[1]
  m <- par[2]
  ## mean of prior NBD
  mean <- par[3]
  ## overdispersion of prior NBD
  overdisp <- par[4]
  prior <- par[5]
  
  ## SIR algorthim 
 
  
  # 1. Simulate the true number of genotypes within each individual (sample a bunch of values (nsim) from a uniform dist with min = n worms and max= 350 worms)
  
  Nstar <- round(runif(nsim, n, Nmax)) 
  
  # 2. Calculate the probability of each n given Nstar and m (using the unique items distribution)
  
  if (prior==1) { # Negative binomial prior
    
    w <- prob(n, Nstar, m)*dnbinom(Nstar, mu=mean, size=overdisp)/   
      dunif(Nstar, n, Nmax)  
  }
  else {  #Uniform prior        
    
    w <- prob(n, Nstar, m)/    
      dunif(Nstar, n, Nmax)     
  }                             
  
  # 3. resample Nstar given its probability of occuring
  samp <- (sample(Nstar, size=nsim, prob=w, replace=T))
  
  
  df <- data.frame(expectN=mean(samp), bias=n-mean(samp),
                   varN = var(samp), percentbias = (n-mean(samp))/mean(samp),
                   mn = m/n,
                   lwr = quantile(samp, probs=c(0.025)), 
                   upr = quantile(samp, probs=c(0.975)))
  return(df)
  
}





########################################################
## 2. explore posterior of N for different n, m and priors
########################################################


# Hypothetical dataset ----------------------------------------------------

s_<- 50  # number of samples from uniform distribution
min_<- 1 # minimum number of miracidia sampled
max_<- 20 #maximum number of miracidia sampled


m<- round(runif(s_,min_, max_)) # m - hypothetical number of miracidia sampled


n<- round(runif(s_, min_, m))  # n - hypothetical number of unique genotypes found (n has to be lower or equal to m )


data<- data.frame(m,n)

# Functio to run the model  ----------------------------------------------------------------

df <- data.frame(n=data$n, m=data$m, mean=0, overdisp=0, prior=0)  # using a weakly informative uniform prior



tmp <- vector("list", nrow(df))
for (i in 1:nrow(df)) {
  if (df[i,1]>df[i,2]) {
    tmp[[i]] <- NA
  } else {
    tmp[[i]] <- f(as.numeric(df[i,]))  #runs SIR algorithm
  }
}

df <- cbind(df, do.call(rbind, tmp))



# Outputs - Plots ---------------------------------------------------------


plot1<- ggplot(data=df, aes(y=expectN, x=n, col=as.numeric(m), group=m)) +
  geom_errorbar(aes(ymin=df$lwr, ymax=df$upr), 
                size=0.4, alpha=1, width=0, position=position_dodge(width=0.6))+
  geom_point(alpha=1, cex=1, position=position_dodge(width=0.6)) +
  scale_color_gradientn(name="m", colours = c("#9ecae1", "#6baed6", "#3182bd", "#08519c", "#08306b"))+
  scale_y_continuous(name="N", limits = c(0,350)) +
  theme_minimal()

plot1

