#### Lukas Beinhauer 10/12/20 ####

## Simulating study data ##


library(metafor)

### The following function is designed to simulate data from multiple studies, in order to assess the impact of various kinds of variances
###  on measures of heterogeneity. Sample sizes are samples for each study, with small sample sizes being stronlgy favoured.
###
### The current implementation only allows to simulate study data comparing 2 groups (imagine one control one treatment), Sample sizes are
###  equal for each group. Number of studdies, mean-difference between group, group intercept, sd of group difference, sd of group data,
###  max and min of group sample sizes may be manipulated

SimStudies2groups <-function(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.5, mean.groupsd=2, sd.groupsd=.1, n.max=125, n.min=20){
  
  meanES <- rnorm(k.studies, mean=meandiff, sd=sd.meandiff) # sample difference between groups for each study
  
  groupsd <- rnorm(k.studies, mean=mean.groupsd, sd=sd.groupsd) # sample a standard deviation for both groups of each study
  
  n.range <- n.min:n.max # define the range of sample sizes
  
  n.sample.prob <- (1/1:length(n.range)/sum(1/1:length(n.range))) # samling probability is inverse of index of size in range.
                                                                  #  e.g. for range of 20:119, the probability of sampling 20
                                                                  #  is p(n=20)=(1/1)/sum(1/n_i), since n=20 describes the smallest
                                                                  #  possible smaple size. p(n=119)=(1/100)/sum(1/n_i), as n=119 is
                                                                  #  the largest possible size (20:119 is 100 individual integers)
  
  N <- sample(n.range, k.studies, replace = T, prob=n.sample.prob) # sampling sample sizes based on inverse probability described above
  
  # studies <- list() # prepping empty list
  
  studies <- lapply(1:k.studies, FUN = function(x) {
    data.frame(
      X1 = rnorm(N[x], mean=intercept, sd=groupsd[x]),
      X2 = rnorm(N[x], mean=intercept+meanES[x], sd=groupsd[x]))
    })
  # 
  # for(i in 1:k.studies){
  #   X1 <- rnorm(N[i], mean=intercept, sd=groupsd[i]) # sampling values for "control" goup 
  #   X2 <- rnorm(N[i], mean=intercept+meanES[i], sd=groupsd[i]) # sampling values for "treatment" group
  #   studies[[i]] <- data.frame(X1,X2)
  # }
  
  
  df <- as.data.frame(matrix(unlist(lapply(as.matrix(1:k.studies, ncol=1), FUN = function(x) {
    summa <- function(studies, x){
      m1 <- mean(studies[[x]]$X1)
      m2 <- mean(studies[[x]]$X2)
      sd1 <- sd(studies[[x]]$X1)
      sd2 <- sd(studies[[x]]$X2)
      N <- length(studies[[x]]$X1)
      md <- mean(studies[[x]]$X2-studies[[x]]$X1)
      return(c(m1, m2, sd1, sd2, N, md))
    }
    summa(studies, x)
  })), ncol=6, byrow=T))
  
  # 
  # mx <- matrix(NA, nrow=k.studies, ncol=6) # prepping empty matrix to store study-summaries
  # 
  # for(i in 1:k.studies){
  #   mx[i,1] <- mean(studies[[i]]$X1) # mean "control" group
  #   mx[i,2] <- mean(studies[[i]]$X2) # mean "treatment" group
  #   mx[i,3] <- sd(studies[[i]]$X1) # sd "control" group
  #   mx[i,4] <- sd(studies[[i]]$X2) # sd "treatment" group
  #   mx[i,5] <- length(studies[[i]]$X1) # n for each group (N = 2n)
  #   mx[i,6] <- mean(studies[[i]]$X2-studies[[i]]$X1) # mean difference
  # }
  # 
  # df <- as.data.frame(mx)
  
  names(df) <- c("mean.X1", "mean.X2", "sd.X1", "sd.X2", "N", "diff")
  
  # function returns 3 objects in one list:
  #  1.: list containing individual study data
  #  2.: data.frame containing study summaries
  #  3.: list containing the sampled effect sizes, sd, range of n, and sampling probabilities of n
  return(list(study.data = studies,
              study.summary = df,
              sim.info = list(meanES, groupsd, n.range, n.sample.prob, N)))
}
