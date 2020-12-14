library(devtools)
library(RCurl)

source_url("https://github.com/LBeinhauer/SimulatingStudies/blob/master/SimStudies.R?raw=TRUE") # hyperlink added with ?raw=TRUE

# X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.5, mean.groupsd=2, sd.groupsd=.00000001, n.max=125, n.min=20)
# 
# y <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
#          sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
# 


# sd.meandiff = .5

pdf("simulatedHeterogeneityPlots.pdf")

group.vars <- seq(from=.01, to=5.005, by=.005)^2

rma.fit <- list()

het.measures <- matrix(NA, nrow=length(group.vars), ncol=3)

pb = txtProgressBar(min = 0, max = length(group.vars), initial = 0, style=3) 
for(i in 1:length(group.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.5, mean.groupsd=sqrt(group.vars[i]), sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}

## Apply is slower with 100 studies, than for loop (unclear why, faster with fewer, e.g. 10 studies. Likely due to est. algoirthm in rma)
# het.measures <- apply(matrix(1:length(group.vars), ncol=1), 1, FUN = function(x) {
#   doingit <- function(x){
#     dat <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.5, mean.groupsd=sqrt(group.vars[x]), sd.groupsd=0, n.max=125, n.min=20)
#     fit <- rma(measure = "SMD", m1i = dat$study.summary$mean.X2, m2i = dat$study.summary$mean.X1, sd1i = dat$study.summary$sd.X2, 
#         sd2i = dat$study.summary$sd.X1, n1i = dat$study.summary$N, n2i = dat$study.summary$N)
#     return(c(fit$I2, fit$H2, fit$tau2))
#   }
#   doingit(x)
# })

plot(group.vars, het.measures[,1], main = "I^2 for sd.meandiff = .5")
plot(group.vars, het.measures[,2], main = "H^2 for sd.meandiff = .5", ylim=c(0,20))
plot(group.vars, het.measures[,3], main = "tau^2 for sd.meandiff = .5", ylim=c(0,10))



# sd.meandiff=1

# I^2 = tau^2 / (tau^2 = sigma^2)

pb = txtProgressBar(min = 0, max = length(group.vars), initial = 0, style=3) 
for(i in 1:length(group.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=1, mean.groupsd=sqrt(group.vars[i]), sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(group.vars, het.measures[,1], main = "I^2 for sd.meandiff = 1")
plot(group.vars, het.measures[,2], main = "H^2 for sd.meandiff = 1", ylim=c(0,20))
plot(group.vars, het.measures[,3], main = "tau^2 for sd.meandiff = 1", ylim=c(0,10))


# sd.meandiff=.05


pb = txtProgressBar(min = 0, max = length(group.vars), initial = 0, style=3) 
for(i in 1:length(group.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.05, mean.groupsd=sqrt(group.vars[i]), sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(group.vars, het.measures[,1], main = "I^2 for sd.meandiff = .05")
plot(group.vars, het.measures[,2], main = "H^2 for sd.meandiff = .05", ylim=c(0,20))
plot(group.vars, het.measures[,3], main = "tau^2 for sd.meandiff = .05", ylim=c(0,10))



 
# sd.meandiff=.2


pb = txtProgressBar(min = 0, max = length(group.vars), initial = 0, style=3) 
for(i in 1:length(group.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=.2, mean.groupsd=sqrt(group.vars[i]), sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(group.vars, het.measures[,1], main = "I^2 for sd.meandiff = .2")
plot(group.vars, het.measures[,2], main = "H^2 for sd.meandiff = .2", ylim=c(0,20))
plot(group.vars, het.measures[,3], main = "tau^2 for sd.meandiff = .2", ylim=c(0,10))







ES.vars <- seq(from=.0005, to=.30, by=.0005)

rma.fit <- list()

het.measures <- matrix(NA, nrow=length(ES.vars), ncol=3)

pb = txtProgressBar(min = 0, max = length(ES.vars), initial = 0, style=3) 
for(i in 1:length(ES.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=ES.vars[i], mean.groupsd=.2, sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(ES.vars, het.measures[,1], main = "I^2 for mean.groupsd=.2")
plot(ES.vars, het.measures[,2], main = "H^2 for mean.groupsd=.2", ylim=c(0,20))
plot(ES.vars, het.measures[,3], main = "tau^2 for mean.groupsd=.2", ylim=c(0,10))



# mean.groupsd=.5

# I^2 = tau^2 / (tau^2 = sigma^2)

b = txtProgressBar(min = 0, max = length(ES.vars), initial = 0, style=3) 
for(i in 1:length(ES.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=ES.vars[i], mean.groupsd=.5, sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(ES.vars, het.measures[,1], main = "I^2 for mean.groupsd=.5")
plot(ES.vars, het.measures[,2], main = "H^2 for mean.groupsd=.5", ylim=c(0,20))
plot(ES.vars, het.measures[,3], main = "tau^2 for mean.groupsd=.5", ylim=c(0,10))


# mean.groupsd=.05


b = txtProgressBar(min = 0, max = length(ES.vars), initial = 0, style=3) 
for(i in 1:length(ES.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=ES.vars[i], mean.groupsd=.05, sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(ES.vars, het.measures[,1], main = "I^2 for mean.groupsd=.05")
plot(ES.vars, het.measures[,2], main = "H^2 for mean.groupsd=.05", ylim=c(0,20))
plot(ES.vars, het.measures[,3], main = "tau^2 for mean.groupsd=.05", ylim=c(0,10))



# mean.groupsd=1


b = txtProgressBar(min = 0, max = length(ES.vars), initial = 0, style=3) 
for(i in 1:length(ES.vars)){
  X <- SimStudies2groups(k.studies=100, meandiff=1, intercept=2, sd.meandiff=ES.vars[i], mean.groupsd=1, sd.groupsd=0, n.max=125, n.min=20)
  
  rma.fit[[i]] <- rma(measure = "SMD", m1i = X$study.summary$mean.X2, m2i = X$study.summary$mean.X1, sd1i = X$study.summary$sd.X2, 
                      sd2i = X$study.summary$sd.X1, n1i = X$study.summary$N, n2i = X$study.summary$N)
  het.measures[i,1] <- rma.fit[[i]]$I2
  het.measures[i,2] <- rma.fit[[i]]$H2
  het.measures[i,3] <- rma.fit[[i]]$tau2
  setTxtProgressBar(pb,i)
}


plot(ES.vars, het.measures[,1], main = "I^2 for mean.groupsd=1")
plot(ES.vars, het.measures[,2], main = "H^2 for mean.groupsd=1", ylim=c(0,20))
plot(ES.vars, het.measures[,3], main = "tau^2 for mean.groupsd=1", ylim=c(0,10))


dev.off()







