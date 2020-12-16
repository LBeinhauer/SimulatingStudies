### Lukas Beinhauer 16/12/20 ###

#################################################################################################################################
# Assessing the influence of various parameters on the post-hoc probability of a statistically significant result indicating a  #
#  genuine "true" effect. This is also known as the Posterior Predictive Values (PPV). It is examined in how far variations in  #
#  significance level, ratio of true hypotheses, and a priori Power have an impact on the PPV.                                  #
# To do so, numbers of studies are simulated, indicating how many studies out of e.g. 1000 studies returned signficant test     #
#  results. This allows the computation of PPV and False Discovery Rate (FDR)                                                   #
#################################################################################################################################


mFDR <- mPPV <- matrix(NA, ncol=3, nrow=1000)

parameters <- list(data.frame(alpha=rep(.05, 1000),
                              Power=rep(.8, 1000),
                              Pb=seq(from=.001, to=.5, length.out=1000)),
                   data.frame(alpha=rep(.05, 1000),
                              Power=seq(from=.001, to=1, length.out=1000),
                              Pb=rep(.3, 1000)),
                   data.frame(alpha=seq(from=.000001, to=.2, length.out=1000),
                              Power=rep(.8, 1000),
                              Pb=(rep(.3, 1000))))


# library(devtools)
# library(RCurl)
# source_url("https://github.com/LBeinhauer/VariousFunctions/blob/master/Functions.R?raw=TRUE") # hyperlink added with ?raw=TRUE

source("~/R-Projects/VariousFunctions/Functions.R")

for(ii in 1:3){
  pars <- parameters[[ii]]
  alpha <- pars$alpha
  Power <- pars$Power
  Pb <- pars$Pb
  for(i in 1:1000){
    x <- getPPV(kstudies=1000, alpha=alpha[i], Power=Power[i], ratio.true.hypotheses=Pb[i])
    mFDR[i,ii] <- x$FDR
    mPPV[i,ii] <- x$PPV
  } # end of i loop
} # end of ii loop

dev.off()
par(mfrow=c(3,2))

plot(parameters[[1]]$Pb, mFDR[,1], main="FDR as function of Ratio of True Hypotheses", xlab="FDR", ylab="Ratio of True Hypotheses")
plot(parameters[[1]]$Pb, mPPV[,1], main="PPV as function of Ratio of True Hypotheses", xlab="PPV", ylab="Ratio of True Hypotheses")

plot(parameters[[2]]$Power, mFDR[,2], main="FDR as function of Power", xlab="FDR", ylab="Power")
plot(parameters[[2]]$Power, mPPV[,2], main="PPV as function of Power", xlab="PPV", ylab="Power")

plot(parameters[[3]]$alpha, mFDR[,3], main="FDR as function of significance level", xlab="FDR", ylab="alpha")
plot(parameters[[3]]$alpha, mPPV[,3], main="PPV as function of significance level", xlab="PPV", ylab="alpha")



mFDR <- mPPV <- matrix(NA, ncol=16, nrow=1000)

Pb <- seq(from=.05, to=.8, length.out=16)
Power <- seq(from=.001, to=1, length.out=1000)
for(ii in 1:16){
  for(i in 1:length(Power)){
    x <- getPPV(kstudies=1000, alpha=.05, Power=Power[i], ratio.true.hypotheses=Pb[ii])
    mFDR[i,ii] <- x$FDR
    mPPV[i,ii] <- x$PPV
  }
}

pdf(file="Power_and_PPV.pdf")

par(mfrow=c(4,4))

for(i in 1:ncol(mFDR)){
  #print(plot(Power, mFDR[,i], main=sprintf("Power and FDR, at Pb=%f", Pb[i]), ylab="FDR"))
  print(plot(Power, mPPV[,i], main=sprintf("Power and PPV, at Pb=%f", Pb[i]), ylab="PPV"))
}

dev.off()



