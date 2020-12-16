### Lukas Beinhauer 16/12/20 ###

#################################################################################################################################
# Assessing the influence of various parameters on the post-hoc probability of a statistically significant result indicating a  #
#  genuine "true" effect. This is also known as the Posterior Predictive Values (PPV). It is examined in how far variations in  #
#  significance level, ratio of true hypotheses, and a priori Power have an impact on the PPV.                                  #
# To do so, numbers of studies are simulated, indicating how many studies out of e.g. 1000 studies returned signficant test     #
#  results. This allows the computation of PPV and False Discovery Rate (FDR)                                                   #
#################################################################################################################################

if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
if (!require(grid)) {
  install.packages("grid")
}


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

FDR.Pb <- ggplot() +
  geom_point(aes(x=parameters[[1]]$Pb, y=mFDR[,1]), color="darkred") +
  labs(title="FDR as function of Ratio of True Hypotheses") +
  xlab("Ratio of True Hypotheses") +
  ylab("FDR") +
  theme_gray()
PPV.Pb <- ggplot() +
  geom_point(aes(x=parameters[[1]]$Pb, y=mPPV[,1]), color="darkblue") +
  labs(title="PPV as function of Ratio of True Hypotheses") +
  xlab("Ratio of True Hypotheses") +
  ylab("PPV") +
  theme_gray()

FDR.power <- ggplot() +
  geom_point(aes(x=parameters[[2]]$Power, y=mFDR[,2]), color="darkred") +
  labs(title="FDR as function of Power") +
  xlab("Power") +
  ylab("FDR") +
  theme_gray()
PPV.power <- ggplot() +
  geom_point(aes(x=parameters[[2]]$Power, y=mPPV[,2]), color="darkblue") +
  labs(title="PPV as function of Ratio of True Hypotheses") +
  xlab("Power") +
  ylab("PPV") +
  theme_gray()

FDR.alpha <- ggplot() +
  geom_point(aes(x=parameters[[3]]$alpha, y=mFDR[,3]), color="darkred") +
  labs(title="FDR as function of significance level") +
  xlab("alpha") +
  ylab("FDR") +
  theme_gray()
PPV.alpha <- ggplot() +
  geom_point(aes(x=parameters[[3]]$alpha, y=mPPV[,3]), color="darkblue") +
  labs(title="PPV as function of significance level") +
  xlab("alpha") +
  ylab("PPV") +
  theme_gray()

grid.arrange(FDR.Pb, PPV.Pb, FDR.power, PPV.power, FDR.alpha, PPV.alpha, ncol=2)


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

## issue: ggplot does not work inside a loop!!!

plot.list <- apply(matrix(1:ncol(mFDR), ncol=1), 1, FUN=function(x) 
                   ggplot() +
                     geom_point(aes(x=Power, y=mPPV[,x]), color="darkblue") +
                     theme_gray() + 
                     theme(axis.title = element_blank())
                   )


# labs(title="PPV as function of significance level") +
#   xlab("Power") +
#   ylab("PPV") +
  
# for(i in 1:ncol(mFDR)){
#   plot.list[[i]] <- ggplot() +
#     geom_point(aes(x=Power, y=mPPV[,i]), color="darkblue") +
#     labs(title="PPV as function of significance level") +
#     xlab("Power") +
#     ylab("PPV") +
#     theme_gray()
#   #print(plot(Power, mFDR[,i], main=sprintf("Power and FDR, at Pb=%f", Pb[i]), ylab="FDR"))
# }

grid.arrange(plot.list[[1]], plot.list[[2]], plot.list[[3]], plot.list[[4]],
             plot.list[[5]], plot.list[[6]], plot.list[[7]], plot.list[[8]],
             plot.list[[9]], plot.list[[10]], plot.list[[11]], plot.list[[12]],
             plot.list[[13]], plot.list[[14]], plot.list[[15]], plot.list[[16]], 
             top = textGrob("PPV as function of a priori power", gp=gpar(fontsize=20, font=2)),
             bottom = textGrob("a priori power", gp=gpar(fontsize=15, font=3)),
             left = textGrob("PPV ", gp=gpar(fontsize=15, font=3)),
             ncol=4)

dev.off()



