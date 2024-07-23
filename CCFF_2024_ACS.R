################################################################################
# Replication "Nonlinear Binscatter Methods"
# Cattaneo, Crump, Farrell and Feng (2024)
# ACS Application
################################################################################


rm(list=ls(all.names = TRUE))
# install.packages("binsreg_1.1.tar.gz", repos = NULL)
library(binsreg)
sessionInfo()
library(ggplot2)
library(dplyr)

data <- read.csv(file='CCFF_2024_ACS_1.csv')
data <- subset(data, select=c(uninsuredRate,perCapitaIncome,percentBachelorsEdu,medianAge,percentHsEdu,ueRate))

# Rescale Uninsured Rate to be between 0 and 1
data$uninsuredRate = data$uninsuredRate/100
data$perCapitaIncome <- data$perCapitaIncome/1000

#If you need to remove the missing data
#data.new <- data[!(is.na(data$uninsuredRate) | is.na(data$perCapitaIncome)),]

#controls
w <- select(data, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))


################################################################################
# FIGURE 1(a) Plain scatter
################################################################################
png('graphs/Fig1a_Census_Scatter.png')
plot(data$perCapitaIncome, data$uninsuredRate, col='navy', pch=19 ,xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", cex.lab=1.25, cex.axis=1.25)
dev.off()

################################################################################
# FIGURE 1(b) Demonstration of scatter to binscatter
################################################################################
png('graphs/Fig1b_Census_ScatterAndBinscatter.png')
res <- binsglm(data$uninsuredRate, data$perCapitaIncome, nbins=10, line=c(0,0), family=binomial())
line <- res$data.plot$`Group Full Sample`$data.line
dot <- res$data.plot$`Group Full Sample`$data.dots

line <- as.list(line[line$isknot == 1,]$x)

plot(data$perCapitaIncome, data$uninsuredRate, col='gray', pch=19 ,xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
points(dot$x, dot$fit, col='blue', pch=19, cex=1.2)

for (l in line){
  abline(v=l, col="black",  lty=2)
}
dev.off()

################################################################################
# FIGURE 1(c) Demonstration of conventional binscatter plot
################################################################################
png('graphs/Fig1c_Census_BinscatterAndLine.png')
res <- binsglm(data$uninsuredRate, data$perCapitaIncome, nbins=10, polyreg=1, plotxrange=c(0,80),family=binomial())
line <- res$data.plot$`Group Full Sample`$data.poly
dots <- res$data.plot$`Group Full Sample`$data.dots

plot(dots$x, dots$fit, col='blue', pch=19, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
lines(line$x, line$fit, col='forestgreen', lwd=1.5)
dev.off()


################################################################################
#  FIGURE 1(d) IMSE optimal number of bins
################################################################################
res <- binsglm(data$uninsuredRate, data$perCapitaIncome, polyreg=1, plotxrange=c(0,80),family=binomial(), randcut = 1)

png('graphs/Fig1d_Census_Binscatter_J_IMSE.png')
line <- res$data.plot$`Group Full Sample`$data.poly
dots <- res$data.plot$`Group Full Sample`$data.dots
plot(dots$x, dots$fit, col='blue', pch=19, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
lines(line$x, line$fit, col='forestgreen', lwd=1.5)
dev.off()


################################################################################
# FIGURE 2 Quantiles
################################################################################

res_1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.1, plotxrange = c(0,80))
res_5 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.5, plotxrange = c(0,80))
res_9 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.9, plotxrange = c(0,80))

dots_1 <- res_1$data.plot$`Group Full Sample`$data.dots
dots_5 <- res_5$data.plot$`Group Full Sample`$data.dots
dots_9 <- res_9$data.plot$`Group Full Sample`$data.dots


res_1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.1, plotxrange = c(0,80))
res_5 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.5, plotxrange = c(0,80))
res_9 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.9, plotxrange = c(0,80))

dots_1a <- res_1$data.plot$`Group Full Sample`$data.dots
dots_5a <- res_5$data.plot$`Group Full Sample`$data.dots
dots_9a <- res_9$data.plot$`Group Full Sample`$data.dots


png('graphs/Fig2a_Census_BinscatterQuantilesNoControls.png')
plot(dots_9$x, dots_9$fit, col='gray', pch=19, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,100), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
points(dots_5$x, dots_5$fit, col='black', pch=19)
points(dots_1$x, dots_1$fit, col='gray', pch=19)
legend("topright", legend=c("10th", "50th", "90th"), col=c("gray", "black",'gray'), pch=19, cex=1.25)
dev.off()

png('graphs/Fig2b_Census_BinscatterQuantilesYesControls.png')
plot(dots_9a$x, dots_9a$fit, col='gray', pch=19, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,100), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
points(dots_5a$x, dots_5a$fit, col='black', pch=19)
points(dots_1a$x, dots_1a$fit, col='gray', pch=19)
legend("topright", legend=c("10th", "50th", "90th"), col=c("gray", "black",'gray'), pch=19, cex=1.25)
dev.off()




################################################################################
# FIGURE 3 - Confidence band around mean fcn and marginal effect
################################################################################
glm.fit <- binsglm(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), nsims=50000, simsgrid=100, level=95)
tmp.plot <- glm.fit$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(0,.4) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
png('graphs/Fig3a_Census_ConfidenceBand_Mean.png')
  plot(tmp.plot)
dev.off()



glm.fit <- binsglm(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T, dots=c(1,1), deriv=1, nolink=FALSE, family=binomial(), plotxrange = c(0,80), nsims=50000, simsgrid=100, level=95)
tmp.plot <- glm.fit$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(-0.025,0.025) + 
  geom_hline(yintercept=0, linetype='dashed', color=c('grey')) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
png('graphs/Fig3b_Census_ConfidenceBand_MarginalEffect.png')
  plot(tmp.plot)
dev.off()



################################################################################
# FIGURE 4 - Group comparisons
################################################################################

#Population density comes in a separate file, and is not available for all areas.
#Data was obtained from https://www.census.gov/data/tables/time-series/dec/density-data-text.html
data3 <- read.csv(file='CCFF_2024_ACS_2.csv')
data3$uninsuredRate = data3$uninsuredRate/100
data3$perCapitaIncome <- data3$perCapitaIncome/1000
w3 <- select(data3, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))



## two groups - no covariates
glm.fit <- binsglm(data3$uninsuredRate, data3$perCapitaIncome, by=data3$idxpopdens, bycolors=c("darkorange","blue"), randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100)
tmp.plot <- glm.fit$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(0,.4) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position="none") 

png('graphs/Fig4a_Census_GroupComparisonNoControls.png')
plot(tmp.plot)
dev.off()


## CATE plot - no covariates
# first get the CATE point estimate
# Get the binning from the two-group plot. Pick whichever group had fewer bins and use that group's bins for the CATE estimation.
which.min(c(nrow(glm.fit$data.plot$`Group 0`$data.bin), nrow(glm.fit$data.plot$`Group 1`$data.bin)))
cate.bins <- glm.fit$data.plot$`Group 1`$data.bin$left.endpoint[-1]
cate.points.fit <- binsglm(data3$uninsuredRate, data3$perCapitaIncome, by=data3$idxpopdens, 
                           bycolors=c("darkorange","blue"), randcut=1, nolink=FALSE, family=binomial(), 
                           plotxrange = c(0,80), samebinsby = TRUE, binspos = cate.bins, 
                           dotsgridmean = TRUE, nsims=50000, simsgrid=100)  

rows.to.keep <- (cate.points.fit$data.plot$`Group 1`$data.dots$x<80)
cate.point.estimate <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"fit"] - cate.points.fit$data.plot$`Group 0`$data.dots[rows.to.keep,"fit"]
x.for.plot <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"x"]
cate.plot.data <- data.frame(x.for.plot,cate.point.estimate)

# now get the CATE confidence band
cate <- binspwc(data3$uninsuredRate, data3$perCapitaIncome, by=data3$idxpopdens, estmethod = "glm", randcut=1, 
                nolink=FALSE, family=binomial(), plot=TRUE, plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100,
                samebinsby = TRUE, binspos = cate.bins)

tmp.plot <- cate$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  geom_point(data=cate.plot.data, aes(x=x.for.plot, y=cate.point.estimate)) + 
  ylab('Percent Uninsured') + xlim(0,80)  + ylim(-0.05,0.2) + 
  geom_hline(yintercept=0, linetype='dashed', color=c('grey')) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position="none") 

png('graphs/Fig4c_Census_CATENoControls.png')
plot(tmp.plot)
dev.off()



## two groups - with covariates
glm.fit <- binsglm(data3$uninsuredRate, data3$perCapitaIncome, w=w3, by=data3$idxpopdens, bycolors=c("darkorange","blue"), randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100)
tmp.plot <- glm.fit$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(0,.4) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position="none") 

png('graphs/Fig4b_Census_GroupComparisonYesControls.png')
plot(tmp.plot)
dev.off()



## CATE plot - with covariates
# first get the CATE point estimate
# Get the binning from the two-group plot. Pick whichever group had fewer bins and use that group's bins for the CATE estimation.
which.min(c(nrow(glm.fit$data.plot$`Group 0`$data.bin), nrow(glm.fit$data.plot$`Group 1`$data.bin)))
cate.bins <- glm.fit$data.plot$`Group 1`$data.bin$left.endpoint[-1]
cate.points.fit <- binsglm(data3$uninsuredRate, data3$perCapitaIncome, w=w3, by=data3$idxpopdens, 
                           bycolors=c("darkorange","blue"), randcut=1, nolink=FALSE, family=binomial(), 
                           plotxrange = c(0,80), samebinsby = TRUE, binspos = cate.bins,  
                           dotsgridmean = TRUE, nsims=50000, simsgrid=100)

rows.to.keep <- cate.points.fit$data.plot$`Group 1`$data.dots$x<80
cate.point.estimate <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"fit"] - cate.points.fit$data.plot$`Group 0`$data.dots[rows.to.keep,"fit"]
x.for.plot <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"x"]
cate.plot.data <- data.frame(x.for.plot,cate.point.estimate)

# now get the CATE confidence band
cate <- binspwc(data3$uninsuredRate, data3$perCapitaIncome, w=w3, by=data3$idxpopdens, estmethod = "glm", randcut=1, 
                nolink=FALSE, family=binomial(), plot=TRUE, plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100,
                samebinsby = TRUE, binspos = cate.bins)

tmp.plot <- cate$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  geom_point(data=cate.plot.data, aes(x=x.for.plot, y=cate.point.estimate)) + 
  ylab('Percent Uninsured') + xlim(0,80)  + ylim(-0.05,0.2) + 
  geom_hline(yintercept=0, linetype='dashed', color=c('grey')) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position="none") 

png('graphs/Fig4d_Census_CATEYesControls.png')
plot(tmp.plot)
dev.off()





### Formally test the above comparisons
#without covariates
summary(
  binspwc(data3$uninsuredRate, data3$perCapitaIncome, by=data3$idxpopdens, estmethod = "glm", randcut=1, nolink=FALSE, family=binomial(), nsims=50000, simsgrid=100)
)
#adding the covariates
summary(
  binspwc(data3$uninsuredRate, data3$perCapitaIncome, w=w3, by=data3$idxpopdens, estmethod = "glm", randcut=1, nolink=FALSE, family=binomial(), nsims=50000, simsgrid=100)
)



################################################################################
# TABLE: Specification & Shape  Testing
################################################################################


data <- read.csv(file='CCFF_2024_ACS_1.csv')
data <- subset(data, select=c(uninsuredRate,perCapitaIncome,percentBachelorsEdu,medianAge,percentHsEdu,ueRate))

# Rescale Uninsured Rate to be between 0 and 1
data$uninsuredRate = data$uninsuredRate/100
data$perCapitaIncome <- data$perCapitaIncome/1000

#controls
w <- select(data, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))

#restrict to those above a medicaid eligibility cutoff
# 16.248=1.38 * average federal povery line, 2013-2017. see paper.
upper <- (data$perCapitaIncome > 16.248) 


### Test against linear in X ###
#sup norm, no controls
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testmodelpoly=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
#sup norm, with controls
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testmodelpoly=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

#sup norm, no controls
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testmodelpoly=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
#sup norm, with controls
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testmodelpoly=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

### Test against cubic in X ###
#sup norm, no controls
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testmodelpoly=3, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
#sup norm, with controls
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testmodelpoly=3, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

#sup norm, no controls
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testmodelpoly=3, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
#sup norm, with controls
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testmodelpoly=3, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )


### Monotonicity ###
#Test if the conditional mean is decreasing, i.e. the derivative is negative
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testshapel=0, deriv=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testshapel=0, deriv=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testshapel=0, deriv=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testshapel=0, deriv=1, estmethod="glm", nolink=FALSE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )




