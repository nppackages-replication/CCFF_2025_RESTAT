################################################################################
# Replication "Nonlinear Binscatter Methods"
# Cattaneo, Crump, Farrell and Feng (2026)
# ACS Application
################################################################################

# Run this script from the replication folder. It reads the two CSV files in
# that folder, writes figures to graphs/, and prints the Table 1/Figure 4 tests
# and a few diagnostics to the R console. The graphs/ directory must exist.
#
# Main binsreg conventions used below:
# - family=binomial() fits logistic QMLE; nolink=FALSE reports results after the
#   inverse-logit transformation, on the outcome scale.
# - Omitting nbins requests the package's data-driven IMSE bin selector.
# - randcut=1 uses the full sample during bin selection rather than a subsample.
# - cb=TRUE requests a uniform confidence band; nsims and simsgrid control its
#   simulation draws and within-bin evaluation grid.

# Start from a clean workspace. Warning: this removes all existing R objects.
rm(list=ls(all.names = TRUE))

# Load binscatter estimation, plotting, and data-selection functions. Install if necessary.
library(binsreg)
library(ggplot2)
library(dplyr)

# Read the main zip-code-level ACS analysis file.
data <- read.csv(file='CCFF_2026_ACS_1.csv')

# Convert uninsured percentages to proportions and income dollars to $1,000s.
data$uninsuredRate <- data$uninsuredRate/100
data$perCapitaIncome <- data$perCapitaIncome/1000

# Keep observations with nonmissing outcome and income. Controlled analyses
# below also use complete cases for the controls, as handled by binsreg.
data <- data[!(is.na(data$uninsuredRate) | is.na(data$perCapitaIncome)),]

# Store the nine controls described in the paper as the matrix/data frame w.
w <- select(data, c('percentBachelorsEdu', 'medianAge', 'percentHsEdu', 'ueRate', 
                     'meanHouseholdSize', 'percentNoWeb', 'percentArmedForces', 
                     'percentEnglishOnly', 'percent65andOlder'
                     )
             )



################################################################################
# FIGURE 1(a) Plain scatter
################################################################################
# Open a PNG device, draw the raw outcome-income scatter, and save it on dev.off().
png('graphs/Fig1a_Census_Scatter.png')
plot(data$perCapitaIncome, data$uninsuredRate, col='navy', pch=19 ,xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", cex.lab=1.25, cex.axis=1.25)
dev.off()


################################################################################
# FIGURE 1(b) Demonstration of scatter to binscatter
################################################################################
# Fit a piecewise-constant logistic binscatter using a deliberately fixed J=10.
# line=c(0,0) also returns the piecewise-constant line used to recover bin knots.
png('graphs/Fig1b_Census_ScatterAndBinscatter.png')
res <- binsglm(data$uninsuredRate, data$perCapitaIncome, nbins=10, line=c(0,0), family=binomial())

# data.plot contains plot-ready coordinates; extract the fitted dots and knots.
line <- res$data.plot$`Group Full Sample`$data.line
dot <- res$data.plot$`Group Full Sample`$data.dots

# Retain the knot rows and draw their x-coordinates as vertical dashed lines.
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
# Retain J=10 and add the global first-degree logistic fit used for comparison.
png('graphs/Fig1c_Census_BinscatterAndLine.png')
res <- binsglm(data$uninsuredRate, data$perCapitaIncome, nbins=10, polyreg=1, plotxrange=c(0,80),family=binomial())
# Extract plot-ready coordinates for the global line and binned point estimates.
line <- res$data.plot$`Group Full Sample`$data.poly
dots <- res$data.plot$`Group Full Sample`$data.dots

plot(dots$x, dots$fit, col='blue', pch=19, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
lines(line$x, line$fit, col='forestgreen', lwd=1.5)
dev.off()


################################################################################
#  FIGURE 1(d) IMSE optimal number of bins
################################################################################
# Repeat Figure 1(c) but let binsglm select the IMSE-optimal number of bins.
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

# Estimate the 10th, 50th, and 90th conditional quantiles without controls.
res_1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.1, plotxrange = c(0,80))
res_5 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.5, plotxrange = c(0,80))
res_9 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=F, quantile=0.9, plotxrange = c(0,80))

dots_1 <- res_1$data.plot$`Group Full Sample`$data.dots
dots_5 <- res_5$data.plot$`Group Full Sample`$data.dots
dots_9 <- res_9$data.plot$`Group Full Sample`$data.dots


# Repeat with the nine controls. The suffix "a" denotes controlled results.
res_1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.1, plotxrange = c(0,80))
res_5 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.5, plotxrange = c(0,80))
res_9 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, cb=F, quantile=0.9, plotxrange = c(0,80))

dots_1a <- res_1$data.plot$`Group Full Sample`$data.dots
dots_5a <- res_5$data.plot$`Group Full Sample`$data.dots
dots_9a <- res_9$data.plot$`Group Full Sample`$data.dots


# Overlay the three quantile estimates, first without and then with controls.
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
# Panel (a): logistic-QMLE conditional mean with a 95% uniform confidence band.
glm.fit <- binsglm(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), nsims=50000, simsgrid=100, level=95)
# bins_plot is the ggplot object returned by binsglm; customize it for the paper.
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



# Panel (b): first derivative of the fitted outcome-scale function. dots=c(1,1)
# uses a continuous piecewise-linear fit for the plotted marginal-effect dots.
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
# TABLE 1 - Specification & Shape  Testing
################################################################################


# Define the restricted sample above the Medicaid-related income cutoff.
# 16.248 equals 1.38 times the 2013--2017 average federal poverty line,
# expressed in thousands of dollars; see the paper for its construction.
upper <- (data$perCapitaIncome > 16.248) 


### Test against linear in X ###
# Each binstest call prints a sup-norm test of the stated global polynomial
# index specification. nolink=TRUE puts both fitted functions on the logit
# index scale. The four calls cover full/restricted samples without/with w.
# Full sample, no controls.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testmodelpoly=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Full sample, with controls evaluated at their sample means.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testmodelpoly=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

# Restricted sample, no controls.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testmodelpoly=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Restricted sample, with controls evaluated at their sample means.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testmodelpoly=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

### Test against cubic in X ###
# Repeat the same four comparisons against a global cubic specification.
# Full sample, no controls.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testmodelpoly=3, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Full sample, with controls.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testmodelpoly=3, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

# Restricted sample, no controls.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testmodelpoly=3, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Restricted sample, with controls.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testmodelpoly=3, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )


### Monotonicity ###
# Test the null that the logit index's first derivative is everywhere
# nonpositive. testshapel=0 specifies the zero upper boundary for that
# one-sided test.
# Full sample, no controls.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, testshapel=0, deriv=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Full sample, with controls.
summary(  binstest(data$uninsuredRate, data$perCapitaIncome, w, testshapel=0, deriv=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )

# Restricted sample, no controls.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], testshapel=0, deriv=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )
# Restricted sample, with controls.
summary(  binstest(data$uninsuredRate[upper], data$perCapitaIncome[upper], w[upper,], testshapel=0, deriv=1, estmethod="glm", nolink=TRUE, family=binomial(), randcut=1, nsims=50000, simsgrid = 100) )







################################################################################
# FIGURE 4 - Group comparisons
################################################################################

# Population density comes in a separate file and is unavailable for some areas.
# Density source: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
# In idxpopdens, Group 1 is low-density (<100 people/sq. mile) and Group 0 is
# high-density. Rescale outcome and income exactly as in the main data.
data_cate <- read.csv(file='CCFF_2026_ACS_2.csv')
data_cate$uninsuredRate = data_cate$uninsuredRate/100
data_cate$perCapitaIncome <- data_cate$perCapitaIncome/1000
# Construct the same nine-control set for the density-comparison sample.
# The commented line records an earlier specification and is not run.
# w_cate <- select(data_cate, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))
w_cate <- select(data_cate, c('percentBachelorsEdu', 'medianAge', 'percentHsEdu', 'ueRate', 
                    'meanHouseholdSize', 'percentNoWeb', 'percentArmedForces', 
                    'percentEnglishOnly', 'percent65andOlder'
                    )
                 )


## Two groups, no controls ##
# by= fits the two density groups separately, including group-specific binning.
glm.fit <- binsglm(data_cate$uninsuredRate, data_cate$perCapitaIncome, by=data_cate$idxpopdens, bycolors=c("darkorange","blue"), randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100)
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


## Group-difference (CATE) plot, no controls ##
# First construct point estimates on one common partition. Print which group has
# fewer bins as a diagnostic (1 means Group 0; 2 means Group 1), then deliberately
# use the low-density partition, as stated in the paper. [-1] leaves interior knots.
which.min(c(nrow(glm.fit$data.plot$`Group 0`$data.bin), nrow(glm.fit$data.plot$`Group 1`$data.bin)))
cate.bins <- glm.fit$data.plot$`Group 1`$data.bin$left.endpoint[-1]
# Refit both groups with those same knots. dotsgridmean=TRUE places each group's
# dot at its own within-bin mean x; the code subtracts the matched-bin fits.
cate.points.fit <- binsglm(data_cate$uninsuredRate, data_cate$perCapitaIncome, by=data_cate$idxpopdens, 
                           bycolors=c("darkorange","blue"), randcut=1, nolink=FALSE, family=binomial(), 
                           plotxrange = c(0,80), samebinsby = TRUE, binspos = cate.bins, 
                           dotsgridmean = TRUE, nsims=50000, simsgrid=100)  

# Retain plotted x-values below $80,000 and form low-density minus high-density.
rows.to.keep <- (cate.points.fit$data.plot$`Group 1`$data.dots$x<80)
cate.point.estimate <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"fit"] - cate.points.fit$data.plot$`Group 0`$data.dots[rows.to.keep,"fit"]
x.for.plot <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"x"]
cate.plot.data <- data.frame(x.for.plot,cate.point.estimate)

# Use binspwc with the same knots to obtain the 95% uniform difference band.
cate <- binspwc(data_cate$uninsuredRate, data_cate$perCapitaIncome, by=data_cate$idxpopdens, estmethod = "glm", randcut=1, 
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



## Two groups, with controls ##
# Repeat the group-specific curves with the nine controls included. With by= and
# no explicit at= value, binsglm evaluates each curve at that group's mean w.
glm.fit <- binsglm(data_cate$uninsuredRate, data_cate$perCapitaIncome, w=w_cate, by=data_cate$idxpopdens, bycolors=c("darkorange","blue"), randcut=1, cb=T, nolink=FALSE, family=binomial(), plotxrange = c(0,80), level=95, nsims=50000, simsgrid=100)
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



## Group-difference (CATE) plot, with controls ##
# As above, print the fewer-bin group (1=Group 0; 2=Group 1) as a diagnostic but
# deliberately impose the low-density (Group 1) partition for the paper.
which.min(c(nrow(glm.fit$data.plot$`Group 0`$data.bin), nrow(glm.fit$data.plot$`Group 1`$data.bin)))
cate.bins <- glm.fit$data.plot$`Group 1`$data.bin$left.endpoint[-1]
# Refit both groups on the common knots before computing low minus high. These
# manual point fits use each group's own mean w, as in the preceding group plot.
cate.points.fit <- binsglm(data_cate$uninsuredRate, data_cate$perCapitaIncome, w=w_cate, by=data_cate$idxpopdens, 
                           bycolors=c("darkorange","blue"), randcut=1, nolink=FALSE, family=binomial(), 
                           plotxrange = c(0,80), samebinsby = TRUE, binspos = cate.bins,  
                           dotsgridmean = TRUE, nsims=50000, simsgrid=100)

# Apply the paper's plotting range and assemble the point-difference data.
rows.to.keep <- cate.points.fit$data.plot$`Group 1`$data.dots$x<80
cate.point.estimate <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"fit"] - cate.points.fit$data.plot$`Group 0`$data.dots[rows.to.keep,"fit"]
x.for.plot <- cate.points.fit$data.plot$`Group 1`$data.dots[rows.to.keep,"x"]
cate.plot.data <- data.frame(x.for.plot,cate.point.estimate)

# Obtain the 95% uniform difference band on the common knots. With no explicit
# at= value, binspwc evaluates both groups at the pooled mean of w_cate.
cate <- binspwc(data_cate$uninsuredRate, data_cate$perCapitaIncome, w=w_cate, by=data_cate$idxpopdens, estmethod = "glm", randcut=1, 
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





### Formally test the above comparisons ###
# binspwc tests equality of the two group functions uniformly over x.
# First print the no-control test.
summary(
  binspwc(data_cate$uninsuredRate, data_cate$perCapitaIncome, by=data_cate$idxpopdens, estmethod = "glm", randcut=1, nolink=FALSE, family=binomial(), nsims=50000, simsgrid=100)
)
# Then print the test with the nine controls.
summary(
  binspwc(data_cate$uninsuredRate, data_cate$perCapitaIncome, w=w_cate, by=data_cate$idxpopdens, estmethod = "glm", randcut=1, nolink=FALSE, family=binomial(), nsims=50000, simsgrid=100)
)











################################################################################
# APPENDIX -- additional empirical illustrations
################################################################################



################################################################################
# FIGURE A1 - Robustness to corrupted data
################################################################################

# Add low-income outliers: these mask the Medicaid pattern for the mean but not
# for the median. Start with the original least-squares binscatter and retain its
# selected bin count so every estimator below uses the same partition size.

# Fit the original data and locate the right edge of the lowest bin.
res_orig <- binsreg(data$uninsuredRate, data$perCapitaIncome, randcut=1)
dots_orig <- res_orig$data.plot$`Group Full Sample`$data.dots
no.of.bins <- nrow(dots_orig)
cutoff.for.lowest.bin <- res_orig$data.plot$`Group Full Sample`$data.bin$right.endpoint[1]

# Corrupt 32 observations, approximately 0.1% of the analysis sample. Candidates
# are observations in the lowest bin whose outcomes exceed that bin's median.
0.001*length(data$uninsuredRate)
# Make the random selection reproducible, copy x and y, and replace selected y's by 1.
set.seed(2)
corrupt.me <- sample(x=which( (data$perCapitaIncome<cutoff.for.lowest.bin & data$uninsuredRate > median(data$uninsuredRate[data$perCapitaIncome<cutoff.for.lowest.bin]))), size=32, replace=FALSE)
y <- data$uninsuredRate
x <- data$perCapitaIncome
y[corrupt.me] <- 1

# Fit median regression and least squares to the corrupted data using the
# original selected number of bins, then extract their plot-ready dots.
res_5 <- binsqreg(y, x, randcut=1, quantile=0.5, nbins=no.of.bins)
dots_5 <- res_5$data.plot$`Group Full Sample`$data.dots

res.ls <- binsreg(y, x, nbins=no.of.bins)
dots.ls <- res.ls$data.plot$`Group Full Sample`$data.dots

# Overlay corrupted-data median and least-squares fits with original least squares.
png('graphs/FigA1a_corrupt_low_values.png')
  plot(dots_5$x, dots_5$fit, col='blue', pch=2, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
  points(dots_orig$x, dots_orig$fit, col="forestgreen", pch=4, cex=2)
  points(dots.ls$x, dots.ls$fit, col="darkorange", pch=16)
  legend("topright", c("Median Regression", "Least squares in original data", "Least squares using corrupted data"), pch=c(2,4,16), col=c("blue", "forestgreen", "darkorange"))
dev.off()



# Repeat the exercise at high incomes, where corruption reverses the downward
# least-squares pattern. Refit the original data to recover its bin structure.
res_orig <- binsreg(data$uninsuredRate, data$perCapitaIncome, randcut=1)
dots_orig <- res_orig$data.plot$`Group Full Sample`$data.dots
no.of.bins <- nrow(dots_orig)
# Set the cutoff at the left edge of the highest three bins.
cutoff.for.high.bin <- res_orig$data.plot$`Group Full Sample`$data.bin$left.endpoint[no.of.bins-2]

# Select 32 above-median outcomes from those bins. Resetting the seed makes this
# second random selection reproducible; selected outcomes become 1.
set.seed(2)
corrupt.me <- sample(x=which( (data$perCapitaIncome>cutoff.for.high.bin & data$uninsuredRate > median(data$uninsuredRate[data$perCapitaIncome>cutoff.for.high.bin]))), size=32, replace=FALSE)
y <- data$uninsuredRate
x <- data$perCapitaIncome
y[corrupt.me] <- 1

# Fit both centrality measures with the original number of bins.
res_5 <- binsqreg(y, x, randcut=1, quantile=0.5, nbins=no.of.bins)
dots_5 <- res_5$data.plot$`Group Full Sample`$data.dots

res.ls <- binsreg(y, x, nbins=no.of.bins)
dots.ls <- res.ls$data.plot$`Group Full Sample`$data.dots

# Overlay the three high-income results using the same legend as Panel (a).
png('graphs/FigA1b_corrupt_high_values.png')
  plot(dots_5$x, dots_5$fit, col='blue', pch=2, xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), ylim=c(0,0.4), cex.lab=1.25, cex.axis=1.25)
  points(dots_orig$x, dots_orig$fit, col="forestgreen", pch=4, cex=2)
  points(dots.ls$x, dots.ls$fit, col="darkorange", pch=16)
  legend("topright", c("Median Regression", "Least squares in original data", "Least squares using corrupted data"), pch=c(2,4,16), col=c("blue", "forestgreen", "darkorange"))
dev.off()





################################################################################
# FIGURE A2 - least squares gives negative predictions
################################################################################



## Logit QMLE at extreme control-variable evaluation points ##

# Fit at the default evaluation point (the sample mean of w) and retain the
# selected bin count and fitted dots for the thick black reference curve.
fit <- binsglm(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, family=binomial())
dots.wmean <- fit$data.plot$`Group Full Sample`$data.dots$fit
bins <- fit$opt$nbins.by
# For each of nine controls, take its observed minimum and maximum. expand.grid
# forms all 2^9=512 combinations used as alternative values of w in at=.
all.points <- expand.grid(lapply(as.data.frame(w), range, na.rm=TRUE))
dots.x <- fit$data.plot$`Group Full Sample`$data.dots$x
# Allocate one column of fitted values for each of the 512 combinations.
dots.y <- matrix(NA, nrow=bins, ncol=nrow(all.points))
# Refit at every evaluation point while holding the selected bin count fixed.
for (j in 1:nrow(all.points)) {
  dots.y[,j] <- binsglm(data$uninsuredRate, data$perCapitaIncome, w=w, family=binomial(), at=unlist(all.points[j,]), nbins = bins)$data.plot$`Group Full Sample`$data.dots$fit
}
# Extend each fitted curve horizontally to x=0 and x=80 for display. The first
# plot call creates an empty canvas; the loop then draws all 512 light-blue lines.
dots.x <- c(0,dots.x,80)
dots.y <- rbind(dots.y[1,], dots.y, dots.y[bins,])
png('graphs/FigA2b_LogitQMLE.png')
  plot(dots.x, dots.y[,1], col=1, ylim=c(-0.25, 1), pch=NA,  xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), cex.lab=1.25, cex.axis=1.25)
  for (j in 1:nrow(all.points)) {
    lines(dots.x, dots.y[,j], col="lightblue", lwd=1)
  }
# Add the sample-mean curve in black and the zero reference line in red.
  lines(dots.x, c(dots.wmean[1], dots.wmean, dots.wmean[bins]), lwd=2)
  abline(h=0, lwd=1.5, col="red")
  legend("topright", c("Sample mean", "Category combinations"), lty=c(1,1), lwd=2, col=c("black", "lightblue"))
dev.off()



## Least squares at the same type of extreme evaluation points ##

# Repeat the complete exercise using Gaussian/identity (least-squares) fitting.
# First obtain its IMSE-selected bin count and sample-mean reference curve.
fit <- binsglm(data$uninsuredRate, data$perCapitaIncome, w=w, randcut=1, family=gaussian())
dots.wmean <- fit$data.plot$`Group Full Sample`$data.dots$fit
bins <- fit$opt$nbins.by
# Recreate the 512 min/max combinations and allocate their fitted values.
all.points <- expand.grid(lapply(as.data.frame(w), range, na.rm=TRUE))
dots.x <- fit$data.plot$`Group Full Sample`$data.dots$x
dots.y <- matrix(NA, nrow=bins, ncol=nrow(all.points))
# Refit least squares at each evaluation point with the bin count held fixed.
for (j in 1:nrow(all.points)) {
  dots.y[,j] <- binsglm(data$uninsuredRate, data$perCapitaIncome, w=w, family=gaussian(), at=unlist(all.points[j,]), nbins = bins)$data.plot$`Group Full Sample`$data.dots$fit
}
# Extend the curves to the plotting limits and draw all 512 in light blue.
dots.x <- c(0,dots.x,80)
dots.y <- rbind(dots.y[1,], dots.y, dots.y[bins,])
png('graphs/FigA2a_LeastSquaresNegative.png')
  plot(dots.x, dots.y[,1], col=1, ylim=c(-0.25, 1), pch=NA,  xlab="Per Capita Income ($ Thousands)", ylab="Percent Uninsured", xlim=c(0,80), cex.lab=1.25, cex.axis=1.25)
  for (j in 1:nrow(all.points)) {
    lines(dots.x, dots.y[,j], col="lightblue", lwd=1)
  }
# Add the sample-mean fit and zero reference line.
  lines(dots.x, c(dots.wmean[1], dots.wmean, dots.wmean[bins]), lwd=2)
  abline(h=0, lwd=1.5, col="red")
  legend("topright", c("Sample mean", "Category combinations"), lty=c(1,1), lwd=2, col=c("black", "lightblue"))
dev.off()


# Report how many least-squares profiles are negative at the right plotting
# endpoint (x=80), followed by the associated min/max control combinations.
negatives <- which(dots.y[bins+2,] < 0)
length(negatives)
all.points[which(dots.y[bins+2,] < 0),]






################################################################################
# FIGURE A3 - confidence band and quantiles
################################################################################

# Contrast estimation uncertainty for the conditional mean with outcome spread
# between the conditional 10th and 90th percentiles. The quantile targets match
# Figure 2, but here all three estimates use the mean fit's selected bin count so
# their dots share a common partition.

# Without controls, fit the mean by the default Gaussian/identity model and
# request an 80% uniform band. Record its selected bin count for both quantiles.
fit.mean <- binsglm(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T, plotxrange = c(0,80), level=80, nsims=50000, simsgrid=100)
no.of.bins <- nrow(fit.mean$data.plot$`Group Full Sample`$data.bin)
qfit1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=0.1, nbins=no.of.bins)$data.plot$`Group Full Sample`$data.dots

qfit2 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=0.9, nbins=no.of.bins)$data.plot$`Group Full Sample`$data.dots

# Put the two quantile series in data frames for overlay on bins_plot.
q_plotdata1 <- data.frame(x1=qfit1$x,q1=qfit1$fit)
q_plotdata2 <- data.frame(x2=qfit2$x,q2=qfit2$fit)

tmp.plot <- fit.mean$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  geom_point(data=q_plotdata1, aes(x=x1, y=q1), color=c('darkgrey')) + 
  geom_point(data=q_plotdata2, aes(x=x2, y=q2), color=c('darkgrey')) + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(0,.4) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
png('graphs/FigA3a_QuantilesAndBandNoControls.png')
  plot(tmp.plot)
dev.off()


# Repeat with controls; each estimator is evaluated at the sample mean of w.
fit.mean <- binsglm(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T, w=w, plotxrange = c(0,80), level=80, nsims=50000, simsgrid=100)
no.of.bins <- nrow(fit.mean$data.plot$`Group Full Sample`$data.bin)
qfit1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=0.1, w=w, nbins=no.of.bins)$data.plot$`Group Full Sample`$data.dots

qfit2 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=0.9, w=w, nbins=no.of.bins)$data.plot$`Group Full Sample`$data.dots

# Assemble and overlay the controlled quantile dots on the controlled mean band.
q_plotdata1 <- data.frame(x1=qfit1$x,q1=qfit1$fit)
q_plotdata2 <- data.frame(x2=qfit2$x,q2=qfit2$fit)

tmp.plot <- fit.mean$bins_plot + geom_boxplot() + xlab('Per Capita Income ($ Thousands)') + 
  geom_point(data=q_plotdata1, aes(x=x1, y=q1), color=c('darkgrey')) + 
  geom_point(data=q_plotdata2, aes(x=x2, y=q2), color=c('darkgrey')) + 
  ylab('Percent Uninsured') + xlim(0,80) + ylim(0,.4) + 
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
png('graphs/FigA3b_QuantilesAndBandYesControls.png')
  plot(tmp.plot)
dev.off()
