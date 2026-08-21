################################################################################
# Replication "Nonlinear Binscatter Methods"
# Cattaneo, Crump, Farrell and Feng (2026)
# ACS Application
################################################################################

# Run this script from the replication folder. It reads the two CSV files in
# that folder, writes figures to graphs/, and prints the Table 1/Figure 4 tests
# and a few diagnostics to the Python console. The graphs/ directory must exist.
#
# Main binsreg conventions used below:
# - dist="Binomial", link="Logit" fits logistic QMLE; nolink=False reports
#   results after the inverse-logit transformation, on the outcome scale.
# - Omitting nbins requests the package's data-driven IMSE bin selector.
# - randcut=1 uses the full sample during bin selection rather than a subsample.
# - cb=True requests a uniform confidence band; nsims and simsgrid control its
#   simulation draws and within-bin evaluation grid.

# Load binscatter estimation, plotting, and data-selection functions.
import itertools

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from binsreg import binsglm, binspwc, binsqreg, binsreg, binstest

# Read the main zip-code-level ACS analysis file.
data = pd.read_csv("CCFF_2026_ACS_1.csv")

# Convert uninsured percentages to proportions and income dollars to $1,000s.
data["uninsuredRate"] = data["uninsuredRate"] / 100
data["perCapitaIncome"] = data["perCapitaIncome"] / 1000

# Keep observations with nonmissing outcome and income. Controlled analyses
# below also use complete cases for the controls, as handled by binsreg.
data = data.loc[data[["uninsuredRate", "perCapitaIncome"]].notna().all(axis=1)].copy()

# Store the nine controls described in the paper as the matrix/data frame w.
w = data[[
    "percentBachelorsEdu", "medianAge", "percentHsEdu", "ueRate",
    "meanHouseholdSize", "percentNoWeb", "percentArmedForces",
    "percentEnglishOnly", "percent65andOlder"
]].copy()



################################################################################
# FIGURE 1(a) Plain scatter
################################################################################
# Draw the raw outcome-income scatter and save it as a PNG.
plt.figure()
plt.scatter(data["perCapitaIncome"], data["uninsuredRate"], color="navy", s=12)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.tight_layout()
plt.savefig("graphs/Fig1a_Census_Scatter.png")
plt.close()


################################################################################
# FIGURE 1(b) Demonstration of scatter to binscatter
################################################################################
# Fit a piecewise-constant logistic binscatter using a deliberately fixed J=10.
# line=(0,0) also returns the piecewise-constant line used to recover bin knots.
res = binsglm(data["uninsuredRate"], data["perCapitaIncome"], nbins=10,
              line=(0, 0), dist="Binomial", link="Logit", noplot=True)

# data_plot contains plot-ready coordinates; extract the fitted dots and knots.
line = res.data_plot[0].line.copy()
dot = res.data_plot[0].dots.copy()

# Retain the knot rows and draw their x-coordinates as vertical dashed lines.
line = line.loc[line["isknot"] == 1, "x"].tolist()

plt.figure()
plt.scatter(data["perCapitaIncome"], data["uninsuredRate"], color="gray", s=12)
plt.scatter(dot["x"], dot["fit"], color="blue", s=24)
for knot in line:
    plt.axvline(knot, color="black", linestyle="--", linewidth=1)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig1b_Census_ScatterAndBinscatter.png")
plt.close()

################################################################################
# FIGURE 1(c) Demonstration of conventional binscatter plot
################################################################################
# Retain J=10 and add the global first-degree logistic fit used for comparison.
res = binsglm(data["uninsuredRate"], data["perCapitaIncome"], nbins=10,
              polyreg=1, plotxrange=(0, 80), dist="Binomial", link="Logit",
              noplot=True)
# Extract plot-ready coordinates for the global line and binned point estimates.
line = res.data_plot[0].poly.copy()
dots = res.data_plot[0].dots.copy()

plt.figure()
plt.scatter(dots["x"], dots["fit"], color="blue", s=24)
plt.plot(line["x"], line["fit"], color="forestgreen", linewidth=1.5)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig1c_Census_BinscatterAndLine.png")
plt.close()


################################################################################
# FIGURE 1(d) IMSE optimal number of bins
################################################################################
# Repeat Figure 1(c) but let binsglm select the IMSE-optimal number of bins.
res = binsglm(data["uninsuredRate"], data["perCapitaIncome"], polyreg=1,
              plotxrange=(0, 80), dist="Binomial", link="Logit", randcut=1,
              noplot=True)

line = res.data_plot[0].poly.copy()
dots = res.data_plot[0].dots.copy()
plt.figure()
plt.scatter(dots["x"], dots["fit"], color="blue", s=24)
plt.plot(line["x"], line["fit"], color="forestgreen", linewidth=1.5)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig1d_Census_Binscatter_J_IMSE.png")
plt.close()




################################################################################
# FIGURE 2 Quantiles
################################################################################

# Estimate the 10th, 50th, and 90th conditional quantiles without controls.
res_1 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                 cb=False, quantile=0.1, plotxrange=(0, 80), noplot=True)
res_5 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                 cb=False, quantile=0.5, plotxrange=(0, 80), noplot=True)
res_9 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                 cb=False, quantile=0.9, plotxrange=(0, 80), noplot=True)

dots_1 = res_1.data_plot[0].dots.copy()
dots_5 = res_5.data_plot[0].dots.copy()
dots_9 = res_9.data_plot[0].dots.copy()


# Repeat with the nine controls. The suffix "a" denotes controlled results.
res_1 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                 randcut=1, cb=False, quantile=0.1, plotxrange=(0, 80),
                 noplot=True)
res_5 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                 randcut=1, cb=False, quantile=0.5, plotxrange=(0, 80),
                 noplot=True)
res_9 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                 randcut=1, cb=False, quantile=0.9, plotxrange=(0, 80),
                 noplot=True)

dots_1a = res_1.data_plot[0].dots.copy()
dots_5a = res_5.data_plot[0].dots.copy()
dots_9a = res_9.data_plot[0].dots.copy()


# Overlay the three quantile estimates, first without and then with controls.
plt.figure()
plt.scatter(dots_9["x"], dots_9["fit"], color="gray", label="90th", s=24)
plt.scatter(dots_5["x"], dots_5["fit"], color="black", label="50th", s=24)
plt.scatter(dots_1["x"], dots_1["fit"], color="gray", label="10th", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 100)
plt.ylim(0, 0.4)
handles, labels = plt.gca().get_legend_handles_labels()
plt.legend([handles[2], handles[1], handles[0]], [labels[2], labels[1], labels[0]],
           loc="upper right")
plt.tight_layout()
plt.savefig("graphs/Fig2a_Census_BinscatterQuantilesNoControls.png")
plt.close()

plt.figure()
plt.scatter(dots_9a["x"], dots_9a["fit"], color="gray", label="90th", s=24)
plt.scatter(dots_5a["x"], dots_5a["fit"], color="black", label="50th", s=24)
plt.scatter(dots_1a["x"], dots_1a["fit"], color="gray", label="10th", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 100)
plt.ylim(0, 0.4)
handles, labels = plt.gca().get_legend_handles_labels()
plt.legend([handles[2], handles[1], handles[0]], [labels[2], labels[1], labels[0]],
           loc="upper right")
plt.tight_layout()
plt.savefig("graphs/Fig2b_Census_BinscatterQuantilesYesControls.png")
plt.close()




################################################################################
# FIGURE 3 - Confidence band around mean fcn and marginal effect
################################################################################
# Panel (a): logistic-QMLE conditional mean with a 95% uniform confidence band.
glm_fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                  cb=True, nolink=False, dist="Binomial", link="Logit",
                  plotxrange=(0, 80), nsims=50000, simsgrid=100, level=95,
                  noplot=True)
dots = glm_fit.data_plot[0].dots.copy()
band = glm_fit.data_plot[0].cb.copy()
plt.figure()
plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                 band["cb_r"].to_numpy(), color="lightgray")
plt.scatter(dots["x"], dots["fit"], color="blue", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig3a_Census_ConfidenceBand_Mean.png")
plt.close()



# Panel (b): first derivative of the fitted outcome-scale function. dots=(1,1)
# uses a continuous piecewise-linear fit for the plotted marginal-effect dots.
glm_fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                  cb=True, dots=(1, 1), deriv=1, nolink=False,
                  dist="Binomial", link="Logit", plotxrange=(0, 80),
                  nsims=50000, simsgrid=100, level=95, noplot=True)
dots = glm_fit.data_plot[0].dots.copy()
band = glm_fit.data_plot[0].cb.copy()
plt.figure()
plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                 band["cb_r"].to_numpy(), color="lightgray")
plt.scatter(dots["x"], dots["fit"], color="blue", s=24)
plt.axhline(0, color="gray", linestyle="--", linewidth=1)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(-0.025, 0.025)
plt.tight_layout()
plt.savefig("graphs/Fig3b_Census_ConfidenceBand_MarginalEffect.png")
plt.close()








################################################################################
# TABLE 1 - Specification & Shape Testing
################################################################################


# Define the restricted sample above the Medicaid-related income cutoff.
# 16.248 equals 1.38 times the 2013--2017 average federal poverty line,
# expressed in thousands of dollars; see the paper for its construction.
upper = data["perCapitaIncome"] > 16.248


### Test against linear in X ###
# Each binstest call prints a sup-norm test of the stated global polynomial
# index specification. nolink=True puts both fitted functions on the logit
# index scale. The four calls cover full/restricted samples without/with w.
# Full sample, no controls.
binstest(data["uninsuredRate"], data["perCapitaIncome"], testmodelpoly=1,
        estmethod="glm", nolink=True, dist="Binomial", link="Logit",
        randcut=1, nsims=50000, simsgrid=100).summary()
# Full sample, with controls evaluated at their sample means.
binstest(data["uninsuredRate"], data["perCapitaIncome"], w=w,
        testmodelpoly=1, estmethod="glm", nolink=True, dist="Binomial",
        link="Logit", randcut=1, nsims=50000, simsgrid=100).summary()

# Restricted sample, no controls.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        testmodelpoly=1, estmethod="glm", nolink=True, dist="Binomial",
        link="Logit", randcut=1, nsims=50000, simsgrid=100).summary()
# Restricted sample, with controls evaluated at their sample means.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        w=w.loc[upper], testmodelpoly=1, estmethod="glm", nolink=True,
        dist="Binomial", link="Logit", randcut=1, nsims=50000,
        simsgrid=100).summary()

### Test against cubic in X ###
# Repeat the same four comparisons against a global cubic specification.
# Full sample, no controls.
binstest(data["uninsuredRate"], data["perCapitaIncome"], testmodelpoly=3,
        estmethod="glm", nolink=True, dist="Binomial", link="Logit",
        randcut=1, nsims=50000, simsgrid=100).summary()
# Full sample, with controls.
binstest(data["uninsuredRate"], data["perCapitaIncome"], w=w,
        testmodelpoly=3, estmethod="glm", nolink=True, dist="Binomial",
        link="Logit", randcut=1, nsims=50000, simsgrid=100).summary()

# Restricted sample, no controls.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        testmodelpoly=3, estmethod="glm", nolink=True, dist="Binomial",
        link="Logit", randcut=1, nsims=50000, simsgrid=100).summary()
# Restricted sample, with controls.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        w=w.loc[upper], testmodelpoly=3, estmethod="glm", nolink=True,
        dist="Binomial", link="Logit", randcut=1, nsims=50000,
        simsgrid=100).summary()


### Monotonicity ###
# Test the null that the logit index's first derivative is everywhere
# nonpositive. testshapel=0 specifies the zero upper boundary for that
# one-sided test.
# Full sample, no controls.
binstest(data["uninsuredRate"], data["perCapitaIncome"], testshapel=0,
        deriv=1, estmethod="glm", nolink=True, dist="Binomial", link="Logit",
        randcut=1, nsims=50000, simsgrid=100).summary()
# Full sample, with controls.
binstest(data["uninsuredRate"], data["perCapitaIncome"], w=w, testshapel=0,
        deriv=1, estmethod="glm", nolink=True, dist="Binomial", link="Logit",
        randcut=1, nsims=50000, simsgrid=100).summary()

# Restricted sample, no controls.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        testshapel=0, deriv=1, estmethod="glm", nolink=True, dist="Binomial",
        link="Logit", randcut=1, nsims=50000, simsgrid=100).summary()
# Restricted sample, with controls.
binstest(data.loc[upper, "uninsuredRate"], data.loc[upper, "perCapitaIncome"],
        w=w.loc[upper], testshapel=0, deriv=1, estmethod="glm", nolink=True,
        dist="Binomial", link="Logit", randcut=1, nsims=50000,
        simsgrid=100).summary()







################################################################################
# FIGURE 4 - Group comparisons
################################################################################

# Population density comes in a separate file and is unavailable for some areas.
# Density source: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
# In idxpopdens, Group 1 is low-density (<100 people/sq. mile) and Group 0 is
# high-density. Rescale outcome and income exactly as in the main data.
data_cate = pd.read_csv("CCFF_2026_ACS_2.csv")
data_cate["uninsuredRate"] = data_cate["uninsuredRate"] / 100
data_cate["perCapitaIncome"] = data_cate["perCapitaIncome"] / 1000
# Construct the same nine-control set for the density-comparison sample.
# The commented line records an earlier specification and is not run.
# w_cate = data_cate[["percentBachelorsEdu", "medianAge", "percentHsEdu", "ueRate"]]
w_cate = data_cate[[
    "percentBachelorsEdu", "medianAge", "percentHsEdu", "ueRate",
    "meanHouseholdSize", "percentNoWeb", "percentArmedForces",
    "percentEnglishOnly", "percent65andOlder"
]].copy()


## Two groups, no controls ##
# by= fits the two density groups separately, including group-specific binning.
glm_fit = binsglm(data_cate["uninsuredRate"], data_cate["perCapitaIncome"],
                  by=data_cate["idxpopdens"], bycolors=("darkorange", "blue"),
                  randcut=1, cb=True, nolink=False, dist="Binomial",
                  link="Logit", plotxrange=(0, 80), level=95, nsims=50000,
                  simsgrid=100, noplot=True)
plt.figure()
for group_plot, color in zip(glm_fit.data_plot, ("darkorange", "blue")):
    band = group_plot.cb
    dots = group_plot.dots
    plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                     band["cb_r"].to_numpy(), color=color, alpha=0.18)
    plt.scatter(dots["x"], dots["fit"], color=color, s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig4a_Census_GroupComparisonNoControls.png")
plt.close()


## Group-difference (CATE) command, no controls ##
# Print which group has fewer bins as a diagnostic (1 means Group 0; 2 means
# Group 1), then deliberately use the low-density partition, as in the paper.
print(np.argmin([len(glm_fit.data_plot[0].data_bin),
                 len(glm_fit.data_plot[1].data_bin)]) + 1)
cate_bins = glm_fit.data_plot[1].data_bin["left_endpoint"].iloc[1:].to_numpy()

# Python binspwc implements the comparable pairwise test on the common knots,
# but does not return the pairwise-difference confidence-band plot available in
# R. As requested, retain and report the comparable command without making a
# CATE plot or creating Figure 4(c).
cate = binspwc(data_cate["uninsuredRate"], data_cate["perCapitaIncome"],
               by=data_cate["idxpopdens"], estmethod="glm", randcut=1,
               nolink=False, dist="Binomial", link="Logit", nsims=50000,
               simsgrid=100, samebinsby=True, binspos=cate_bins)
cate.summary()



## Two groups, with controls ##
# Repeat the group-specific curves with the nine controls included. With by= and
# no explicit at= value, binsglm evaluates each curve at that group's mean w.
glm_fit = binsglm(data_cate["uninsuredRate"], data_cate["perCapitaIncome"],
                  w=w_cate, by=data_cate["idxpopdens"],
                  bycolors=("darkorange", "blue"), randcut=1, cb=True,
                  nolink=False, dist="Binomial", link="Logit",
                  plotxrange=(0, 80), level=95, nsims=50000, simsgrid=100,
                  noplot=True)
plt.figure()
for group_plot, color in zip(glm_fit.data_plot, ("darkorange", "blue")):
    band = group_plot.cb
    dots = group_plot.dots
    plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                     band["cb_r"].to_numpy(), color=color, alpha=0.18)
    plt.scatter(dots["x"], dots["fit"], color=color, s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/Fig4b_Census_GroupComparisonYesControls.png")
plt.close()



## Group-difference (CATE) command, with controls ##
# Print the fewer-bin group (1=Group 0; 2=Group 1) as a diagnostic and impose
# the low-density (Group 1) partition for the paper.
print(np.argmin([len(glm_fit.data_plot[0].data_bin),
                 len(glm_fit.data_plot[1].data_bin)]) + 1)
cate_bins = glm_fit.data_plot[1].data_bin["left_endpoint"].iloc[1:].to_numpy()

# Python binspwc uses the pooled mean of w_cate when at= is omitted. It does not
# return the R pairwise-difference band plot, so retain the comparable command
# without making a CATE plot or creating Figure 4(d).
cate = binspwc(data_cate["uninsuredRate"], data_cate["perCapitaIncome"],
               w=w_cate, by=data_cate["idxpopdens"], estmethod="glm",
               randcut=1, nolink=False, dist="Binomial", link="Logit",
               nsims=50000, simsgrid=100, samebinsby=True, binspos=cate_bins)
cate.summary()





### Formally test the above comparisons ###
# binspwc tests equality of the two group functions uniformly over x.
# First print the no-control test.
binspwc(data_cate["uninsuredRate"], data_cate["perCapitaIncome"],
        by=data_cate["idxpopdens"], estmethod="glm", randcut=1, nolink=False,
        dist="Binomial", link="Logit", nsims=50000, simsgrid=100).summary()
# Then print the test with the nine controls.
binspwc(data_cate["uninsuredRate"], data_cate["perCapitaIncome"], w=w_cate,
        by=data_cate["idxpopdens"], estmethod="glm", randcut=1, nolink=False,
        dist="Binomial", link="Logit", nsims=50000, simsgrid=100).summary()










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
res_orig = binsreg(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                   noplot=True)
dots_orig = res_orig.data_plot[0].dots.copy()
no_of_bins = len(dots_orig)
cutoff_for_lowest_bin = res_orig.data_plot[0].data_bin["right.endpoint"].iloc[0]

# Corrupt 32 observations, approximately 0.1% of the analysis sample. Candidates
# are observations in the lowest bin whose outcomes exceed that bin's median.
print(0.001 * len(data["uninsuredRate"]))
# Make the random selection reproducible, copy x and y, and replace selected y's by 1.
rng = np.random.default_rng(2)
lowest_median = data.loc[data["perCapitaIncome"] < cutoff_for_lowest_bin,
                         "uninsuredRate"].median()
corrupt_candidates = data.index[
    (data["perCapitaIncome"] < cutoff_for_lowest_bin) &
    (data["uninsuredRate"] > lowest_median)
].to_numpy()
corrupt_me = rng.choice(corrupt_candidates, size=32, replace=False)
y = data["uninsuredRate"].copy()
x = data["perCapitaIncome"].copy()
y.loc[corrupt_me] = 1

# Fit median regression and least squares to the corrupted data using the
# original selected number of bins, then extract their plot-ready dots.
res_5 = binsqreg(y, x, randcut=1, quantile=0.5, nbins=no_of_bins, noplot=True)
dots_5 = res_5.data_plot[0].dots.copy()

res_ls = binsreg(y, x, nbins=no_of_bins, noplot=True)
dots_ls = res_ls.data_plot[0].dots.copy()

# Overlay corrupted-data median and least-squares fits with original least squares.
plt.figure()
plt.scatter(dots_5["x"], dots_5["fit"], color="blue", marker="^", facecolors="none",
            label="Median Regression", s=34)
plt.scatter(dots_orig["x"], dots_orig["fit"], color="forestgreen", marker="x",
            label="Least squares in original data", s=45)
plt.scatter(dots_ls["x"], dots_ls["fit"], color="darkorange", marker="o",
            label="Least squares using corrupted data", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.legend(loc="upper right")
plt.tight_layout()
plt.savefig("graphs/FigA1a_corrupt_low_values.png")
plt.close()



# Repeat the exercise at high incomes, where corruption reverses the downward
# least-squares pattern. Refit the original data to recover its bin structure.
res_orig = binsreg(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                   noplot=True)
dots_orig = res_orig.data_plot[0].dots.copy()
no_of_bins = len(dots_orig)

# Set the cutoff at the left edge of the highest three bins.
cutoff_for_high_bin = res_orig.data_plot[0].data_bin["left_endpoint"].iloc[no_of_bins - 3]

# Select 32 above-median outcomes from those bins. Resetting the seed makes this
# second random selection reproducible; selected outcomes become 1.
rng = np.random.default_rng(2)
high_median = data.loc[data["perCapitaIncome"] > cutoff_for_high_bin,
                       "uninsuredRate"].median()
corrupt_candidates = data.index[
    (data["perCapitaIncome"] > cutoff_for_high_bin) &
    (data["uninsuredRate"] > high_median)
].to_numpy()
corrupt_me = rng.choice(corrupt_candidates, size=32, replace=False)
y = data["uninsuredRate"].copy()
x = data["perCapitaIncome"].copy()
y.loc[corrupt_me] = 1

# Fit both centrality measures with the original number of bins.
res_5 = binsqreg(y, x, randcut=1, quantile=0.5, nbins=no_of_bins, noplot=True)
dots_5 = res_5.data_plot[0].dots.copy()

res_ls = binsreg(y, x, nbins=no_of_bins, noplot=True)
dots_ls = res_ls.data_plot[0].dots.copy()

# Overlay the three high-income results using the same legend as Panel (a).
plt.figure()
plt.scatter(dots_5["x"], dots_5["fit"], color="blue", marker="^", facecolors="none",
            label="Median Regression", s=34)
plt.scatter(dots_orig["x"], dots_orig["fit"], color="forestgreen", marker="x",
            label="Least squares in original data", s=45)
plt.scatter(dots_ls["x"], dots_ls["fit"], color="darkorange", marker="o",
            label="Least squares using corrupted data", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.legend(loc="upper right")
plt.tight_layout()
plt.savefig("graphs/FigA1b_corrupt_high_values.png")
plt.close()





################################################################################
# FIGURE A2 - least squares gives negative predictions
################################################################################

## Logit QMLE at extreme control-variable evaluation points ##

# Fit at the default evaluation point (the sample mean of w) and retain the
# selected bin count and fitted dots for the thick black reference curve.
fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"], w=w,
              randcut=1, dist="Binomial", link="Logit", noplot=True)
dots_wmean = fit.data_plot[0].dots["fit"].to_numpy()
bins = fit.options.nbins_by[0]
# For each of nine controls, take its observed minimum and maximum. The reversed
# product order matches R's expand.grid convention that the first control varies
# fastest, and forms all 2^9=512 alternative values of w for at=.
control_ranges = [(w[column].min(), w[column].max()) for column in w.columns]
all_points = pd.DataFrame(
    [values[::-1] for values in itertools.product(*control_ranges[::-1])],
    columns=w.columns
)
dots_x = fit.data_plot[0].dots["x"].to_numpy()
# Allocate one column of fitted values for each of the 512 combinations.
dots_y = np.full((bins, len(all_points)), np.nan)
# Refit at every evaluation point while holding the selected bin count fixed.
for j in range(len(all_points)):
    extreme_fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"],
                          w=w, dist="Binomial", link="Logit",
                          at=all_points.iloc[j].to_numpy(), nbins=bins,
                          noplot=True)
    dots_y[:, j] = extreme_fit.data_plot[0].dots["fit"].to_numpy()
# Extend each fitted curve horizontally to x=0 and x=80 for display, then
# draw all 512 light-blue lines.
dots_x = np.concatenate(([0], dots_x, [80]))
dots_y = np.vstack((dots_y[0, :], dots_y, dots_y[-1, :]))
plt.figure()
for j in range(len(all_points)):
    plt.plot(dots_x, dots_y[:, j], color="lightblue", linewidth=1)
# Add the sample-mean curve in black and the zero reference line in red.
plt.plot(dots_x, np.concatenate(([dots_wmean[0]], dots_wmean,
                                 [dots_wmean[-1]])), color="black",
         linewidth=2, label="Sample mean")
plt.plot([], [], color="lightblue", linewidth=2, label="Category combinations")
plt.axhline(0, color="red", linewidth=1.5)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(-0.25, 1)
plt.legend(loc="upper right")
plt.tight_layout()
plt.savefig("graphs/FigA2b_LogitQMLE.png")
plt.close()



## Least squares at the same type of extreme evaluation points ##

# Repeat the complete exercise using Gaussian/identity (least-squares)
# fitting. First obtain its selected bin count and sample-mean curve.
fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"], w=w,
              randcut=1, dist="Gaussian", noplot=True)
dots_wmean = fit.data_plot[0].dots["fit"].to_numpy()
bins = fit.options.nbins_by[0]
# Recreate the 512 min/max combinations and allocate their fitted values.
control_ranges = [(w[column].min(), w[column].max()) for column in w.columns]
all_points = pd.DataFrame(
    [values[::-1] for values in itertools.product(*control_ranges[::-1])],
    columns=w.columns
)
dots_x = fit.data_plot[0].dots["x"].to_numpy()
dots_y = np.full((bins, len(all_points)), np.nan)
# Refit least squares at each evaluation point with the bin count held fixed.
for j in range(len(all_points)):
    extreme_fit = binsglm(data["uninsuredRate"], data["perCapitaIncome"],
                          w=w, dist="Gaussian",
                          at=all_points.iloc[j].to_numpy(), nbins=bins,
                          noplot=True)
    dots_y[:, j] = extreme_fit.data_plot[0].dots["fit"].to_numpy()
# Extend the curves to the plotting limits and draw all 512 in light blue.
dots_x = np.concatenate(([0], dots_x, [80]))
dots_y = np.vstack((dots_y[0, :], dots_y, dots_y[-1, :]))
plt.figure()
for j in range(len(all_points)):
    plt.plot(dots_x, dots_y[:, j], color="lightblue", linewidth=1)
# Add the sample-mean fit and zero reference line.
plt.plot(dots_x, np.concatenate(([dots_wmean[0]], dots_wmean,
                                 [dots_wmean[-1]])), color="black",
         linewidth=2, label="Sample mean")
plt.plot([], [], color="lightblue", linewidth=2, label="Category combinations")
plt.axhline(0, color="red", linewidth=1.5)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(-0.25, 1)
plt.legend(loc="upper right")
plt.tight_layout()
plt.savefig("graphs/FigA2a_LeastSquaresNegative.png")
plt.close()

# Report how many least-squares profiles are negative at the right plotting
# endpoint (x=80), followed by the associated min/max control combinations.
negatives = np.flatnonzero(dots_y[-1, :] < 0)
print(len(negatives))
print(all_points.iloc[negatives])






################################################################################
# FIGURE A3 - confidence band and quantiles
################################################################################

# Contrast estimation uncertainty for the conditional mean with outcome spread
# between the conditional 10th and 90th percentiles. The quantile targets match
# Figure 2, but here all three estimates use the mean fit's selected bin count so
# their dots share a common partition.

# Without controls, fit the mean by the default Gaussian/identity model and
# request an 80% uniform band. Record its selected bin count for both quantiles.
fit_mean = binsglm(data["uninsuredRate"], data["perCapitaIncome"], randcut=1,
                   cb=True, plotxrange=(0, 80), level=80, nsims=50000,
                   simsgrid=100, noplot=True)
no_of_bins = len(fit_mean.data_plot[0].data_bin)
qfit1 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], quantile=0.1,
                 nbins=no_of_bins, noplot=True).data_plot[0].dots.copy()

qfit2 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], quantile=0.9,
                 nbins=no_of_bins, noplot=True).data_plot[0].dots.copy()

# Overlay the two quantile series on the binsglm mean and confidence band.
mean_dots = fit_mean.data_plot[0].dots.copy()
band = fit_mean.data_plot[0].cb.copy()
plt.figure()
plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                 band["cb_r"].to_numpy(), color="lightgray")
plt.scatter(mean_dots["x"], mean_dots["fit"], color="blue", s=24)
plt.scatter(qfit1["x"], qfit1["fit"], color="darkgray", s=24)
plt.scatter(qfit2["x"], qfit2["fit"], color="darkgray", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/FigA3a_QuantilesAndBandNoControls.png")
plt.close()


# Repeat with controls; each estimator is evaluated at the sample mean of w.
fit_mean = binsglm(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                   randcut=1, cb=True, plotxrange=(0, 80), level=80,
                   nsims=50000, simsgrid=100, noplot=True)
no_of_bins = len(fit_mean.data_plot[0].data_bin)
qfit1 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                 quantile=0.1, nbins=no_of_bins,
                 noplot=True).data_plot[0].dots.copy()

qfit2 = binsqreg(data["uninsuredRate"], data["perCapitaIncome"], w=w,
                 quantile=0.9, nbins=no_of_bins,
                 noplot=True).data_plot[0].dots.copy()

# Overlay the controlled quantile dots on the controlled mean band.
mean_dots = fit_mean.data_plot[0].dots.copy()
band = fit_mean.data_plot[0].cb.copy()
plt.figure()
plt.fill_between(band["x"].to_numpy(), band["cb_l"].to_numpy(),
                 band["cb_r"].to_numpy(), color="lightgray")
plt.scatter(mean_dots["x"], mean_dots["fit"], color="blue", s=24)
plt.scatter(qfit1["x"], qfit1["fit"], color="darkgray", s=24)
plt.scatter(qfit2["x"], qfit2["fit"], color="darkgray", s=24)
plt.xlabel("Per Capita Income ($ Thousands)")
plt.ylabel("Percent Uninsured")
plt.xlim(0, 80)
plt.ylim(0, 0.4)
plt.tight_layout()
plt.savefig("graphs/FigA3b_QuantilesAndBandYesControls.png")
plt.close()
