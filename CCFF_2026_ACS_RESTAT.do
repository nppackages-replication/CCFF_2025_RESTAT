********************************************************************************
* Replication "Nonlinear Binscatter Methods"
* Cattaneo, Crump, Farrell and Feng (2026)
* ACS Application
********************************************************************************

* Run this script from the replication folder. It reads the two CSV files in
* that folder, writes figures to graphs/, and prints the Table 1/Figure 4 tests
* and a few diagnostics to the Stata Results window. The graphs/ directory must
* exist.
*
* Main binsreg conventions used below:
* - family(binomial) link(logit) fits logistic QMLE; omitting nolink reports
*   results after the inverse-logit transformation, on the outcome scale.
* - Omitting nbins() requests the package's data-driven IMSE bin selector.
* - randcut(1) uses the full sample during bin selection rather than a subsample.
* - cb(T) requests a uniform confidence band; nsims() and simsgrid() control its
*   simulation draws and within-bin evaluation grid.
* - glmopt(irls) and estmethodopt(irls) use the same iteratively reweighted
*   least-squares algorithm used by R and Python for generalized linear models.

* Start from a clean workspace. Warning: this removes all existing Stata data.
clear all
set more off
set scheme s2color

* Store the nine controls described in the paper.
global controls percentBachelorsEdu medianAge percentHsEdu ueRate meanHouseholdSize percentNoWeb percentArmedForces percentEnglishOnly percent65andOlder

* Temporary datasets exist only for the current Stata session.
tempfile data_main data_cate

* Read the main zip-code-level ACS analysis file.
import delimited using "CCFF_2026_ACS_1.csv", clear case(preserve) asdouble

* Convert uninsured percentages to proportions and income dollars to $1,000s.
replace uninsuredRate = uninsuredRate / 100
replace perCapitaIncome = perCapitaIncome / 1000

* Keep observations with nonmissing outcome and income. Controlled analyses
* below also use complete cases for the controls, as handled by binsreg.
drop if missing(uninsuredRate) | missing(perCapitaIncome)
save `data_main', replace



********************************************************************************
* FIGURE 1(a) Plain scatter
********************************************************************************
* Draw the raw outcome-income scatter and save it as a PNG.
scatter uninsuredRate perCapitaIncome, mcolor(navy) msize(vsmall) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") ylabel(, nogrid) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig1a_Census_Scatter.png", replace as(png) width(800) height(600)


********************************************************************************
* FIGURE 1(b) Demonstration of scatter to binscatter
********************************************************************************
* Fit a piecewise-constant logistic binscatter using a deliberately fixed J=10.
* line(0 0) also returns the piecewise-constant line used to recover bin knots.
tempfile fig1b
binsglm uninsuredRate perCapitaIncome, nbins(10) line(0 0) family(binomial) link(logit) glmopt(irls) savedata(`fig1b') replace noplot

* Append the saved plot coordinates temporarily, recover the interior knots,
* and overlay the fitted dots and vertical dashed lines on the raw scatter.
preserve
append using `fig1b'
levelsof line_x if line_isknot == 1, local(fig1b_knots) clean
twoway (scatter uninsuredRate perCapitaIncome if inrange(perCapitaIncome,0,80) & inrange(uninsuredRate,0,.4), mcolor(gs10) msize(vsmall)) (scatter dots_fit dots_x if inrange(dots_x,0,80), mcolor(blue) msize(medsmall)), xline(`fig1b_knots', lcolor(black) lpattern(dash) lwidth(vthin)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(0 .4)) xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig1b_Census_ScatterAndBinscatter.png", replace as(png) width(800) height(600)
restore

********************************************************************************
* FIGURE 1(c) Demonstration of conventional binscatter plot
********************************************************************************
* Retain J=10 and add the global first-degree logistic fit used for comparison.
binsglm uninsuredRate perCapitaIncome, nbins(10) polyreg(1) plotxrange(0 80) plotyrange(0 .4) family(binomial) link(logit) glmopt(irls) dotsplotopt(mcolor(blue)) polyregplotopt(lcolor(forest_green) lwidth(medthick)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig1c_Census_BinscatterAndLine.png", replace as(png) width(800) height(600)


********************************************************************************
* FIGURE 1(d) IMSE optimal number of bins
********************************************************************************
* Repeat Figure 1(c) but let binsglm select the IMSE-optimal number of bins.
binsglm uninsuredRate perCapitaIncome, polyreg(1) plotxrange(0 80) plotyrange(0 .4) family(binomial) link(logit) glmopt(irls) randcut(1) dotsplotopt(mcolor(blue)) polyregplotopt(lcolor(forest_green) lwidth(medthick)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig1d_Census_Binscatter_J_IMSE.png", replace as(png) width(800) height(600)




********************************************************************************
* FIGURE 2 Quantiles
********************************************************************************

* Estimate the 10th, 50th, and 90th conditional quantiles without controls.
tempfile q1 q5 q9 q1_plot q5_plot q9_plot
binsqreg uninsuredRate perCapitaIncome, randcut(1) quantile(.1) plotxrange(0 80) savedata(`q1') replace noplot
binsqreg uninsuredRate perCapitaIncome, randcut(1) quantile(.5) plotxrange(0 80) savedata(`q5') replace noplot
binsqreg uninsuredRate perCapitaIncome, randcut(1) quantile(.9) plotxrange(0 80) savedata(`q9') replace noplot

* Put the three sets of plot-ready dots into a common temporary dataset.
preserve
use `q1', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 1
save `q1_plot', replace
use `q5', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 5
save `q5_plot', replace
use `q9', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 9
save `q9_plot', replace
use `q9_plot', clear
append using `q5_plot' `q1_plot'
twoway (scatter dots_fit dots_x if quantile_series==9 & inrange(dots_x,0,100), mcolor(gs8)) (scatter dots_fit dots_x if quantile_series==5 & inrange(dots_x,0,100), mcolor(black)) (scatter dots_fit dots_x if quantile_series==1 & inrange(dots_x,0,100), mcolor(gs8)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 100)) yscale(range(0 .4)) xlabel(0(20)100) ylabel(0(.1).4, nogrid) legend(order(3 "10th" 2 "50th" 1 "90th") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig2a_Census_BinscatterQuantilesNoControls.png", replace as(png) width(800) height(600)
restore


* Repeat with the nine controls. The suffix "a" denotes controlled results.
tempfile q1a q5a q9a q1a_plot q5a_plot q9a_plot
binsqreg uninsuredRate perCapitaIncome $controls, randcut(1) quantile(.1) plotxrange(0 80) savedata(`q1a') replace noplot
binsqreg uninsuredRate perCapitaIncome $controls, randcut(1) quantile(.5) plotxrange(0 80) savedata(`q5a') replace noplot
binsqreg uninsuredRate perCapitaIncome $controls, randcut(1) quantile(.9) plotxrange(0 80) savedata(`q9a') replace noplot

* Overlay the three controlled quantile estimates.
preserve
use `q1a', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 1
save `q1a_plot', replace
use `q5a', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 5
save `q5a_plot', replace
use `q9a', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte quantile_series = 9
save `q9a_plot', replace
use `q9a_plot', clear
append using `q5a_plot' `q1a_plot'
twoway (scatter dots_fit dots_x if quantile_series==9 & inrange(dots_x,0,100), mcolor(gs8)) (scatter dots_fit dots_x if quantile_series==5 & inrange(dots_x,0,100), mcolor(black)) (scatter dots_fit dots_x if quantile_series==1 & inrange(dots_x,0,100), mcolor(gs8)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 100)) yscale(range(0 .4)) xlabel(0(20)100) ylabel(0(.1).4, nogrid) legend(order(3 "10th" 2 "50th" 1 "90th") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig2b_Census_BinscatterQuantilesYesControls.png", replace as(png) width(800) height(600)
restore




********************************************************************************
* FIGURE 3 - Confidence band around mean fcn and marginal effect
********************************************************************************
* Panel (a): logistic-QMLE conditional mean with a 95% uniform confidence band.
binsglm uninsuredRate perCapitaIncome, randcut(1) cb(T) family(binomial) link(logit) glmopt(irls) plotxrange(0 80) plotyrange(0 .4) nsims(50000) simsgrid(100) level(95) dotsplotopt(mcolor(blue)) cbplotopt(fcolor(gs12) fintensity(40) lwidth(none)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig3a_Census_ConfidenceBand_Mean.png", replace as(png) width(800) height(600)



* Panel (b): first derivative of the fitted outcome-scale function. dots(1 1)
* uses a continuous piecewise-linear fit for the plotted marginal-effect dots.
binsglm uninsuredRate perCapitaIncome, randcut(1) cb(T) dots(1 1) deriv(1) family(binomial) link(logit) glmopt(irls) plotxrange(0 80) plotyrange(-.025 .025) nsims(50000) simsgrid(100) level(95) dotsplotopt(mcolor(blue)) cbplotopt(fcolor(gs12) fintensity(40) lwidth(none)) yline(0, lcolor(gs8) lpattern(dash)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(-.02(.01).02, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig3b_Census_ConfidenceBand_MarginalEffect.png", replace as(png) width(800) height(600)








********************************************************************************
* TABLE 1 - Specification & Shape Testing
********************************************************************************


* Define the restricted sample above the Medicaid-related income cutoff.
* 16.248 equals 1.38 times the 2013--2017 average federal poverty line,
* expressed in thousands of dollars; see the paper for its construction.
generate byte upper = perCapitaIncome > 16.248


*** Test against linear in X ***
* Each binstest call prints a sup-norm test of the stated global polynomial
* index specification. nolink puts both fitted functions on the logit index
* scale. The four calls cover full/restricted samples without/with controls.
* Full sample, no controls.
binstest uninsuredRate perCapitaIncome, testmodelpoly(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Full sample, with controls evaluated at their sample means.
binstest uninsuredRate perCapitaIncome $controls, testmodelpoly(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)

* Restricted sample, no controls.
binstest uninsuredRate perCapitaIncome if upper, testmodelpoly(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Restricted sample, with controls evaluated at their sample means.
binstest uninsuredRate perCapitaIncome $controls if upper, testmodelpoly(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)

*** Test against cubic in X ***
* Repeat the same four comparisons against a global cubic specification.
* Full sample, no controls.
binstest uninsuredRate perCapitaIncome, testmodelpoly(3) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Full sample, with controls.
binstest uninsuredRate perCapitaIncome $controls, testmodelpoly(3) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)

* Restricted sample, no controls.
binstest uninsuredRate perCapitaIncome if upper, testmodelpoly(3) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Restricted sample, with controls.
binstest uninsuredRate perCapitaIncome $controls if upper, testmodelpoly(3) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)


*** Monotonicity ***
* Test the null that the logit index's first derivative is everywhere
* nonpositive. testshapel(0) specifies the zero upper boundary for that
* one-sided test.
* Full sample, no controls.
binstest uninsuredRate perCapitaIncome, testshapel(0) deriv(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Full sample, with controls.
binstest uninsuredRate perCapitaIncome $controls, testshapel(0) deriv(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)

* Restricted sample, no controls.
binstest uninsuredRate perCapitaIncome if upper, testshapel(0) deriv(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)
* Restricted sample, with controls.
binstest uninsuredRate perCapitaIncome $controls if upper, testshapel(0) deriv(1) estmethod(glm) estmethodopt(irls) nolink family(binomial) link(logit) randcut(1) nsims(50000) simsgrid(100)







********************************************************************************
* FIGURE 4 - Group comparisons
********************************************************************************

* Population density comes in a separate file and is unavailable for some areas.
* Density source: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
* In idxpopdens, Group 1 is low-density (<100 people/sq. mile) and Group 0 is
* high-density. Rescale outcome and income exactly as in the main data.
import delimited using "CCFF_2026_ACS_2.csv", clear case(preserve) asdouble
replace uninsuredRate = uninsuredRate / 100
replace perCapitaIncome = perCapitaIncome / 1000
save `data_cate', replace

* Construct the same nine-control set for the density-comparison sample.
* The commented line records an earlier specification and is not run.
* global controls_cate percentBachelorsEdu medianAge percentHsEdu ueRate
global controls_cate $controls


** Two groups, no controls **
* by() fits the two density groups separately, including group-specific binning.
tempfile glm_nocontrols
binsglm uninsuredRate perCapitaIncome, by(idxpopdens) bycolors(dkorange blue) randcut(1) cb(T) family(binomial) link(logit) glmopt(irls) plotxrange(0 80) plotyrange(0 .4) level(95) nsims(50000) simsgrid(100) savedata(`glm_nocontrols') replace dotsplotopt(msize(small)) cbplotopt(fintensity(20) lwidth(none)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig4a_Census_GroupComparisonNoControls.png", replace as(png) width(800) height(600)


** Group-difference (CATE) command, no controls **
* Print which group has fewer bins (1=Group 0; 2=Group 1), then deliberately
* use the low-density partition, as stated in the paper.
preserve
use `glm_nocontrols', clear
quietly count if idxpopdens==0 & !missing(binid)
local bins_group0 = r(N)
quietly count if idxpopdens==1 & !missing(binid)
local bins_group1 = r(N)
display cond(`bins_group0'<=`bins_group1', 1, 2)
format lef_ep %21.15g
levelsof lef_ep if idxpopdens==1 & binid>1 & !missing(binid), local(cate_bins) clean
restore

* Stata binspwc implements the comparable pairwise test on the common knots,
* but does not return the pairwise-difference confidence-band plot available in
* R. As requested, retain the comparable command without making a CATE plot or
* creating Figure 4(c).
binspwc uninsuredRate perCapitaIncome, by(idxpopdens) estmethod(glm) estmethodopt(irls) randcut(1) family(binomial) link(logit) nsims(50000) simsgrid(100) samebinsby binspos(`cate_bins')



** Two groups, with controls **
* Repeat the group-specific curves with the nine controls included. With by()
* and no explicit at() value, binsglm evaluates each curve at its group mean.
tempfile glm_controls
binsglm uninsuredRate perCapitaIncome $controls_cate, by(idxpopdens) bycolors(dkorange blue) randcut(1) cb(T) family(binomial) link(logit) glmopt(irls) plotxrange(0 80) plotyrange(0 .4) level(95) nsims(50000) simsgrid(100) savedata(`glm_controls') replace dotsplotopt(msize(small)) cbplotopt(fintensity(20) lwidth(none)) xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/Fig4b_Census_GroupComparisonYesControls.png", replace as(png) width(800) height(600)



** Group-difference (CATE) command, with controls **
* Print the fewer-bin group (1=Group 0; 2=Group 1) and deliberately impose the
* low-density (Group 1) partition for the paper.
preserve
use `glm_controls', clear
quietly count if idxpopdens==0 & !missing(binid)
local bins_group0 = r(N)
quietly count if idxpopdens==1 & !missing(binid)
local bins_group1 = r(N)
display cond(`bins_group0'<=`bins_group1', 1, 2)
format lef_ep %21.15g
levelsof lef_ep if idxpopdens==1 & binid>1 & !missing(binid), local(cate_bins) clean
restore

* Stata binspwc uses the pooled mean of the controls when at() is omitted. It
* does not return the R difference-band plot, so retain the comparable command
* without making a CATE plot or creating Figure 4(d).
binspwc uninsuredRate perCapitaIncome $controls_cate, by(idxpopdens) estmethod(glm) estmethodopt(irls) randcut(1) family(binomial) link(logit) nsims(50000) simsgrid(100) samebinsby binspos(`cate_bins')





*** Formally test the above comparisons ***
* binspwc tests equality of the two group functions uniformly over x.
* First print the no-control test.
binspwc uninsuredRate perCapitaIncome, by(idxpopdens) estmethod(glm) estmethodopt(irls) randcut(1) family(binomial) link(logit) nsims(50000) simsgrid(100)
* Then print the test with the nine controls.
binspwc uninsuredRate perCapitaIncome $controls_cate, by(idxpopdens) estmethod(glm) estmethodopt(irls) randcut(1) family(binomial) link(logit) nsims(50000) simsgrid(100)










********************************************************************************
* APPENDIX -- additional empirical illustrations
********************************************************************************

use `data_main', clear



********************************************************************************
* FIGURE A1 - Robustness to corrupted data
********************************************************************************

* Add low-income outliers: these mask the Medicaid pattern for the mean but not
* for the median. Start with the original least-squares binscatter and retain its
* selected bin count so every estimator below uses the same partition size.

* Fit the original data and locate the right edge of the lowest bin.
tempfile res_orig dots_orig
binsreg uninsuredRate perCapitaIncome, randcut(1) savedata(`res_orig') replace noplot
matrix nbins_matrix = e(nbins_by)
local no_of_bins = nbins_matrix[1,1]
preserve
use `res_orig', clear
quietly summarize rig_ep if binid==1, meanonly
scalar cutoff_for_lowest_bin = r(mean)
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 2
save `dots_orig', replace
restore

* Corrupt 32 observations, approximately 0.1% of the analysis sample. Candidates
* are observations in the lowest bin whose outcomes exceed that bin's median.
display 0.001 * _N
* Make the random selection reproducible, copy x and y, and replace selected y's by 1.
set seed 2
quietly summarize uninsuredRate if perCapitaIncome<cutoff_for_lowest_bin, detail
scalar lowest_median = r(p50)
generate byte corrupt_candidate = perCapitaIncome<cutoff_for_lowest_bin & uninsuredRate>lowest_median
generate double random_draw = runiform() if corrupt_candidate
egen corrupt_rank = rank(random_draw) if corrupt_candidate, unique
replace y = uninsuredRate
replace x = perCapitaIncome
replace y = 1 if corrupt_rank<=32

* Fit median regression and least squares to the corrupted data using the
* original selected number of bins, then retain their plot-ready dots.
tempfile median_low ls_low median_low_plot ls_low_plot
binsqreg y x, randcut(1) quantile(.5) nbins(`no_of_bins') savedata(`median_low') replace noplot
binsreg y x, nbins(`no_of_bins') savedata(`ls_low') replace noplot

* Overlay corrupted-data median and least-squares fits with original least squares.
preserve
use `median_low', clear
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 1
save `median_low_plot', replace
use `ls_low', clear
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 3
save `ls_low_plot', replace
use `median_low_plot', clear
append using `dots_orig' `ls_low_plot'
twoway (scatter plot_fit plot_x if result_series==1 & inrange(plot_x,0,80), mcolor(blue) msymbol(triangle_hollow)) (scatter plot_fit plot_x if result_series==2 & inrange(plot_x,0,80), mcolor(forest_green) msymbol(X) msize(medlarge)) (scatter plot_fit plot_x if result_series==3 & inrange(plot_x,0,80), mcolor(dkorange) msymbol(O)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(0 .4)) xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(order(1 "Median Regression" 2 "Least squares in original data" 3 "Least squares using corrupted data") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/FigA1a_corrupt_low_values.png", replace as(png) width(800) height(600)
restore



* Repeat the exercise at high incomes, where corruption reverses the downward
* least-squares pattern. Refit the original data to recover its bin structure.
use `data_main', clear
tempfile res_orig_high dots_orig_high
binsreg uninsuredRate perCapitaIncome, randcut(1) savedata(`res_orig_high') replace noplot
matrix nbins_matrix = e(nbins_by)
local no_of_bins = nbins_matrix[1,1]

* Set the cutoff at the left edge of the highest three bins.
local high_bin = `no_of_bins' - 2
preserve
use `res_orig_high', clear
quietly summarize lef_ep if binid==`high_bin', meanonly
scalar cutoff_for_high_bin = r(mean)
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 2
save `dots_orig_high', replace
restore

* Select 32 above-median outcomes from those bins. Resetting the seed makes this
* second random selection reproducible; selected outcomes become 1.
set seed 2
quietly summarize uninsuredRate if perCapitaIncome>cutoff_for_high_bin, detail
scalar high_median = r(p50)
generate byte corrupt_candidate = perCapitaIncome>cutoff_for_high_bin & uninsuredRate>high_median
generate double random_draw = runiform() if corrupt_candidate
egen corrupt_rank = rank(random_draw) if corrupt_candidate, unique
replace y = uninsuredRate
replace x = perCapitaIncome
replace y = 1 if corrupt_rank<=32

* Fit both centrality measures with the original number of bins.
tempfile median_high ls_high median_high_plot ls_high_plot
binsqreg y x, randcut(1) quantile(.5) nbins(`no_of_bins') savedata(`median_high') replace noplot
binsreg y x, nbins(`no_of_bins') savedata(`ls_high') replace noplot

* Overlay the three high-income results using the same legend as Panel (a).
preserve
use `median_high', clear
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 1
save `median_high_plot', replace
use `ls_high', clear
keep if !missing(dots_x)
keep dots_x dots_fit
rename dots_x plot_x
rename dots_fit plot_fit
generate byte result_series = 3
save `ls_high_plot', replace
use `median_high_plot', clear
append using `dots_orig_high' `ls_high_plot'
twoway (scatter plot_fit plot_x if result_series==1 & inrange(plot_x,0,80), mcolor(blue) msymbol(triangle_hollow)) (scatter plot_fit plot_x if result_series==2 & inrange(plot_x,0,80), mcolor(forest_green) msymbol(X) msize(medlarge)) (scatter plot_fit plot_x if result_series==3 & inrange(plot_x,0,80), mcolor(dkorange) msymbol(O)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(0 .4)) xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(order(1 "Median Regression" 2 "Least squares in original data" 3 "Least squares using corrupted data") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/FigA1b_corrupt_high_values.png", replace as(png) width(800) height(600)
restore





********************************************************************************
* FIGURE A2 - least squares gives negative predictions
********************************************************************************

    ** Logit QMLE at extreme control-variable evaluation points **

    * Fit at the default evaluation point (the sample mean of the controls) and
    * retain the selected bin count and fitted dots for the black reference.
    tempfile logit_mean all_points at_values extreme_fit logit_profiles
    binsglm uninsuredRate perCapitaIncome $controls, randcut(1) family(binomial) link(logit) glmopt(irls) savedata(`logit_mean') replace noplot
    matrix nbins_matrix = e(nbins_by)
    local bins = nbins_matrix[1,1]

    * Extend the sample-mean curve horizontally to x=0 and x=80, as in R.
    preserve
    use `logit_mean', clear
    keep if !missing(dots_x)
    sort dots_x
    local mean_count = _N
    local extended_count = _N + 2
    set obs `extended_count'
    replace dots_x = 0 if _n==`mean_count'+1
    replace dots_fit = dots_fit[1] if _n==`mean_count'+1
    replace dots_x = 80 if _n==`mean_count'+2
    replace dots_fit = dots_fit[`mean_count'] if _n==`mean_count'+2
    sort dots_x
    save `logit_mean', replace
    restore

    * For each control, record its observed minimum and maximum.
    foreach control of global controls {
        quietly summarize `control'
        scalar ccff_min_`control' = r(min)
        scalar ccff_max_`control' = r(max)
    }

    * Form all 2^9=512 min/max combinations used as alternative at() values.
    preserve
    clear
    set obs 512
    generate int combination = _n
    local bit = 0
    foreach control of global controls {
        generate double `control' = cond(mod(floor((_n-1)/(2^`bit')),2)==0, ccff_min_`control', ccff_max_`control')
        local ++bit
    }
    save `all_points', replace
    restore

    * Refit at every evaluation point while holding the selected bin count fixed.
    tempname post_logit
    postfile `post_logit' int combination double plot_x plot_fit using `logit_profiles', replace
    forvalues j = 1/512 {
        preserve
        use `all_points', clear
        keep if combination==`j'
        drop combination
        save `at_values', replace
        restore

        binsglm uninsuredRate perCapitaIncome $controls, family(binomial) link(logit) glmopt(irls) at(`at_values') nbins(`bins') savedata(`extreme_fit') replace noplot
        preserve
        use `extreme_fit', clear
        keep if !missing(dots_x)
        sort dots_x
        local dot_count = _N
        post `post_logit' (`j') (0) (dots_fit[1])
        forvalues k = 1/`dot_count' {
            post `post_logit' (`j') (dots_x[`k']) (dots_fit[`k'])
        }
        post `post_logit' (`j') (80) (dots_fit[`dot_count'])
        post `post_logit' (`j') (.) (.)
        restore
    }
    postclose `post_logit'

    * Draw all 512 light-blue profiles and add the sample-mean curve and zero.
    preserve
    use `logit_profiles', clear
    append using `logit_mean'
    twoway (line plot_fit plot_x if !missing(combination), cmissing(n) lcolor(ltblue) lwidth(vthin)) (line dots_fit dots_x if missing(combination) & !missing(dots_x), sort lcolor(black) lwidth(medthick)) (function y=0, range(0 80) lcolor(red) lwidth(medthick)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(-.25 1)) xlabel(0(20)80) ylabel(-.25(.25)1, nogrid) legend(order(2 "Sample mean" 1 "Category combinations") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
    graph export "graphs/FigA2b_LogitQMLE.png", replace as(png) width(800) height(600)
    restore



    ** Least squares at the same type of extreme evaluation points **

    * Repeat the complete exercise using Gaussian/identity least squares.
    tempfile ls_mean ls_profiles
    binsglm uninsuredRate perCapitaIncome $controls, randcut(1) family(gaussian) link(identity) glmopt(irls) savedata(`ls_mean') replace noplot
    matrix nbins_matrix = e(nbins_by)
    local bins = nbins_matrix[1,1]

    * Extend the least-squares sample-mean curve to the same plotting limits.
    preserve
    use `ls_mean', clear
    keep if !missing(dots_x)
    sort dots_x
    local mean_count = _N
    local extended_count = _N + 2
    set obs `extended_count'
    replace dots_x = 0 if _n==`mean_count'+1
    replace dots_fit = dots_fit[1] if _n==`mean_count'+1
    replace dots_x = 80 if _n==`mean_count'+2
    replace dots_fit = dots_fit[`mean_count'] if _n==`mean_count'+2
    sort dots_x
    save `ls_mean', replace
    restore

    * Refit least squares at each evaluation point with the bin count fixed.
    tempname post_ls
    postfile `post_ls' int combination double plot_x plot_fit using `ls_profiles', replace
    forvalues j = 1/512 {
        preserve
        use `all_points', clear
        keep if combination==`j'
        drop combination
        save `at_values', replace
        restore

        binsglm uninsuredRate perCapitaIncome $controls, family(gaussian) link(identity) glmopt(irls) at(`at_values') nbins(`bins') savedata(`extreme_fit') replace noplot
        preserve
        use `extreme_fit', clear
        keep if !missing(dots_x)
        sort dots_x
        local dot_count = _N
        post `post_ls' (`j') (0) (dots_fit[1])
        forvalues k = 1/`dot_count' {
            post `post_ls' (`j') (dots_x[`k']) (dots_fit[`k'])
        }
        post `post_ls' (`j') (80) (dots_fit[`dot_count'])
        post `post_ls' (`j') (.) (.)
        restore
    }
    postclose `post_ls'

    * Draw all profiles and add the least-squares sample-mean curve and zero.
    preserve
    use `ls_profiles', clear
    append using `ls_mean'
    twoway (line plot_fit plot_x if !missing(combination), cmissing(n) lcolor(ltblue) lwidth(vthin)) (line dots_fit dots_x if missing(combination) & !missing(dots_x), sort lcolor(black) lwidth(medthick)) (function y=0, range(0 80) lcolor(red) lwidth(medthick)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(-.25 1)) xlabel(0(20)80) ylabel(-.25(.25)1, nogrid) legend(order(2 "Sample mean" 1 "Category combinations") position(1) ring(0) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black))
    graph export "graphs/FigA2a_LeastSquaresNegative.png", replace as(png) width(800) height(600)
    restore

    * Report how many least-squares profiles are negative at x=80 and list the
    * associated min/max control combinations.
    preserve
    use `ls_profiles', clear
    keep if plot_x==80 & plot_fit<0
    keep combination
    merge 1:1 combination using `all_points', keep(match) nogen
    count
    display r(N)
    list combination $controls, noobs
    restore






********************************************************************************
* FIGURE A3 - confidence band and quantiles
********************************************************************************

* Contrast estimation uncertainty for the conditional mean with outcome spread
* between the conditional 10th and 90th percentiles. The quantile targets match
* Figure 2, but all three estimates use the mean fit's selected bin count.

* Without controls, fit the mean by the default Gaussian/identity model and
* request an 80% uniform band. Record its selected bin count for both quantiles.
tempfile mean_a3 q1_a3 q9_a3 q1_a3_plot q9_a3_plot
binsglm uninsuredRate perCapitaIncome, randcut(1) cb(T) glmopt(irls) plotxrange(0 80) level(80) nsims(50000) simsgrid(100) savedata(`mean_a3') replace noplot
matrix nbins_matrix = e(nbins_by)
local no_of_bins = nbins_matrix[1,1]
binsqreg uninsuredRate perCapitaIncome, quantile(.1) nbins(`no_of_bins') savedata(`q1_a3') replace noplot
binsqreg uninsuredRate perCapitaIncome, quantile(.9) nbins(`no_of_bins') savedata(`q9_a3') replace noplot

* Overlay the two quantile series on the binsglm mean and confidence band.
preserve
use `q1_a3', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte result_series = 1
save `q1_a3_plot', replace
use `q9_a3', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte result_series = 9
save `q9_a3_plot', replace
use `mean_a3', clear
generate byte result_series = 0
append using `q1_a3_plot' `q9_a3_plot'
twoway (rarea CB_l CB_r CB_x if result_series==0 & inrange(CB_x,0,80), fcolor(gs12) fintensity(40) lwidth(none)) (scatter dots_fit dots_x if result_series==0 & inrange(dots_x,0,80), mcolor(blue)) (scatter dots_fit dots_x if result_series==1 & inrange(dots_x,0,80), mcolor(gs8)) (scatter dots_fit dots_x if result_series==9 & inrange(dots_x,0,80), mcolor(gs8)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(0 .4)) xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/FigA3a_QuantilesAndBandNoControls.png", replace as(png) width(800) height(600)
restore


* Repeat with controls; each estimator is evaluated at the sample mean.
tempfile mean_a3_controls q1_a3_controls q9_a3_controls q1_a3_controls_plot q9_a3_controls_plot
binsglm uninsuredRate perCapitaIncome $controls, randcut(1) cb(T) glmopt(irls) plotxrange(0 80) level(80) nsims(50000) simsgrid(100) savedata(`mean_a3_controls') replace noplot
matrix nbins_matrix = e(nbins_by)
local no_of_bins = nbins_matrix[1,1]
binsqreg uninsuredRate perCapitaIncome $controls, quantile(.1) nbins(`no_of_bins') savedata(`q1_a3_controls') replace noplot
binsqreg uninsuredRate perCapitaIncome $controls, quantile(.9) nbins(`no_of_bins') savedata(`q9_a3_controls') replace noplot

* Overlay the controlled quantile dots on the controlled mean band.
preserve
use `q1_a3_controls', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte result_series = 1
save `q1_a3_controls_plot', replace
use `q9_a3_controls', clear
keep if !missing(dots_x)
keep dots_x dots_fit
generate byte result_series = 9
save `q9_a3_controls_plot', replace
use `mean_a3_controls', clear
generate byte result_series = 0
append using `q1_a3_controls_plot' `q9_a3_controls_plot'
twoway (rarea CB_l CB_r CB_x if result_series==0 & inrange(CB_x,0,80), fcolor(gs12) fintensity(40) lwidth(none)) (scatter dots_fit dots_x if result_series==0 & inrange(dots_x,0,80), mcolor(blue)) (scatter dots_fit dots_x if result_series==1 & inrange(dots_x,0,80), mcolor(gs8)) (scatter dots_fit dots_x if result_series==9 & inrange(dots_x,0,80), mcolor(gs8)), xtitle("Per Capita Income ($ Thousands)") ytitle("Percent Uninsured") xscale(range(0 80)) yscale(range(0 .4)) xlabel(0(20)80) ylabel(0(.1).4, nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black))
graph export "graphs/FigA3b_QuantilesAndBandYesControls.png", replace as(png) width(800) height(600)
restore
