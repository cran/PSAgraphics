\name{loess.psa}
\alias{loess.psa}
\title{Graphic for data and loess-based estimate of effect size after propensity score adjustment}
\description{Plots data points using propesity scores vs. the response, separately for treatment 
and control groups; points are distinguished by both type and color for the two groups.  Also shows (non-linear, 
loess-based) regression curves for both groups.  The loess regresion curves are then used to derive an overall  
estimate of effect size (based on number and/or location of strata as set by the user).  Several other statistics are also
provided, for both description and inference.  Graphic motivated by a suggestion of R. L. Obenchain.
}
\usage{
loess.psa(response, treatment = NULL, propensity = NULL, 
family = "gaussian", 
span = 0.7, degree = 1, minsize = 5, xlim = c(0, 1), 
colors = c('dark blue','green','blue','dark green'), 
legend.xy = "topleft", legend = NULL,
int = 15, lines = TRUE, rg = TRUE, xlab = "Estimated Propensity Scores", 
ylab = "Response", pch = c(16,1), ...)
}
\arguments{
  \item{response}{Either a numeric vector containing the response of interest in a 
     propensity score analysis, or a three column array containing response, treatment and strata.}
  \item{treatment}{Binary variable of same length as \code{response}; 0 for 'control,' 1 for 'treatment.' }
  \item{propensity}{Numeric vector of estimated propensity scores.}
  \item{family}{Passed to loess. Either \code{"gaussian"} (default) or \code{"symmetric"}.}
  \item{span}{Parameter passed to loess governing degree of smoothing.  Default = 0.7.}
  \item{degree}{Parameter passed to loess governing degree of polynomials used.  Default = 1}
  \item{minsize}{Integer.  Determines the minimum number of observations in each stratum treatment group allowed.  
     If one of the treatment groups in a given statum does not meet this minsize, then all observations 
     in this stratum are ignored as far as the effect size calculation is concerned.}
  \item{xlim}{Binary vector \code{(min, max)} providing the horizontal axis minimum and maximum.  Default is \code{c(0, 1)}.}
  \item{colors}{List of four colors used for control points, treatment points, control loess line, treatment loess 
     line respectively.  Default = \code{c("seagreen3", "goldenrod1", "seagreen4", "goldenrod3")}.}
  \item{legend.xy}{Coordinates for legend box, see \code{legend}.  Default = \code{"topleft"}.}
  \item{legend}{Binary character vector containing the text of the legend.  Default is taken from \code{treatment}.} 
  \item{int}{Integer or ordered vector.  If an integer is used, it represents the maximum number of equally sized strata.  
     Alternatively, it may be a vector of cuts of the unit interval. Lower and upper ends need not be included.  
     See examples. Default = 15.}
  \item{lines}{Logical; fitted loess values are plotted by default as points.  If true, values are plotted as two lines.}
  \item{rg}{Logical; if \code{TRUE} (default) then rug plots are given for treatment and control propensity score and
     response distributions.}
  \item{xlab}{X axis label, default = \code{"Estimated Propensity Scores"}.}
  \item{ylab}{Y axis label, default = \code{"Response"}.}
  \item{pch}{Character types for plotted points, default = \code{c(16, 1)}.  Note: must be of length 2 to allow 
    different plotting points for each treatment.}
  \item{\dots}{Optional parameters passed to points command.}
}
\value{In addition to the plot, the function returns a list with the following components: 
  \item{effect.size.est}{Estimated effect size based upon (number of) strata defined by \code{int}; that is, 
  this is the Direct Adjustment Estimate of the treatment effect, after propensity-based adjustment.}
  \item{sd.wtd}{Weighted standard deviation based on pooling of within-strata variance estimates.}
  \item{CI.95}{Approximate 95\% confidence interval for the overall effect size (conditional on the specification of \code{int}).}
  \item{summary.strata}{A table with rows corresponding to strata; first two columns show
  counts (by statum) for both control and treatment; followed by mean differences for all strata. 
  for control and treatment, followed by mean differences for all strata.
  The weighted average difference yields the effect size noted above.}
}

\author{James E. Helmreich \email{James.Helmreich@Marist.edu}

 Robert M. Pruzek \email{RMPruzek@yahoo.com}}

\seealso{\code{\link{circ.psa}}}

\examples{
#Artificial example where DAE should be 1.5 over all of (0,1).
#Over the 4 subintervals provided should be 1.5 as well as is 
#symmetric about .5
response <- c(rep(1,150), rep(2,150), rep(0,300))
treatment <- c(rep(1,300), rep(0,300))
propensity <- rep(seq(.01, .99, (.98/299)), 2)
a <- data.frame(response, treatment, propensity)
loess.psa(a, span = .15, degree = 1, int = c(.1, .3, .5, .7, .9))

#Artificial example where estimates are unstable with varying 
#numbers of strata. Note: sometimes get empty treatment/strata error.
rr <- c(rnorm(150, 3, .75), rnorm(700, 0, .75), rnorm(150, 3, .75), 
     rnorm(150, -3, .75), rnorm(700, 0, .75), rnorm(150, -3, .75))
tt <- c(rep(1, 1000),rep(0, 1000))
pp <- NULL
for(i in 1:1000){pp <- c(pp, rnorm(1, 0, .05) + .00045*i + .25)}
for(i in 1:1000){pp <- c(pp, rnorm(1, 0, .05) + .00045*i + .4)}
a <- data.frame(rr, tt, pp)
loess.psa(a, span=.5, cex = .6)

#Using strata of possible interest as determined by loess lines.
data(lindner)
attach(lindner)
lindner.ps <- glm(abcix ~ stent + height + female + 
      diabetic + acutemi + ejecfrac + ves1proc, 
      data = lindner, family = binomial)
loess.psa(log(cardbill), abcix, lindner.ps$fitted,  
         int = c(.37, .56, .87, 1), lines = TRUE)
         abline(v=.37)
         abline(v=.56)
         abline(v=.87)
}
\keyword{ hplot }

