\name{box.psa}
\alias{box.psa}
\title{Compare balance graphically of a continuous covariate as part of a PSA}
\description{Given predefined strata and two level treatment for a continuous covariate from a propensity score analysis, 
\code{box.psa} draws pairs of side by side boxplots corresponding to control and treatment for each stratum.}
\usage{
box.psa(continuous, treatment = NULL, strata = NULL, boxwex = 0.17, 
offset = 0.17, col = c("yellow", "orange", "black", "red", "darkorange3"),
xlab = "Stratum", legend.xy = NULL, 
legend.labels = c("Control", "Treatment", "C/T Means Compared", 
"KS p-values", "Strata-Treatment Size"), pts = TRUE, 
balance = FALSE, B = 1000, ...)
}
\arguments{
  \item{continuous}{Quantitative covariate that is being balanced within strata in a PSA. If \code{continuous} 
  has three columns, then the second and third are assumed to be the treatment and strata respectively.  Missing values are not allowed. }
  \item{treatment}{ Binary variable of same length as \code{continuous}; generally 0 for 'control,' 1 for 'treatment.' }
  \item{strata}{ Integer variable (usually 1 - 5);  A vector of same length as continuous indicating the 
  derived strata from estimated propensity scores. Generally 5 or 6 strata are used, but graph works 
  reasonably well at least up to 10 strata.}
  \item{boxwex}{ Numeric; controls width of boxes. Default = 0.17 }
  \item{offset}{ Numeric; controls distance between the two boxes in each stratum. Default = 0.17 }
  \item{col}{ Color vector for the control boxes, treatment boxes, and 
  line connecting their means.  Default = \code{c("yellow", "orange", "black", "red", "darkorange3")}. }
  \item{xlab}{ Label for the x-axis; default = \code{"Stratum"}.  Other standard labels may be used as well. }
  \item{legend.xy}{ Binary vector giving coordinates of the legend. By default the legend is placed to the top left.  }
  \item{legend.labels}{ Vector of labels for the legend; default = \code{c("Control", "Treatment", "C/T Means Connected")}}
  \item{pts}{Logical; if \code{TRUE} then (jittered) points are added on top of the boxplots.}
  \item{balance}{Logical; if \code{TRUE} then \code{bal.ms.psa} provides a histogram of 
  a permutation distribution and reference statstic to assess balance across strata; \code{bal.ks.psa} 
  adds p-values to the graph derived from 2-sample Kologmorov-Smirnov tests of 
  equivalence of control/treatment distributions within each stratum.}
  \item{B}{Passed to \code{bal.ms.psa} if necessary, determines number of randomly generated 
  comparison statistics.  Default =1000.}
  \item{\dots}{Other graphical parameters passed to \code{boxplot}.}
}
\details{Draws a pair of side by side boxplots for each stratum of a propensity score analysis.  
This allows visual comparisons within strata of the distribution of the given continuous covariate, 
and comparisons between strata as well.  The number of observations in each boxplot are 
given below each box, and the means of paired treatment and control groups are connected.}

\author{James E. Helmreich \email{ James.Helmreich@Marist.edu}

 Robert M. Pruzek \email{RMPruzek@yahoo.com}}

\seealso{ \code{bal.ks.psa}, \code{bal.ms.psa}, \code{cat.psa} }

\examples{ 
continuous<-rnorm(1000)
treatment<-sample(c(0,1),1000,replace=TRUE)
strata<-sample(5,1000,replace=TRUE)
box.psa(continuous, treatment, strata)

data(lindner)
attach(lindner)
lindner.ps <- glm(abcix ~ stent + height + female + 
      diabetic + acutemi + ejecfrac + ves1proc, 
      data = lindner, family = binomial)
ps<-lindner.ps$fitted
lindner.s5 <- as.numeric(cut(ps, quantile(ps, seq(0, 1, 1/5)),
      include.lowest = TRUE, labels = FALSE))
box.psa(ejecfrac, abcix, lindner.s5, xlab = "ejecfrac", legend.xy = c(3.5,110))

lindner.s10 <- as.numeric(cut(ps, quantile(ps, seq(0, 1, 1/5)),
      include.lowest = TRUE, labels = FALSE))
box.psa(height, abcix, lindner.s10, xlab="height", 
      boxwex = .15, offset = .15, legend.xy = c(2,130), balance = TRUE)
}
\keyword{ hplot }
\keyword{ htest }

