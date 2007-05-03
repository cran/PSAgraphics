\name{circ.psa}
\alias{circ.psa}
\title{Graphically investigate strata effect size contributions in a PSA}
\description{Graphic for use as part of the second stage of a Propensity Score
Analysis (PSA).  Function to create a graphic plotting each strata as a circle 
centered at the treatment responses. The size of the circle corresponds to the 
size of the strata.  On the diagonal, the direct effect estimate is shown,
and the contributions of each stratum to that estimate may be seen.}
\usage{
circ.psa(response, treatment = NULL, strata = NULL, summary = FALSE, 
statistic = "mean", revc = FALSE, sw = 0.4, ne = 0.5, inc = 0.35, 
pw = 0.4, lab = TRUE, labcex = 1, xlab = NULL, ylab = NULL, main = NULL)
}
\arguments{
  \item{response}{Either a numeric vector containing the response of interest in a 
  propensity score analysis, or a three column array containing response, treatment and strata.}
 \item{treatment}{Binary variable of same length as \code{response}; generally 0 for 'control,' 1 for 'treatment.' }
 \item{strata}{Integer variable;  a vector of same length as \code{response} indicating the 
  derived strata from estimated propensity scores. Generally 5 or 6 strata are used, but graph works 
  reasonably well at least up to 10 strata.}
 \item{summary}{Logical (default \code{FALSE}).  If \code{TRUE} then \code{response} must have rows corresponding to strata, 
 with the first two columns containing treatment sizes for each stratum, and the second 
 two columns containing the appropriate summary statistics for each statum.  For example, the first 
 four columns of the \code{strata.summary} output of \code{loess.psa}.}
  \item{statistic}{Any univariate function appropriate for the response effect.  See details below.  Default = "mean"}
  \item{revc}{Logical; if \code{TRUE} then X and Y axes are interchanged in graph.}
  \item{sw}{Numerical parameter (default = 0.4); extends axes on lower ends, effectively moving circles to lower left.}
  \item{ne}{Numerical parameter (default = 0.5); extends axes on upper ends, effectively moving circles to upper right.}
  \item{inc}{Numerical parameter (default = 0.35); controls circle sizes, but relative circle sizes 
   are controlled via \code{pw}. In general one wants circle areas to appear subjectively to be 
   sized in accordance with strata sizes.}
  \item{pw}{numerical parameter (default = 0.4); controls relative circle sizes.}
  \item{lab}{Logical (default TRUE); labels circles with strata number.}
  \item{labcex}{numerical parameter (default = 1); controls the size of the circle labels.}
  \item{xlab}{Label for horizontal axis, by default taken from \code{treatment}.}
  \item{ylab}{Label for vertical axis, by default taken from \code{treatment}.}
  \item{main}{Main label for graph.}
  }

\value{Produces a Propensity Assessment Plot, details below as well as
\item{summary.strata}{An array with rows corresponding to strata and four columns providing counts in control and treatment, and mean 
response values for control and treatment.}
\item{C.wtd}{Weighted control response.}
\item{T.wtd}{Weighted treatment response.}
\item{diff.wtd}{Direct adjusted estimator.}
\item{sd.wtd}{Weighted standard deviation for \code{diff.wtd}}
\item{approx.t}{Ration of the direct adjusted estimaor and weighted standard deviation.}
\item{df}{Estimate of degree of freedom; response size minus twice number of strata.}
}

\details{The \code{statistic} used is by default \code{mean}, but median may be used or any other
statistic of interest that is univariate.  Parameters are not passed to routine, but this may be 
worked around via user defined functions.  For example: 

\code{tr.mean <- function(x){mean(x, trim = .1)}}

Rug plots of each treatment reponse statistic by strata are provided at top and right; 
the weighted means of these marginal distributions are shown as red dotted lines.  
Circle sizes correspond to size of the strata (both treatments combined).  The 
black \code{Y = X} diagonal reference line is the line of "no effect." The parallel 
heavy blue dashed line represents the overall direct effect estimate, with the 
scatterplot of blue \code{X}s indicating again the distribution of effect estimates 
from each statum.}
\author{James E. Helmreich \email{ James.Helmreich@Marist.edu}

 Robert M. Pruzek \email{RMPruzek@yahoo.com}}
\seealso{\code{loess.psa}}
\examples{
##Random data with effect size 0
response <- rnorm(1000)
treatment <- sample(c(0,1), 1000, replace = TRUE)
strata <- sample(1:6, 1000, replace = TRUE)
aaa <- cbind(response, treatment, strata)
circ.psa(response, treatment, strata)

##Random data with effect size -.2
response <- c(rnorm(500, 0, 12), rnorm(500, 6, 12))
treatment <- c(rep(0, 500), rep(1,500))
strata <- sample(1:5, 1000, replace = TRUE)
aaa <- cbind(response, treatment, strata)
circ.psa(aaa)

##Tree derived strata
library(rpart)
data(lindner)
attach(lindner)
lindner.rpart <- rpart(abcix ~ stent + height + female + diabetic +
     acutemi + ejecfrac + ves1proc, data = lindner, method = "class")
lindner.tree<-factor(lindner.rpart$where, labels = 1:6)
circ.psa(log(cardbill), abcix, lindner.tree)

##Loess derived strata
lindner.ps <- glm(abcix ~ stent + height + female + 
      diabetic + acutemi + ejecfrac + ves1proc, 
      data = lindner, family = binomial)
ps<-lindner.ps$fitted
lindner.loess<-loess.psa(log(cardbill), abcix, ps)
circ.psa(lindner.loess$summary.strata[, 1:4], summary = TRUE, 
      inc = .1, labcex = .7)}

\keyword{ hplot }

