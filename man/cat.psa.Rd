\name{cat.psa}
\alias{cat.psa}
\title{Compare balance graphically of a categorical covariate as part of a PSA}
\description{Given predefined strata and two level treatment for a categorical covariate from a propensity score analysis, 
\code{cat.psa} draws pairs of side by side barplots corresponding to control and treatment for each stratum.}
\usage{
cat.psa(categorical, treatment = NULL, strata = NULL, 
catnames = NULL, catcol = "terrain.colors", width = 0.25, 
barlab = c("0", "1"), barnames = c("Control", "Treatment"), 
rtmar = 1.5, balance = FALSE, B = 100, tbl = TRUE, 
cex.leg = 1, ...)
}
\arguments{
 \item{categorical}{Categorical covariate that is being balanced within strata 
  in a PSA. If \code{categorical} has three columns, then the second and third
  are assumed to be the treatment and strata respectively.  Missing values are
  not allowed. May be factor or numeric. }
 \item{treatment}{Binary variable of same length as \code{categorical}; generally 0 for 'control,' 1 for 'treatment.' }
 \item{strata}{Integer variable;  a vector of same length as \code{categorical} indicating the 
  derived strata from estimated propensity scores. Generally 5 or 6 strata are used, but graph works 
  reasonably well at least up to 10 strata.}
 \item{catnames}{List of names in order of the categories; used in the plot legend. Default is \code{1:n}.}
 \item{catcol}{List of colors used for the categories, default is "terrain.colors"}
  \item{width}{Controls width of bars, default = 0.25.}
  \item{barlab}{Binary list of single character labels for the bars, default is \code{c("0", "1")} for 
  \code{treatment} as described above.}
  \item{barnames}{Binary list treatment names used in legend; default is \code{c("Control", "Treatment")}. }
  \item{rtmar}{Numeric.  Governs size of right margin allocated for legend.  Default = 1.5}
  \item{balance}{Logical.  If \code{TRUE} a call is made to functions \code{bal.cs.psa} and 
   \code{bal.cws.psa}. The former provides a reference histogram and ad hoc balance statistic, 
   the second provides bootstrapped p-values for the two-way table formed in each statum.  
   Default is \code{FALSE}.}
  \item{B}{Numeric; passed to \code{bal.cs.psa} governing size of reference histogram generated.  Default is 100.}
  \item{tbl}{Logical; if \code{TRUE}, then a matrix of the proportions used in the creation of the bargraph is returned.}
  \item{cex.leg}{Numeric; value of \code{cex} (governing font size) passed to legend.  Default = 1.}
  \item{\dots}{Other graphical parameters passed to plot.}}
\details{Pairs of bars are graphed side by side so that comparisons may be made within each stratum and across strata.
  If \code{balance} is \code{TRUE}, then the histogram represents an ad hoc balance measure 
  of the given strata as compared to randomly generated strata.  The p-values provided on the bargraph 
  are bootstrapped in a standard fashion via randomly generated treatment divisions within given strata.  
  For continuous covariates use \code{box.psa}.
}
\value{If \code{tbl} is \code{TRUE}, then a matrix is returned containing the proportions of each category, and in each treatment
 level and stratum that were used to draw the bargraph.
}

\author{James E. Helmreich \email{ James.Helmreich@Marist.edu}

 Robert M. Pruzek \email{RMPruzek@yahoo.com}}
\seealso{ \code{bal.cs.psa}, \code{bal.cws.psa}, \code{box.psa} }
\examples{
categorical<-sample(1:7,1000,replace=TRUE)
treatment<-sample(c(0,1),1000,replace=TRUE)
strata<-sample(5,1000,replace=TRUE)
cat.psa(categorical,treatment,strata)

data(lindner)
attach(lindner)
lindner.ps <- glm(abcix ~ stent + height + female + 
      diabetic + acutemi + ejecfrac + ves1proc, 
      data = lindner, family = binomial)
ps<-lindner.ps$fitted
lindner.s5 <- as.numeric(cut(ps, quantile(ps, seq(0, 1, 1/5)),
      include.lowest = TRUE, labels = FALSE))
cat.psa(stent, abcix, lindner.s5, xlab = "stent")

lindner.s10 <- as.numeric(cut(ps, quantile(ps, seq(0, 1, 1/10)),
      include.lowest = TRUE, labels = FALSE))
cat.psa(ves1proc,abcix, lindner.s10, balance = TRUE, xlab = "ves1proc")

#Using a rpart tree for strata
library(rpart)
lindner.rpart<-rpart(abcix ~ stent + height + female + diabetic + 
      acutemi + ejecfrac + ves1proc, data=lindner, method="class")
lindner.tree<-factor(lindner.rpart$where, labels = 1:6)
cat.psa(stent, abcix, lindner.tree, xlab = "stent")
cat.psa(ves1proc, abcix, lindner.tree, xlab = "ves1proc")
}

\keyword{ hplot }
\keyword{ htest }
