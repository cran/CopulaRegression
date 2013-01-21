\name{mle_joint}
\alias{mle_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ML-Estimates of the joint model.
}
\description{Computes the maximum-likelihood estimates for the regression coefficients and the copula parameter.
}
\usage{
mle_joint(alpha0,beta0,theta0, delta0, x, y, R, S, family, exposure, sd.error = FALSE,zt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha0}{The starting value of the regression coefficients for the Gamma regression
}
\item{beta0}{The starting value of the regression coefficients for the (zero-truncated) Poisson regression
}
\item{theta0}{The starting value of the copula parameter
}
  \item{delta0}{The starting value for the dispersion parameter of the Gamma distribution
}
  \item{x}{n observations of the Gamma variable
}
  \item{y}{n observations of the zero-truncated Poisson variable
}
  \item{R}{n x p design matrix for the Gamma model
}
  \item{S}{n x q design matrix for the zero-truncated Poisson model
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
  \item{exposure}{exposure time for the zero-truncated Poisson model, all entries of the vector have to be \eqn{>0}. Default is a constant vector of 1.
}
 \item{sd.error}{logical. Should the standard errors of the regression coefficients be returned? Default is FALSE.}
\item{zt}{logical. If \code{zt=TRUE}, we use a zero-truncated Poisson variable. Otherwise, we use a Poisson variable. Default is \code{TRUE}.
} 
}
\details{This is an internal function called by \code{\link{copreg}}.
}
\value{
\item{alpha}{estimated coefficients for X, including the intercept}
\item{beta}{estimated coefficients for Y, including the intercept}
\item{sd.alpha}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{sd.beta}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{sd.g.theta}{estimated standard deviation of \eqn{g(\theta)} (if \code{sd.error=TRUE})}
\item{delta}{estimated dispersion parameter}
\item{theta}{estimated copula parameter}
\item{tau}{estimated value of Kendall's tau}
\item{family}{copula family}
\item{ll}{loglikelihood of the estimated model, evaluated at each observation}
\item{loglik}{overall loglikelihood, i.e. sum of \code{ll}}


}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{copreg}}, \code{\link{mle_marginal}}
}
\examples{
##---- This is an internal function called by copreg() ----

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{htest}
\keyword{multivariate}
