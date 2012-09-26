\name{mle_marginal}
\alias{mle_marginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ML-estimates of the marginal models
}
\description{We fit the Gamma and the ZTP model separately.
}
\usage{
mle_marginal(x, y, R, S, family,exposure,sd.error=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
}
\details{This is an internal function called by \code{\link{copreg}}.
}
\value{
\item{alpha}{estimated coefficients for X, including the intercept}
\item{beta}{estimated coefficients for X, including the intercept}
\item{sd.alpha}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{sd.beta}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{delta}{estimated dispersion parameter}
\item{theta}{0, in combination with \code{family=1}, this corresponds to the independence assumption}
\item{family}{1, in combination with \code{theta=0}, this corresponds to the independence assumption}
\item{family0}{copula family as provided in the function call}
\item{theta.ifm}{estimated copula parameter, estimated via inference from margins}
\item{tau.ifm}{estimated value of Kendall's tau, estimated via inference from margins}
\item{ll}{loglikelihood of the estimated model, assuming independence,evaluated at each observation}
\item{loglik}{overall loglikelihood, assuming independence, i.e. sum of \code{ll}}
\item{ll.ifm}{loglikelihood of the estimated model, using \code{theta.ifm} as the copula parameter, evaluated at each observation}
\item{loglik.ifm}{overall loglikelihood, using \code{theta.ifm} as the copula parameter, i.e. sum of \code{ll.ifm}}
}

\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available upon request

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{copreg}}, \code{\link{mle_joint}} 
}
\examples{
##---- This is an internal function called by copreg() ----

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{htest}
\keyword{multivariate}
