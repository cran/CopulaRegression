\name{epolicy_loss}
\alias{epolicy_loss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expectation of the policy loss
}
\description{
Expectation and variance of the policy loss
}
\usage{
epolicy_loss(mu, delta, lambda, theta, family, y.max = 300,zt=TRUE,compute.var=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{expectation of the Gamma distribution, can be a vector
}
  \item{delta}{dispersion parameter of the Gamma distribution
}
  \item{lambda}{parameter of the (zero-truncated) Poisson distribution, can be a vector of the same length as mu
}
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
  \item{y.max}{upper value of the finite sum that we use to approximate the infinite sum in the density, see below for details
}
\item{zt}{logical. If \code{zt=TRUE}, we use a zero-truncated Poisson variable. Otherwise, we use a Poisson variable. Default is \code{TRUE}.
} 
\item{compute.var}{logical. If \code{compute.var=TRUE}, we also compute the variance of the policy loss. Default is \code{FALSE}.
} 
}
\details{
For a Gamma distributed variable X and a (zero truncated) Possion variable Y, the policy loss is defined as \eqn{L=X\cdot Y}. Its density is an infinite sum of weighted Gamma densities. The parameter \code{y.max} is the upper value of the finite sum that approximates the infinite sum.
}
\value{
\item{mean}{expectation of the policy loss}
\item{var}{variance of the policy loss}
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2013): Total loss estimation using copula-based regression models. Insurance: Mathematics and Economics 53 (3), 829 - 839.

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{dpolicy_loss}}
}
\examples{
library(VineCopula)
mu<-1000
delta<-0.09
lambda<-2.5
family<-3
theta<-BiCopTau2Par(tau=0.5,family=family)
out<-epolicy_loss(mu,delta,lambda,theta,family)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{models}
