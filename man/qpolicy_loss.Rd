\name{qpolicy_loss}
\alias{qpolicy_loss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile of the policy loss
}
\description{Quantile of the policy  loss 
}
\usage{
qpolicy_loss(q, mu, delta, lambda, theta, family, y.max = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{value at which the quantile function is evaluated
}
  \item{mu}{expectation of the Gamma distribution
}
  \item{delta}{dispersion parameter of the Gamma distribution
}
  \item{lambda}{parameter of the ZTP distribution
}
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
  \item{y.max}{upper value of the finite sum that we use to approximate the infinite sum in the density, see below for more details
}
}
\details{
For a Gamma distributed variable X and a zero truncated Possion variable Y, the policy loss is defined as \eqn{L=X\cdot Y}. Its density is an infinite sum of weighted Gamma densities. The parameter \code{y.max} is the upper value of the finite sum that approximates the infinite sum.
}
\value{quantile, evaluated at q
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available upon request

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ppolicy_loss}},\code{\link{epolicy_loss}},\code{\link{dpolicy_loss}}
}
\examples{
mu<-1000
delta<-0.09
lambda<-2.5
family<-1
theta<-BiCopTau2Par(tau=0.5,family=family)
# upper quartile
out<-qpolicy_loss(0.75,mu,delta,lambda,theta,family)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
