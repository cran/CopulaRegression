\name{density_joint}
\alias{density_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Joint density of X and Y}
\description{Density of a Gamma distributed variable X and a zero-truncated Poisson variable Y if their joint distribution is defined via a copula
}
\usage{
density_joint(x, y, mu, delta, lambda, theta, family)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector at which the density is evaluated
}
  \item{y}{vector at which the density is evaluated}
  \item{mu}{expectation of the Gamma distribution
}
  \item{delta}{dispersion parameter  of the Gamma distribution
}
  \item{lambda}{parameter of the zero-truncated Poisson distribution
}
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
}
\details{For a Gamma distributed variable X and a zero truncated Possion variable Y, their joint density function is given by
\deqn{f_{XY}(x,y)=f_X(x) \left(D_u(F_Y(y),F_X(x)|\theta) - D_u(F_Y(y-1),F_X(x)|\theta) \right)\,. }Here \eqn{D_u} is the h-function of a copula  famila \code{family} with copula parameter \code{theta}. 
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available upon request

}
\author{Nicole Kraemer
}
\note{
We allow two options: If mu and lambda are vectors of the same length as x and y, we evaluate the density for the corresponding parameter values. Otherwise, lambda and nu have to be numbers,
and the parameters are the same for all entries of x and y.
}


\seealso{\code{\link{density_conditional}}, \code{\link{D_u}}
}
\examples{
out<-density_joint(2,3,mu=1,delta=1,lambda=4,theta=0.5,family=1)
}
\keyword{multivariate}
\keyword{distribution}% __ONLY ONE__ keyword per line
