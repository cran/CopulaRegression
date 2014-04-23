\name{density_conditional}
\alias{density_conditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conditional density of Y given X
}
\description{Conditional density of a (zero-truncated) Poisson variable Y given X=x for a Gamma-distributed variable X.
}
\usage{
density_conditional(y,x, mu, delta, lambda, theta, family,zt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector at which the conditional density is evaluated
}
\item{x}{conditioning value of the Gamma distributed variable
}
  \item{mu}{expectation of the Gamma distribution
}
  \item{delta}{dispersion parameter of the Gamma distribution
}
  \item{lambda}{parameter of the zero-truncated Poisson distribution
}
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
\item{zt}{logical. If \code{zt=TRUE}, we use a zero-truncated Poisson variable. Otherwise, we use a Poisson variable. Default is \code{TRUE}.}
}
\details{For a Gamma distributed variable X and a (zero truncated) Possion variable Y with joint density function \eqn{f_{XY}(x,y)}, this function evaluates \deqn{P(Y=y|X=x)=\frac{f_{XY}(x,y)}{f_X(x)}\,.} The joint density function is determined by a copula famila \code{family} with copula parameter \code{theta}. 

}
\value{vector of length \code{length(y)}
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2013): Total loss estimation using copula-based regression models. Insurance: Mathematics and Economics 53 (3), 829 - 839.

}
\author{Nicole Kraemer, Daniel Silvestrini
}


\seealso{\code{\link{density_joint}}
}
\examples{
out<-density_conditional(y=0:10,x=3,mu=1,delta=1,lambda=2,theta=0.5,family=1)
names(out)=0:10
barplot(out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{models}
