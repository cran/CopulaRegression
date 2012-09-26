\name{simulate_joint}
\alias{simulate_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation from the joint model
}
\description{
Simulation from the joint model
}
\usage{
simulate_joint(n, mu, delta, lambda, theta, family, max.y = 5000, eps = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of samples
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
  \item{max.y}{upper value for the conditional zero truncated Poisson variable, see below for more details
}
  \item{eps}{precision, see below for more details
}
}
\details{For a Gamma distributed variable X and a zero truncated Possion variable Y, we sample from their joint distribution that is given by the density function
\deqn{f_{XY}(x,y)=f_X(x) \left(D_u(F_Y(y),F_X(x)|\theta) - D_u(F_Y(y-1),F_X(x)|\theta) \right)\,. }Here \eqn{D_u} is the h-function of a copula  famila \code{family} with copula parameter \code{theta}. First, we sample n observations \code{x} from the marginal Gamma distribution. Second, for each x, we then sample an observation from the conditional distribution of Y given X=x.   In the second step, the conditional distribution is evaluated up to the maximum of  \code{max.y} and the smallest integer >  \code{y.max} for which the conditional probability is smaller than \code{eps}.
}
\value{n samples, stored in a \eqn{n \times 2} matrix
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available upon request

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{density_joint}}, \code{\link{simulate_regression_data}},\code{\link{density_conditional}}
}
\examples{
n<-100 # number of observations
mu<-1000
delta<-0.09
lambda<-2.5
family<-1
theta<-BiCopTau2Par(tau=0.5,family=family)
my.data<-simulate_joint(n,mu,delta,lambda,theta,family)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
