\name{loglik_joint}
\alias{loglik_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Loglikelihood of the joint  regression model
}
\description{Loglikelihood of the joint regression model
}
\usage{
loglik_joint(alpha,beta,theta, delta, x, y, R, S, family, exposure, negative = TRUE,zt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{The regression coefficients for the Gamma regression
}
\item{beta}{The regression coefficients for the (zero-truncated) Poisson regression
}
\item{theta}{The copula parameter
}
  \item{delta}{dispersion parameter of the Gamma distribution
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
  \item{negative}{boolean, if TRUE the negative of the loglikelihood is returned. Default is TRUE.
}
\item{zt}{logical. If \code{zt=TRUE}, we use a zero-truncated Poisson variable. Otherwise, we use a Poisson variable. Default is \code{TRUE}.
} 
}
\details{For a Gamma distributed variable X and a (zero truncated) Possion variable Y, the loglikelihood is given by
\deqn{\ell=\sum_{i=1} ^n \left(f_X(x_i) \left(D_u(F_Y(y_i),F_X(x_i)|\theta) - D_u(F_Y(y_i -1),F_X(x_i)|\theta) \right)\right)\,. }Here \eqn{D_u} is the h-function of a copula  famila \code{family} with copula parameter \code{theta}. 
}
\value{
loglikelihood 
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

}
\author{Nicole Kraemer, Daniel Silvestrini
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{density_joint}}
}
\examples{
 n<-200 # number of examples
 R<-S<-cbind(rep(1,n),rnorm(n)) # design matrices with intercept 
 alpha<-beta<-c(1,-1) # regression coefficients
 exposure<-rep(1,n) # constant exposure
 delta<-0.5 # dispersion parameter
 tau<-0.3 # Kendall's tau
 family=3 # Clayton copula
 theta<-BiCopTau2Par(tau=tau,family=family)
 # simulate data
  my.data<-simulate_regression_data(n,alpha,beta,R,S,delta,tau,family,TRUE,exposure)
 x<-my.data[,1]
 y<-my.data[,2]
 #compute loglikelihood for the true coefficients
 out<-loglik_joint(alpha,beta,theta,delta,x,y,R,S,family,exposure)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{distribution}
\keyword{multivariate}
