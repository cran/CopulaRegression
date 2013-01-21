\name{simulate_regression_data}
\alias{simulate_regression_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate regression data 
}
\description{Simulate regression data
}
\usage{
simulate_regression_data(n,alpha,beta,R,S,delta,tau,family,zt,exposure)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{n}{number of samples}
  \item{alpha}{coefficients for the Gamma regression
}
\item{beta}{coefficients for the (zero-truncated) Poisson regression
}

  
  \item{R}{n x p design matrix for the Gamma model
}
  \item{S}{n x q design matrix for the (zero-truncated) Poisson model
}
\item{delta}{dispersion parameter of the Gamma distribution
}
  \item{tau}{Kendalls tau
}

  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
  \item{exposure}{exposure time for the zero-truncated Poisson model, all entries of the vector have to be \eqn{>0}. Default is a constant vector of 1.
}
\item{zt}{logical. If \code{zt=TRUE}, we use a zero-truncated Poisson variable. Otherwise, we use a Poisson variable. Default is \code{TRUE}.
} 
  
}
\details{We consider positive continuous random variables \eqn{X_i} and positive or non-negative
count variables \eqn{Y_i}. We model \eqn{X_i} in terms of a covariate 
vector \eqn{r_i} and \eqn{Y_i} in terms of a covariate 
vector \eqn{s_i}. The marginal regression models are 
specified via 

\deqn{X_i\sim Gamma(\mu_i,\delta)}
with \eqn{\ln(\mu_i)={ r_i} 
^\top \alpha} 
for the continuous variable. For the count variable, if \code{zt=TRUE}, we use a zero-truncated Poisson model,
\deqn{Y_i\sim ZTP(\lambda_{i})}
with \eqn{\ln(\lambda_{i})=\ln(e_i)+{s_i}^\top \beta}. 
Otherwise, we use a Poisson model. \eqn{e_i} denotes the 
exposure time. 
Further,we assume that the dependency of \eqn{X_i} and \eqn{Y_i} is modeled 
in terms of a copula family with parameter \eqn{\theta}.  }

\value{n samples from the joint regression model
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

}
\author{Nicole Kraemer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{simulate_joint}}
}
\examples{
n<-200 # number of examples
 R<-S<-cbind(rep(1,n),rnorm(n)) # design matrices with intercept 
 alpha<-beta<-c(1,-1) # regression coefficients
 exposure<-rep(1,n) # constant exposure
 delta<-0.5 # dispersion parameter
 tau<-0.3 # Kendall's tau
 family=3 # Clayton copula
 # simulate data
 my.data<-simulate_regression_data(n,alpha,beta,R,S,delta,tau,family,TRUE,exposure)
 x<-my.data[,1]
 y<-my.data[,2]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
