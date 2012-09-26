\name{ztp.glm}
\alias{ztp.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{GLM for a zero truncated Poisson variable
}
\description{
Zero truncated generalized linear model.
}
\usage{
ztp.glm(y, S, exposure = rep(1, length(y)),sd.error=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector of response values
}
  \item{S}{design matrix
}
  \item{exposure}{exposure time for the zero-truncated Poisson model, all entries of the vector have to be \eqn{>0}. Default is a constant vector of 1.
}
\item{sd.error}{logical. Should the standard errors of the regression coefficients be returned? Default is FALSE.}
}
\details{We consider  positive count variables \eqn{Y_i}. We model  \eqn{Y_i} in terms of a covariate 
vector \eqn{s_i}. The generalized linear model is 
specified via 

 \deqn{Y_i\sim ZTP(\lambda_{i})} with 
\eqn{\ln(\lambda_{i})=\ln(e_i)+{s_i}^\top \beta}. Here \eqn{e_i} denotes the 
exposure time. 

}
\value{
\item{coefficients}{estimated regression coefficients}
\item{sd}{estimated standard error, if \code{sd.error=TRUE}}
}

\author{Nicole Kraemer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{mle_marginal}}, \code{\link{mle_joint}}
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
 my.data<-simulate_regression_data(n,alpha,beta,R,S,delta,tau,family,exposure)
 x<-my.data[,1]
 y<-my.data[,2]
# fit marginal ZTP-model with standard errors
my.model<-ztp.glm(y,S,exposure=exposure,TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
