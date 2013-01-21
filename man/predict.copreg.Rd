\name{predict.copreg}
\alias{predict.copreg}
\alias{predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of the copula regression model}
\description{This function predicts the outcome of a copula regression model for new data.
}
\usage{
\method{predict}{copreg}(object,Rtest,Stest,exposure=rep(1,nrow(Stest)),independence=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{copreg} object returned from \code{copreg}
}
  \item{Rtest}{design matrix of the new data for the Gamma model
}
  \item{Stest}{design matrix of the new data for the zero truncated Poisson model
}
  \item{exposure}{exposure time for the zero-truncated Poisson model, all entries of the vector have to be \eqn{>0}. Default is a constant vector of 1.
}
  \item{independence}{logical. Do we assume that the two variables are independent. Default is FALSE.
}
\item{...}{other parameters}
 
}
\details{For new data that is defined by the design matrices \code{Rtest} and \code{Stest}, and the exposure time \code{exposure}, the function predicts the values x of the Gamma variable, the values y of the (zero truncated) Poisson variable, and the policy loss. If \code{independence=TRUE}, the function predicts the policy loss under the assumption that X and Y are independent. 
}
\value{
\item{x.pred}{predicted value of x}
\item{y.pred}{predicted value of y}
\item{l.pred}{predicted value of the policy loss}
}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

}
\author{Nicole Kraemer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{copreg}}, \code{\link{simulate_regression_data}}
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
 
 # joint model without standard errors
 my.model<-copreg(x,y,R,S,family,exposure,FALSE,TRUE)
 
# fitted values

\dontrun{out<-predict(my.model,R,S)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
