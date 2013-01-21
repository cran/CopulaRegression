\name{copreg}
\alias{copreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Joint, copula-based regression model
}
\description{This functions fits a joint, bivariate regression model for a Gamma generalized linear model and a (zero-truncated) Poisson generalized linear model.
}
\usage{
copreg(x,y,R,S=R,family=1,exposure,sd.error=FALSE,joint=TRUE,zt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{n observations of the Gamma variable
}
  \item{y}{n observations of the (zero-truncated) Poisson variable
}
  \item{R}{n x p design matrix for the Gamma model
}
  \item{S}{n x q design matrix for the (zero-truncated) Poisson model
}
\item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
  \item{exposure}{exposure time for the (zero-truncated) Poisson model, all entries of the vector have to be \eqn{>0}. Default is a constant vector of 1.
} \item{sd.error}{logical. Should the standard errors of the regression 
coefficients be returned? Default is \code{FALSE}.} \item{joint}{logical. 
Should the two generalized liner models be estimated jointly? Default is 
\code{TRUE}.} 
\item{zt}{logical. If \code{zt=TRUE}, we fit a marginal zero-truncated Poisson regression model. Otherwise, we fit a marginal Poisson regression model. Default is \code{TRUE}.}
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

Further, mwe assume that the dependency of \eqn{X_i} and \eqn{Y_i} is modeled 
in terms of a copula family with parameter \eqn{\theta}. } 

\value{This is an object of class \code{copreg}
\item{alpha}{estimated coefficients for X, including the intercept}
\item{beta}{estimated coefficients for Y, including the intercept}
\item{sd.alpha}{estimated standard deviation of \code{alpha} (if \code{sd.error=TRUE})}
\item{sd.beta}{estimated standard deviation of \code{beta} (if \code{sd.error=TRUE})}
\item{delta}{estimated dispersion parameter}
\item{theta}{estimated copula parameter if \code{joint=TRUE}, 0 otherwise (in combination with \code{family=1}, 0 corresponds to the independence assumption)}
\item{family}{copula family as provided in the function call if \code{joint=TRUE}, 1 otherwise  (in combination with \code{theta=0}, 1  corresponds to the independence assumption)}
\item{ll}{loglikelihood of the estimated model, evaluated at each observation}
\item{loglik}{overall loglikelihood, i.e. sum of \code{ll}}
\item{alpha0}{estimated coefficients for X under independence, including the intercept}
\item{beta0}{estimated coefficients for Y under independence, including the intercept}
\item{sd.alpha0}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{sd.beta0}{estimated standard deviation (if \code{sd.error=TRUE})}
\item{delta0}{estimated dispersion parameter under independence}
\item{theta0}{0 (in combination with \code{family0=1}, 0 corresponds to the independence assumption)}
\item{family0}{1  (in combination with \code{theta0=0}, 1  corresponds to the independence assumption)}
\item{ll0}{loglikelihood of the estimated model under independence, evaluated at each observation}
\item{loglik0}{overall loglikelihood, under independence, i.e. sum of \code{ll0}}
\item{zt}{The value of \code{zt}
}



\item{tau_IFM}{estimated Kendall's \eqn{\tau} based on the marginal models, using inference from margins}
\item{theta_ifm}{estimated copula parameter, estimated via inference from margins}
}

\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{mle_marginal}},\code{\link{mle_joint}}, 
\code{\link{simulate_regression_data}}} 


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
 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{htest}
\keyword{multivariate}
