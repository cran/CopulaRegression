\name{vuongtest}
\alias{vuongtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Model comparison using a Vuong test}
\description{This function compares two bivariate copula-based regression models
}
\usage{
vuongtest(model1,model2,selection="AIC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model1}{\code{copreg} object returned from \code{copreg}
}
  \item{model2}{\code{copreg} object returned from \code{copreg}
}
\item{selection}{model selection criterion. Options are AIC and BIC. Default ist AIC.}
}
\details{Let us denote by \eqn{{ \ell}^{(1)},\,{\bm \ell}^{(2)}} the vectors of pointwise loglikelihoods for a model with copula family 1 and 2 respectively. Here, we assume that both models have the same degrees of freedom, i.e. the same number of parameters. We now compute the differences of the pointwise loglikelihood as
\deqn{m_i:=\ell^{(1)}_i - \ell^{(2)}_i,\ i=1,\ldots,n\,.}
Denote by
\deqn{\overline{m}=\frac{1}{n}\sum_{i=1}^n m_i}
the mean of the differences. The test statistic
\deqn{T_V:= \frac{\sqrt{n}\cdot\overline{m}}{ \sqrt{\sum_{i=1}^n \left(m_i - \overline{m}\right)^2}},}
is asymptotically normally distributed with zero mean and unit variance. Hence, we prefer copula family 1 to copula family 2 at level \eqn{\alpha} if

\deqn{T_V> \Phi^{-1}\left(1-\frac{\alpha}{2}\right)\,,}
where \eqn{\Phi} denotes the standard normal distribution function. If
\deqn{T_V< \Phi^{-1}\left(\frac{\alpha}{2}\right)\,,}
we prefer copula family 2. Otherwise, no decision among the two copula families is possible. If the models contain different numbers of estimated parameters, the test statistic is corrected using either the AIC or BIC criterion.}
\value{value of the test statistic

}
\references{N. Kraemer, E. Brechmann, D. Silvestrini, C. Czado (2012): Total loss estimation using copula-based regression models. Preprint, available at http://arxiv.org/abs/1209.5356

Vuong, Q. H. (1989). Ratio tests for model selection and non-nested hypotheses. Econometrica 57
(2), 307-333.

}
\author{Nicole Kraemer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{copreg}}
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
 my.model.clayton<-copreg(x,y,R,S,family=3,exposure,FALSE,TRUE)
 my.model.gauss<-copreg(x,y,R,S,family=1,exposure,FALSE,TRUE)
 #
 vuongtest(my.model.clayton,my.model.gauss)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
