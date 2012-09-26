\name{dgam}
\alias{dgam}

\title{Density of a Gamma variable
}
\description{
Density of a Gamma-distributed variable in mean parametrization
}
\usage{
dgam(y,mu,delta)
}
\arguments{
  \item{y}{vector of length n
}
  \item{mu}{mean parameter; either a number or a vector of length n
}
\item{delta}{dispersion parameter;  a number
}
}

\value{density, evaluated at y, this is a vector of length n
}
\author{Nicole Kraemer
}

\seealso{\code{\link{pgam}}
}
\examples{
y<-0:10
mu<-5 
delta<-1
out<-dgam(y,mu,delta)
# out equals
out1<-dgamma(y,shape=1/delta,rate=1/(delta*mu))
}

\keyword{distribution}

