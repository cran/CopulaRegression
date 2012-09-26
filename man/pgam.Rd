\name{pgam}
\alias{pgam}

\title{Distribution of a Gamma variable
}
\description{
Cumulative distribution function of a Gamma-distributed variable in mean parametrization
}
\usage{
pgam(y,mu,delta)
}
\arguments{
  \item{y}{vector of length n
}
  \item{mu}{mean parameter; either a number or a vector of length n
}
\item{delta}{dispersion parameter;  a number
}
}

\value{cumulative distribution function, evaluated at y, this is a vector of length n
}
\author{Nicole Kraemer
}

\seealso{\code{\link{pztp}}, \code{\link{dgam}}
}
\examples{
y<-0:10
mu<-5 
delta<-1
out<-pgam(y,mu,delta)
}

\keyword{distribution}

