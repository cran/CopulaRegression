\name{dztp}
\alias{dztp}

\title{Density of a zero truncated Poisson variable
}
\description{
Probability mass function of a zero truncated Poisson variable
}
\usage{
dztp(y, lambda)
}
\arguments{
  \item{y}{vector of length n
}
  \item{lambda}{either a number or a vector of length n
}
}

\value{density, evaluated at y, this is a vector of length n
}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{pztp}}
}
\examples{
y<-0:10
lambda1<-5 # fixed lambda
out1<-dztp(y,lambda1)
lambda2<-2:12 # variable lambda
out2<-dztp(y,lambda2)
}

\keyword{distribution}

