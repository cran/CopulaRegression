\name{pztp}
\alias{pztp}
\title{Cumulative distribution function of a zero truncated Poisson variable
}
\description{
Cumulative distribution function of a zero truncated Poisson variable
}
\usage{
pztp(y, lambda)
}
\arguments{
  \item{y}{vector of length n
}
  \item{lambda}{either a number or a vector of length n
}
}

\value{cumulative distribution function, evaluated at y, this is a vector of length n
}
\author{Nicole Kraemer
}



\seealso{ \code{\link{dztp}}
}
\examples{
y<-0:10
lambda1<-5 # fixed lambda
out1<-pztp(y,lambda1)
lambda2<-2:12 # variable lambda
out2<-pztp(y,lambda2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

