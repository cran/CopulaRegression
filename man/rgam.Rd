\name{rgam}
\alias{rgam}

\title{Samples from a Gamma variable
}
\description{
Samples from a  Gamma-distributed variable in mean parametrization
}
\usage{
rgam(n,mu,delta)
}
\arguments{
  \item{n}{number of samples
}
  \item{mu}{mean parameter; either a number or a vector of length n
}
\item{delta}{dispersion parameter;  a number
}
}

\value{n samples
}
\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{pgam}}, \code{\link{dgam}}
}
\examples{
n=20
mu<-5 
delta<-1
out<-rgam(n,mu,delta)
}

\keyword{distribution}
\keyword{datagen}
