\name{z2theta}
\alias{z2theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse of the parameter transformation
}
\description{This is the inverse of the function \code{\link{theta2z}}}
\usage{
z2theta(z, family)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{value to be transformed
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
}
\details{
The function \code{\link{theta2z}} transforms the parameter of a copula such that the transformed value in unconstrained. This function is its inverse. 
}
\value{transformed value
}

\author{Nicole Kraemer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{theta2z}}
}
\examples{
##---- this is an internal function ----

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}

