\name{theta2z}
\alias{theta2z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transformation of the copula parameter
}
\description{
This function  transforms the copula parameter theta such that \code{theta2z(theta)} is unrestricted.
}
\usage{
theta2z(theta, family)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
}
\details{This function transforms the parameter of a copula such that the transformed value in unconstrained. E.g., for the Gauss copula (\code{family=1}), the parameter \eqn{\theta} lies in the interval \eqn{]-1,1[}, and the transformation is defined as
\deqn{\theta \mapsto\frac{1}{2} \ln \left(\frac{1+\theta}{1-\theta}\right)}
}
\value{transformation
}

\author{Nicole Kraemer
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{z2theta}}
}
\examples{
##---- this is an internal function ----

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}

