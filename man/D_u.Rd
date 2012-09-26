\name{D_u}
\alias{D_u}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{H-function of the copula
}
\description{H-function of the copula
}
\usage{
D_u(u, v, theta, family)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{
vector of [0,1] values}
  \item{v}{vector of [0,1] values
}
  \item{theta}{copula parameter
}
  \item{family}{an integer defining the bivariate copula family: 1 = Gauss, 3 = Clayton, 4=Gumbel, 5=Frank
}
}
\details{For a bivariate copula family \eqn{C(u,v|\theta)}, the h-function is defined as
\deqn{D_u(u,v)=\frac{\partial}{\partial u} C(u,v|\theta)}

}
\value{value of the h-function
}

\author{Nicole Kraemer
}
\note{
This is  the function \code{BiCopHfunc} from the \code{VineCopula} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{density_joint}}
}
\examples{
u<-v<-0.5
theta<-0.2
family=1
out<-D_u(u,v,theta,family)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

