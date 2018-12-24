\name{RFortLangComp-package}
\alias{RFortLangComp-package}
\alias{RFortLangComp}

\title{
  R, Fortran, and C versions of layer loss functions for speed comparison
}
\description{
  This package contains various flavors of R, Fortran, and C versions of a simple layer loss cost function. These flavors represent levels of OpenMP parallelism. These functions' performance will be compared with Rcpp versions of the same function in the CppLangComp package. These packages are \bold{not} intended for uploading to CRAN.
}
\details{
  The functions are called by name into the same environment as the functions from CppLangComp and timings are compared.
}
\author{
Maintainer: Avraham Adler \email{Avraham.Adler@gmail.com}
}
\references{
  There will be a blog post on \url{https://www.avrahamadler.com} that will discuss these packages.
}
\keyword{ package }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}


