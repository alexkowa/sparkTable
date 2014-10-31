\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,sparkbar-method}
\alias{plot,sparkbox-method}
\alias{plot,sparkline-method}
\alias{plot,sparkhist-method}
\title{Plot objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox'}
\description{
  Function that calls plot-methods for objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox'.
}
\usage{
  plot(x, y, ...)
}
\arguments{
  \item{x}{an object of class 'sparkline', 'sparkbox' or 'sparkbar'.}
  \item{y}{not used, only for compatibility.}
  \item{...}{additional parameters passed, currently not used.}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\examples{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]
  a <- newSparkLine(values=x, pointWidth=8)
  plot(a)
}
