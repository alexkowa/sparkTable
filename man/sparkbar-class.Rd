\name{sparkbar-class}
\Rdversion{1.1}
\docType{class}
\alias{sparkbar-class}

\title{Class \code{"sparkbar"}}
\description{
  This class defines data objects holding all information required to plot sparkbars.
}
\section{Objects from the Class}{
  Objects can be created by using function \code{\link{newSparkBar}}.
}
\section{Slots}{
  \describe{
    \item{\code{barCol}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{barWidth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{barSpacingPerc}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{width}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{height}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{values}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{padding}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{availableWidth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{availableHeight}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{stepWidth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{coordsX}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{coordsY}:}{Object of class \code{"numeric"} ~~ }
  }
}

\section{Methods}{
No methods defined with class "sparkbar" in the signature.
}

\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{newSparkBar}}, \code{\link{plot}}, \code{\link{export}}, \code{\link{setParameter}}, \code{\link{getParameter}}}

\examples{
showClass("sparkbar")
}
\keyword{classes}
