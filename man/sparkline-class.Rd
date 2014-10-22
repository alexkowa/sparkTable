\name{sparkline-class}
\Rdversion{1.1}
\docType{class}
\alias{sparkline-class}

\title{Class \code{"sparkline"}}
\description{
  This class defines data objects holding all information required to plot sparklines.
}
\section{Objects from the Class}{
  Objects can be created by using function \code{\link{newSparkLine}}.
}
\section{Slots}{
  \describe{
    \item{\code{allColors}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{pointWidth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{lineWidth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{showIQR}:}{Object of class \code{"logical"} ~~ }
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
	No methods defined with class "sparkline" in the signature.
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{newSparkLine}}, \code{\link{plot}}, \code{\link{export}}, code{\link{setParameter}}, \code{\link{getParameter}}}

\examples{
showClass("sparkline")
}
\keyword{classes}
