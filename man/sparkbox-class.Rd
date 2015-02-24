\name{sparkbox-class}
\Rdversion{1.1}
\docType{class}
\alias{sparkbox-class}

\title{Class \code{"sparkbox"}}
\description{
  This class defines data objects holding all information required to plot sparkboxes.
}
\section{Objects from the Class}{
  Objects can be created by using function \code{\link{newSparkBox}}.
}
\section{Slots}{
  \describe{
    \item{\code{outCol}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{boxCol}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{bgCol}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{boxLineWidth}:}{Object of class \code{"numeric"} ~~ }
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
No methods defined with class "sparkbox" in the signature.
}

\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{newSparkBox}}, \code{\link{plot}}, \code{\link{export}} \code{\link{setParameter}}, \code{\link{getParameter}}}

\examples{
showClass("sparkbox")
}
\keyword{classes}
