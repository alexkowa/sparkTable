\name{geoTable-class}
\Rdversion{1.1}
\docType{class}
\alias{geoTable-class}

\title{Class \code{"geoTable"}}
\description{
  This class defines data objects holding all information required to create a geoTable.
}
\section{Objects from the Class}{
  Objects can be created by using function \code{\link{newGeoTable}}.
}
\section{Slots}{
  \describe{
    \item{\code{dataObj}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{varType}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{tableContent}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{geographicVar}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{geographicInfo}:}{Object of class \code{"dfOrNULL"} ~~ }
    \item{\code{geographicOrder}:}{Object of class \code{"dfOrNULL"} ~~ }
  }
}

\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{plotGeoTable}}}

\examples{
showClass("geoTable")
}
\keyword{classes}
