\name{sparkTable-class}
\Rdversion{1.1}
\docType{class}
\alias{sparkTable-class}

\title{Class \code{"sparkTable"}}
\description{
  This class defines data objects holding all information required to create a sparkTable.
}
\section{Objects from the Class}{
  Objects can be created by using function \code{\link{newSparkTable}}.
}
\section{Slots}{
  \describe{
    \item{\code{dataObj}:}{Object of class \code{"dfOrNULL"} ~~ }
    \item{\code{varType}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{tableContent}:}{Object of class \code{"listOrNULL"} ~~ }
  }
}

\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{export}}}

\examples{
showClass("sparkTable")
}
\keyword{classes}
