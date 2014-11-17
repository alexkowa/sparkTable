\name{newSparkTable}
\alias{newSparkTable}
\title{Function to create new SparkTable object}
\description{
  User-function to create objects of the class 'sparkTable'.
}
\usage{
  newSparkTable(dataObj, tableContent, varType)
}
\arguments{
  \item{dataObj}{a data frame containing information to be plotted.}
  \item{tableContent}{a list with elements of class 'sparkline','sparkbox','sparkbar' or 'function'}
  \item{varType}{a character vector containing variable names existing in dataObj.}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{plot}}, \code{\link{export}}, \code{\link{setParameter}}, \code{\link{getParameter}}
}

\examples{
  \dontrun{
  #Population
  data(pop)
  b <- newSparkBox()
  l <- newSparkLine()
  bb <- newSparkBar()
  content <- list(function(x) { round(mean(x),2) },
    b,l,bb,
    function(x) { round(tail(x,1),2) })
  names(content) <- paste("column",1:5,sep="")
  varType <- rep("value",5)
  pop <- pop[,c("variable","value","time")]
  pop$time <- as.numeric(as.character(pop$time))
  xx <- reshapeExt(pop,idvar="variable", varying=list(2))
  x1 <- newSparkTable(xx, content, varType)
  export(x1, outputType="html", graphNames="out1")
  }
}
