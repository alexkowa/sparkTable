\name{newGeoTable}
\alias{newGeoTable}
\title{Functions to create a new object of class 'geoTable'}
\description{
  User-function to create objects of the class 'geoTable'.
}
\usage{
  newGeoTable(dataObj, tableContent, varType, geographicVar, geographicInfo=NULL)
}
\arguments{
  \item{dataObj}{a data frame containing information to be plotted.}
  \item{tableContent}{a list with elements of class 'sparkline','sparkbox','sparkbar' or 'function'}
  \item{varType}{a character vector containing variable names existing in dataObj.}
  \item{geographicVar}{a character variable of length 1 with a variable name of dataObj that holds regional information.}
  \item{geographicInfo}{if specified, a data.frame containing 3 columns.
  \itemize{
      \item first column: row-indices
      \item second column: column-indices
      \item third column: regional codes
    }
   }
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{
  \code{\link{export}}
}

\examples{
  ###Example EU population and debt
  data(popEU,package="sparkTable")
  data(debtEU,package="sparkTable")
  data(coordsEU,package="sparkTable")
  popEU <- popEU[popEU$country\%in\%coordsEU$country,]
  debtEU <- debtEU[debtEU$country\%in\%coordsEU$country,]
  EU <- cbind(popEU,debtEU[,-1])
  EUlong <- reshapeExt(EU,idvar="country",v.names=c("pop","debt"),
    varying=list(2:13,14:25),geographicVar="country",timeValues=1999:2010)
  l <- newSparkLine()
  l <- setParameter(l, 'lineWidth', 2.5)
  content <- list(function(x){"Population:"},l,function(x){"Debt:"},l)
  varType <- c(rep("pop",2),rep("debt",2))
  xGeoEU <- newGeoTable(EUlong, content, varType,geographicVar="country",
    geographicInfo=coordsEU)
}
