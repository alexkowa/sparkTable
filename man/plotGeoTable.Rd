\name{plotGeoTable-methods}
\docType{methods}
\alias{plotGeoTable-methods}
\alias{plotGeoTable,geoTable-method}
\alias{plotGeoTable}
\title{Plot objects of class 'geoTable'}
\description{
  Function that plots objects of class 'geoTable'.
}
\usage{
	plotGeoTable(object, outputType='html', filename=NULL, graphNames='out',
	  transpose=FALSE, include.rownames=FALSE, include.colnames=FALSE, rownames=NULL,
	  colnames=NULL,...)
}
\arguments{
  \item{object}{an object of class 'geoTable'}
  \item{outputType}{a character vector of length one specifying the desired output format:
	  \itemize{
	  	\item 'tex': latex output is produced
	  	\item 'html': html output is procuded
	  }	
  }
  \item{filename}{ the filename of the output (minus '.tex' or '.html'}
  \item{graphNames}{ the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{transpose}{logical vector of length 1 defining if the plot be transposed}
  \item{include.rownames}{logical vector of length 1 defining if rownames should be included}
  \item{include.colnames}{logical vector of length 1 defining if colnames should be included}
  \item{rownames}{optional chracter vector specifying row names}
  \item{colnames}{optional chracter vector specifying column names} 
  \item{...}{additional parameters to be passed}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

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
	plotGeoTable(xGeoEU, outputType="html", graphNames="outEU",filename="testEUT",
	  transpose=TRUE)
	plotGeoTable(xGeoEU, outputType="html", graphNames="outEU1",filename="testEU",
	  transpose=FALSE)
	plotGeoTable(xGeoEU, outputType="tex", graphNames="out1",filename="testEU",
	  transpose=FALSE)
	plotGeoTable(xGeoEU, outputType="tex", graphNames="out1",filename="testEUT",
	  transpose=TRUE)
}
