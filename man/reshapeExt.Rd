\name{reshapeExt}
\alias{reshapeExt}
\title{Reshaping datasets}
\description{
  reshapeExt() can be used to transform data that are already in 'long' format
  to the form that the data can be used by \code{\link{newSparkTable}} or \code{\link{newGeoTable}}.
}
\usage{
  reshapeExt(x,timeValues=NULL,geographicVar=NULL,...)
}
\arguments{
  \item{x}{data frame}
  \item{timeValues}{if specified, vector of valid time-points}
  \item{geographicVar}{if specified, name of a variable in x holding regional information.}
  \item{...}{additional parameter used for reshape()}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }
		
\seealso{\code{\link{setParameter}}, \code{\link{getParameter}},\code{\link{reshape}}}
				
\examples{
	data(pop,package='sparkTable')
	content <- list(
		function(x) { round(mean(x),2) },
		newSparkBox(), newSparkLine(), newSparkBar(),
		function(x) { round(tail(x,1),2) })
	names(content) <- paste('column',1:5,sep='')
	varType <- rep('value',5)
	pop <- pop[,c('variable','value','time')]
	pop$time <- as.numeric(as.character(pop$time))
	xx <- reshapeExt(pop, idvar='variable', varying=list(2))
	x1 <- newSparkTable(xx, content, varType)
	#plotSparkTable(x1, outputType='html', graphNames='o2',filename='t1')
}
