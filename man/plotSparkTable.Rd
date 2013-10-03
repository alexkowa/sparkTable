\name{plotSparkTable-methods}
\docType{methods}
\alias{plotSparkTable-methods}
\alias{plotSparkTable,sparkTable-method}
\alias{plotSparkTable}
\title{Plot objects of class 'sparkTable'}
\description{
  Function that plots objects of class 'sparkTable'.
}
\usage{
	plotSparkTable(object, outputType='html', filename=NULL, graphNames='out',infonote=TRUE, scaleByRow=FALSE,...)
}
\arguments{
  \item{object}{an object of class 'sparkTable'}
  \item{outputType}{a character vector of length one specifying the desired output format:
	  \itemize{
	  	\item 'tex': latex output is produced
	  	\item 'html': html output is procuded
	  }	
  }
  \item{filename}{ the filename of the output (minus '.tex' or '.html'}
  \item{graphNames}{ the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{infonote}{TRUE/FALSE if the latex command should be included in the output}
  \item{scaleByCol}{Either TRUE/FALSE to set the scaling for all columns
  or a TRUE/FALSE vector of the same length as the content object. Keeps the scaling the same in all rows of a column.}
  \item{...}{additional parameters to be passed}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }
\seealso{\code{\link{summaryST}}}
\examples{
	data(pop,package="sparkTable")
	b <- newSparkBox()
	l <- newSparkLine()
	bb <- newSparkBar()
	content <- list(function(x) { round(mean(x),2) },
		b,
		l,
		bb,
		function(x) { round(tail(x,1),2) })
	names(content) <- paste("column",1:length(content),sep="")
	varType <- rep("value",length(content))
	pop <- pop[,c("variable","value","time")]
	pop$time <- as.numeric(as.character(pop$time))
	xx <- reshapeExt(pop,idvar="variable", varying=list(2))
	x1 <- newSparkTable(xx, content, varType)
	plotSparkTable(x1, outputType="html", graphNames="o2",filename="t1")
	plotSparkTable(x1, outputType="tex", graphNames="o3",filename="t2")
	#Example Hist+Box with 2 variables in 10 different groups
	datEx <- data.frame(variable=sample(paste("Cat",1:10,sep="_"),1000,replace=TRUE),value=rnorm(1000),value2=rlnorm(1000))
	b <- newSparkBox()
	h <- newSparkHist()
  content <- list(function(x) { round(mean(x),2) },
		function(x) { round(median(x),2) },
		function(x) { round(quantile(x,.25),2) },
		function(x) { round(quantile(x,.75),2) },
		b,
	  h,
	  function(x) { round(mean(x),2) },
    function(x) { round(median(x),2) },
		function(x) { round(quantile(x,.25),2) },
		function(x) { round(quantile(x,.75),2) },
		b,
	  h
	)
	names(content) <- c(paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v1",sep=""),
	paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v2",sep="")
	)
	varType <- c(rep("value",length(content)/2),rep("value2",length(content)/2))
	datEx <- reshapeExt(datEx,idvar="variable", varying=list(2,3))
	x2 <- newSparkTable(datEx, content, varType)
	plotSparkTable(x2, outputType="html", graphNames="o2",filename="t1")
	
}
