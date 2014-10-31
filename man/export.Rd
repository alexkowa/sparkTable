\name{export-methods}
\docType{methods}
\alias{export}
\alias{export-methods}
\alias{export,sparkbar-method}
\alias{export,sparkbox-method}
\alias{export,sparkline-method}
\alias{export,sparkhist-method}
\alias{export,sparkTable-method}
\title{Saves objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox' to a file}
\description{
  Spark-Objects are plotted and saved into different file-formats while for objects of class \code{\linkS4class{sparkTable}} all required graphs are stored and the necessary code to include the graphical table in the desired format is returned to the promt.
}
\usage{
  export(object, ...)
  \S4method{export}{sparkline}(object, outputType="pdf", filename="sparkLine", ...)
  \S4method{export}{sparkbar}(object, outputType="pdf", filename="sparkBar", ...)
  \S4method{export}{sparkhist}(object, outputType="pdf", filename="sparkHist", ...)
  \S4method{export}{sparkbox}(object, outputType="pdf", filename="sparkBox", ...)
  \S4method{export}{sparkTable}(object, outputType="html", filename=NULL, graphNames="out",infonote=TRUE, scaleByCol=FALSE,...)
}
\arguments{
  \item{object}{an object of class 'sparkline', 'sparkbox' or 'sparkbar'.}
  \item{outputType}{for objects of class \code{\linkS4class{sparkline}}, \code{\linkS4class{sparkbar}} or \code{\linkS4class{sparkbox}} a character vector specifying the desired output formats. Supported formats are:
    \itemize{
      \item 'pdf': a pdf image is produced
      \item 'eps': an eps image is procuded
      \item 'png': a png image is procuded
    }
    For objects of class \code{\linkS4class{sparkTable}} a character vector specifying the desired output format, supported types are:
    \itemize{
      \item "tex": latex output is produced
      \item "html": html output is generated
    }
  }
  \item{filename}{the filename of the output (minus '.pdf', 'eps','.eps' for single graphs, minus '.tex' or '.html' for graphical tables}
  \item{graphNames}{the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{infonote}{TRUE/FALSE if the latex command should be included in the output, only used for graphical tables}
  \item{scaleByCol}{Either TRUE/FALSE to set the scaling for all columns or a TRUE/FALSE vector of the same length as the content object. Keeps the scaling the same in all rows of a column. This parameter is used only for graphical tables (\code{\linkS4class{sparkTable}} or \code{\linkS4class{geoTable}})}
  \item{...}{additional parameters to be passed, currently not used}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\examples{
data(pop)
x <- pop[pop[,2]=="Insgesamt",3]
a <- newSparkLine(values=x, pointWidth=8)
export(a, outputType=c('pdf','png'), filename='myFirstSparkLine')

## simple graphical table
#data(pop,package="sparkTable")
#b <- newSparkBox()
#l <- newSparkLine()
#bb <- newSparkBar()
#content <- list(function(x) { round(mean(x),2) },
#  b,l,bb,function(x) { round(tail(x,1),2)
#})
#names(content) <- paste("column",1:length(content),sep="")
#varType <- rep("value",length(content))
#pop <- pop[,c("variable","value","time")]
#pop$time <- as.numeric(as.character(pop$time))
#xx <- reshapeExt(pop,idvar="variable", varying=list(2))
#x1 <- newSparkTable(xx, content, varType)
#export(x1, outputType="html", graphNames="o2",filename="t1")
#export(x1, outputType="tex", graphNames="o3",filename="t2")

## Example Histogram+Boxplot with 2 variables in 10 different groups
#datEx <- data.frame(
#  variable=sample(paste("Cat",1:10,sep="_"),1000,replace=TRUE),
#  value=rnorm(1000),
#  value2=rlnorm(1000))
#b <- newSparkBox()
#h <- newSparkHist()
#content <- list(
#  function(x) { round(mean(x),2) },
#  function(x) { round(median(x),2) },
#  function(x) { round(quantile(x,.25),2) },
#  function(x) { round(quantile(x,.75),2) },
#  b,h,
#  function(x) { round(mean(x),2) },
#  function(x) { round(median(x),2) },
#  function(x) { round(quantile(x,.25),2) },
#  function(x) { round(quantile(x,.75),2) },
#  b, h
#)
#names(content) <- c(
#  paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v1",sep=""),
#  paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v2",sep="")
#)
#varType <- c(rep("value",length(content)/2),rep("value2",length(content)/2))
#datEx <- reshapeExt(datEx,idvar="variable", varying=list(2,3))
#x2 <- newSparkTable(datEx, content, varType)
#export(x2, outputType="html", graphNames="o2",filename="t1")
}
