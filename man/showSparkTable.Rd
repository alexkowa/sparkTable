\name{showSparkTable-methods}
\docType{methods}
\alias{showSparkTable}
\alias{showSparkTable-methods}
\alias{showSparkTable,sparkTable-method}
\alias{showSparkTable,data.frame-method}
\title{ Look at your sparkTable in a shiny App}
\description{
 ~~ Methods for function \code{showSparkTable}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "sparkTable")}}{
%%  ~~describe this method here~~
}
}}
\usage{
  showSparkTable(object, outputDir=tempdir(),outputType="html", filename=NULL,
        graphNames="out", ...)
}
\arguments{
  \item{object}{an object of class 'sparkTable' or 'data.frame'}
  \item{outputDir}{a path to a directory for the output (Default=temporary directory)}
  \item{outputType}{a character vector of length one specifying the desired output format:
  \itemize{
    \item 'tex': latex output is produced (does not work)
    \item 'html': html output is procuded
  }}
  \item{filename}{ the filename of the output (minus '.tex' or '.html'}
  \item{graphNames}{ the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{...}{additional parameters to be passed ro plotSparkTable and summaryST}
}
\seealso{\code{\link{plotSparkTable}}}
\examples{
\dontrun{
data(pop,package="sparkTable")
b <- newSparkBox()
l <- newSparkLine()
bb <- newSparkBar()
content <- list(function(x) { round(mean(x),2) },
  b,
  l,
  bb,
  function(x) { round(tail(x,1),2) }
)
names(content) <- paste("column",1:length(content),sep="")
varType <- rep("value",length(content))
pop <- pop[,c("variable","value","time")]
pop$time <- as.numeric(as.character(pop$time))
xx <- reshapeExt(pop,idvar="variable", varying=list(2))
x1 <- newSparkTable(xx, content, varType)
showSparkTable(x1)

#Example Hist+Box with 2 variables in 10 different groups
datEx <- data.frame(
  variable=sample(paste("Cat",1:10,sep="_"),1000,replace=TRUE),
  value=rnorm(1000),value2=rlnorm(1000)
)
b <- newSparkBox()
h <- newSparkHist()
content <- list(
  function(x) { round(mean(x),2) },
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
names(content) <- c(
  paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v1",sep=""),
  paste(c("Mean","Median","Q25","Q75","Boxplot","Histogram"),"_v2",sep="")
)
varType <- c(rep("value",length(content)/2),rep("value2",length(content)/2))
datEx <- reshapeExt(datEx,idvar="variable", varying=list(2,3))
x2 <- newSparkTable(datEx, content, varType)
showSparkTable(x2)

#Example for the data.frame method (uses summaryST)
data2 <- data.frame(x1=rnorm(100),x2=rnorm(100)+1,x3=rnorm(100)+5)
showSparkTable(data2)
}
}
