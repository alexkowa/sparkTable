\name{customizeSparkTable-methods}
\docType{methods}
\alias{customizeSparkTable}
\alias{custromizeSparkTable-methods}
\alias{customizeSparkTable,sparkTable-method}
\title{ Customize a sparkTable within a shiny App}
\description{
 ~~ Methods for function \code{customizeSparkTable}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "sparkTable")}}{
%%  ~~describe this method here~~
}
}}
\usage{
  customizeSparkTable(object, outputDir=getwd(), ...)
}
\arguments{
  \item{object}{an object of class 'sparkTable' or 'data.frame'}
  \item{outputDir}{a path to a directory for the output (Default=current working directory)}
  \item{...}{additional parameters to be passed to \code{\link{export}} and \code{\link{summaryST}}}
}
\seealso{\code{\link{export}}}
\examples{
\dontrun{
data(pop,package="sparkTable")
b <- newSparkBox()
l <- newSparkLine()
bb <- newSparkBar()
content <- list(
  function(x) { round(mean(x),2) },
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
x1@dataObj$v1 <- rpois(nrow(xx),1)
x1@dataObj$v2 <- rpois(nrow(xx),2)
x1@dataObj$v3 <- rpois(nrow(xx),3)

customizeSparkTable(x1, outputDir=getwd())
}
}
