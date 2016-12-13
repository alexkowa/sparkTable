\name{customizeSparkTable}
\alias{customizeSparkTable}
\title{Customize a sparkTable within a shiny App}
\description{
 Interactive modification of an \code{\link{sparkTable-class}}
}
\usage{
  customizeSparkTable(object, outputDir=getwd())
}
\arguments{
  \item{object}{an object of class \code{\link{sparkTable-class}}}
  \item{outputDir}{a path to a directory for the output (Default=current working directory)}
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
xx <- reshapeExt(pop, varying=list(2))
x1 <- newSparkTable(xx, content, varType)
x1@dataObj$v1 <- rpois(nrow(xx),1)
x1@dataObj$v2 <- rpois(nrow(xx),2)
x1@dataObj$v3 <- rpois(nrow(xx),3)

customizeSparkTable(x1, outputDir=getwd())
}
}
