\name{newSparkBar}
\alias{newSparkBar}
\alias{newSparkLine}
\alias{newSparkBox}
\alias{newSparkHist}
\title{Functions to create new Spark object}
\description{
  Basic functions to create objects of the class 'spark'. The functions are the base for creating a graphical table.
}
\usage{
newSparkLine(width=NULL, height=NULL, values=NULL, padding=NULL, allColors=NULL,
  pointWidth=NULL, lineWidth=NULL, showIQR=NULL, vMin=NULL, vMax=NULL,outputType="html")
newSparkBar(width=NULL, height=NULL, values=NULL, padding=NULL, barCol=NULL,
  barWidth=NULL, barSpacingPerc=NULL, vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html")
newSparkBox(width=NULL, height=NULL, values=NULL, padding=NULL, boxOutCol=NULL,
    boxMedCol=NULL, boxShowOut=NULL, boxCol=NULL, boxLineWidth=NULL,
    vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html")
newSparkHist(width=NULL, height=NULL, values=NULL, padding=NULL, barCol=NULL,
  barWidth=NULL, barSpacingPerc=NULL, vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html")
}
\arguments{
  \item{width}{described in \code{\link{setParameter}}}
  \item{height}{described in \code{\link{setParameter}}}
  \item{values}{described in \code{\link{setParameter}}}
  \item{padding}{described in \code{\link{setParameter}}}
  \item{allColors}{described in \code{\link{setParameter}}}
  \item{pointWidth}{described in \code{\link{setParameter}}}
  \item{lineWidth}{described in \code{\link{setParameter}}}
  \item{showIQR}{described in \code{\link{setParameter}}}
  \item{vMin}{numeric vector of length 1 definining minimum value required for data scaling}
  \item{vMax}{numeric vector of length 1 definining maximum value required for data scaling}
  \item{barCol}{described in \code{\link{setParameter}}}
  \item{barWidth}{described in \code{\link{setParameter}}}
  \item{barSpacingPerc}{described in \code{\link{setParameter}}}
  \item{boxOutCol}{character vector of length 1 defining the color of outliers in spark boxplots}
  \item{boxMedCol}{character vector of length 1 defining the color of median line in spark boxplots}
  \item{boxShowOut}{logical vector specifying if outliers should be displayed in spark boxplots}
  \item{boxCol}{described in \code{\link{setParameter}}}
  \item{boxLineWidth}{described in \code{\link{setParameter}}}
  \item{bgCol}{described in \code{\link{setParameter}}}
  \item{outputType}{described in \code{\link{plot}}}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{plot}}, \code{\link{export}}, \code{\link{setParameter}}, \code{\link{getParameter}}
}

\examples{
  \dontrun{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]

  ### SparkLine
  a <- newSparkLine(values=x, pointWidth=8)
  export(a, outputType='png', filename='testLine1')

  a <- setParameter(a, sample(1:10, 15, replace=TRUE), type='values')
  getParameter(a, type='values')

  a <- setParameter(a, c("darkred", "darkgreen","darkblue", "white", "black", "red"),
    type='allColors')
  getParameter(a, type='allColors')

  a <- setParameter(a, 3, type='pointWidth')
  a <- setParameter(a, 1, type='lineWidth')

  export(a, outputType="pdf", filename='testLine2')

  a <- setParameter(a, 6, type='width')
  a <- setParameter(a, .6, type='height')
  export(a, outputType='eps', filename='testLine2')

  ### SparkBar
  b <- newSparkBar(values=x-min(x))
  getParameter(b, type='values')

  b <- setParameter(b, c("darkred", "darkgreen","black"), type='barCol')
  export(b, outputType='pdf', filename='testBar1')

  b <- setParameter(b, 0:10, type='values')
  export(b, outputType='pdf', filename='testBar2')

  b <- setParameter(b, 0:-10, type='values')
  export(b, outputType='pdf', filename='testBar3')

  ### SparkBox
  cc <- newSparkBox(values=x)
  cc <- setParameter(cc, "darkgreen", type='outCol')
  getParameter(cc, type='outCol')
  cc <- setParameter(cc, c("black","red"), type='boxCol')

  export(cc, outputType='pdf', filename='testBox1')

  cc <- setParameter(cc, c("black","darkgreen"), type='boxCol')
  cc <- setParameter(cc, "darkred", type='outCol')
  export(cc, outputType='pdf', filename='testBox2')

  ###SparkHist
  hh <- newSparkHist(values=rnorm(100))
  export(hh, outputType='pdf', filename='testHist1')
  }
}
