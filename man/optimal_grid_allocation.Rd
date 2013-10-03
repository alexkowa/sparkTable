\name{optimal_grid_allocation}
\alias{optimal_grid_allocation}
\title{Optimal Allocation of Coordinates to a grid}
\description{
  optimal_grid_allocation() ... \code{\link{newGeoTable}}.
}
\usage{
  optimal_grid_allocation(data, grid.cols=NULL, grid.rows=NULL, addGrid=0, plot=FALSE)
}
\arguments{
  \item{data}{data frame with first column X-coordinate and second column Y-coordinate}
  \item{grid.cols}{number of columns in the grid}
  \item{grid.rows}{number of rows in the grid}
  \item{addGrid}{additional columns and rows in the grid}
  \item{plot}{TRUE/FALSE for plotting the allocation}
}
\author{Alexander Kowarik, Statistics Austria }
		
\seealso{\code{\link{plotGeoTable}}}
				
\examples{
	data <- data.frame(x=c(0,2,1.24,2,1.98,1.62,1.24,1.91,0.48),
	  y=c(2.93,2.45,1.94,1.46,0.98,3,0.70,0.56,0))
rownames(data) <- c("IS","FI","NO","EE","LV","SE","DK","LT","IE")
index <-  optimal_grid_allocation(data,plot=TRUE)
index2 <- optimal_grid_allocation(data,grid.cols=3,grid.rows=4,plot=TRUE)
}
