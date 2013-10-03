\name{setParameter}
\alias{setParameter}
\title{Functions to interact with a Sparkline object}
\description{
  Basic functions to set parameters for objects of class 'sparkline', 'sparkbar', 
  'sparkbox', 'sparkTable' or 'geoTable'.
}
\usage{
	setParameter(object, value, type)
}
\arguments{
  \item{object}{objects of class 'sparkline', 'sparkbar', 'sparkbox', 'sparkTable' or 'geoTable'}
  \item{type}{one of the following:
	\itemize{
	   \item 'width': set/change slot 'width' for objects of class 'spark' and classes that directly extend this class.
	   \item 'height': set/change slot 'height' for objects of class 'spark' and classes that directly extend this class.
	   \item 'values': set/change slot 'values' for objects of class 'spark' and classes that directly extend this class.
	   \item 'padding': set/change slot 'padding' for objects of class 'spark' and classes that directly extend this class.
	   \item 'allColors': set/change slot 'allColors' for objects of class 'sparkline'.
	   \item 'lineWidth': set/change slot 'lineWidth' for objects of class 'sparkline'.
	   \item 'pointWidth': set/change slot 'pointWidth' for objects of class 'sparkline'.
	   \item 'showIQR': set/change slot 'showIQR' for objects of class 'sparkline'.
	   \item 'boxCol': set/change slot 'boxCol' for objects of class 'sparkbox'.
	   \item 'outCol': set/change slot 'outCol' for objects of class 'sparkbox'.	 
	   \item 'boxLineWidth': set/change slot 'boxLineWidth' for objects of class 'sparkbox'.
	   \item 'barCol': set/change slot 'barCol' for objects of class 'sparkbar'.	 	   
	   \item 'barSpacingPerc': set/change slot 'barSpacingPerc' for objects of class 'sparkbar'.
	   \item 'dataObj':	set/change slot 'dataObj' for objects of class 'sparkTable' or 'geoTable'.	
	   \item 'tableContent': set/change slot 'tableContent' for objects of class 'sparkTable' or 'geoTable'.	
	   \item 'varType':	set/change slot 'varType' for objects of class 'sparkTable' or 'geoTable'.		   
	   \item 'geographicVar': set/change slot 'geographicVar' for objects of class 'geoTable'.		   	
	   \item 'geographicInfo': set/change slot 'geographicInfo' for objects of class 'geoTable'.	
	   \item 'geographicOrder':	set/change slot 'geographicOrder' for objects of class 'geoTable'.	
	}  
  }
  \item{value}{	values that are used to updated the slot chosen with argument 'type':
	\itemize{
	   \item if type=='width': numeric vector of length 1 defining the width of the resulting plot
	   \item if type=='height': numeric vector of length 1 defining the height of the resulting plot
	   \item if type=='values': numeric vector defining the values to be plotted
	   \item if type=='padding': numeric vector of length 4 defining the padding of the plot in percent. The order is: top,bottom,left,right.
	   \item if type=='allColors': a character vector of length 6 (including NA's) containing colors. The elements of the color vector are used as:
	   \itemize{
	   	\item first element: color for minimal value
	   	\item second element: color for maximal value
	   	\item third element: color for last value
	   	\item fourth element: color for filling
	   	\item fifth element: color for the line
	   	\item sixth element: color for interquartil range
	   }
	   \item if type=='lineWidth': numeric vector of length 1 defining the line width of the resulting sparkline
	   \item if type=='pointWidth': numeric vector of length 1 defining the width of points (min, max, last) of the resulting sparkline.
	   \item if type=='showIQR': logical vector of length 1 defining if the IQR of the data should be plotted in the sparkline.
	   \item if type=='boxCol': character vector of length 2 defining colors to be used in a sparkbox plot.
	   \itemize{
	   	\item first element: color of the lines surrounding the boxes
	   	\item second element: fill color of the box
	   }
	   \item if type=='outCol': character vector of length 1 defining the color of outliers in a sparkboxplot.
	   \item if type=='boxLineWidth': numeric vector of length 1 defining the width of the surrounding lines of a sparkboxplot.
	   \item if type=='barCol': character vector of length 3 defining colors to be used in a sparkbar plot.
	   \itemize{
	   	\item first element: color of bars showing negative values 
	   	\item second element: color of bars showing positive values 
	   	\item third element: color of lines in the plot 
	   }
	   \item if type=='barSpacingPerc': numeric vector of length 1 defining the spacing in percent used between the bars in the sparkbar plot
	   \item if type=='dataObj': a data frame containing information to be plotted.		
	   \item if type=='tableContent': a list with elements of class 'sparkline','sparkbox','sparkbar' or 'function'
	   \item if type=='varType': a character vector containing variable names existing in dataObj.		   
	   \item if type=='geographicVar': a character variable of length 1 with a variable name of dataObj that holds regional information.  	   	
	   \item if type=='geographicInfo': a data.frame with information on coordinates of regions to be plotted.	
	   \item if type=='geographicOrder': a data.frame containing 3 columns that is usually automatically created. 
	   \itemize{
	   	\item first column: row-indices
	   	\item second column: column-indices
	   	\item third column: regional codes
	   }
	} 	
  }
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{getParameter}}}

\examples{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]
  a <- newSparkLine(values=x, pointWidth=8)
  
  a <- setParameter(a, type='values', value=sample(1:10, 15, replace=TRUE))
  getParameter(a, 'values')

  a <- setParameter(a, type='allColors', value=c("darkred", "darkgreen","darkblue", "white", "black", "red"))
  getParameter(a, 'allColors')

  getParameter(a, 'pointWidth')
  a <- setParameter(a, type='pointWidth', value=3)
  getParameter(a, 'pointWidth')
 
  a <- setParameter(a, type='lineWidth', value=1)
  a <- setParameter(a, type='width', value=6)
  a <- setParameter(a, type='height', value=.6)
}
