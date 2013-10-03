\name{checkerplot}
\alias{checkerplot}
\title{Checkerplot}
\description{
   Visualization of regional indicators with the checkerplot
}
\usage{
  checkerplot(data, cols=5, rows=5, geom="line", errorbar=FALSE, title=NULL, title.size=20,
    label.size=11, xbreaks=NULL, xlabels=NULL, ybreaks=NULL, ylabels=NULL, ymin=NULL,
    ymax=NULL, img=NULL, aes_geom=NULL, formatter=NULL, margin_yaxis=0, margin_yaxis2=0,
    margin_xaxis=0, margin_xaxis2=0, opts=NULL, ...) 
}
\arguments{
  \item{data}{data frame}
  \item{cols}{number of columns in the grid}
  \item{rows}{number of rows in the grid}
  \item{geom}{defines the geom, the geometric object, of the plot ("line", "bar", "point" are possible at the moment)}
  \item{errorbar}{TRUE/FALSE plot additional errorbars (testing only)}
  \item{title}{title of the plot, displayed above the plot}
  \item{title.size}{font size of the plot title}
  \item{label.size}{font size of the labels}
  \item{xbreaks}{number of breaks for the x-axis, default it is calculated automatically}
  \item{xlabels}{labels of the breaks (optional)}
  \item{ybreaks}{number of breaks for the y-axis, default it is calculated automatically}
  \item{ylabels}{labels of the breaks (optional)}
  \item{ymin}{minimum value of y-axis displayed, if not provided it will be automatically calculated}
  \item{ymax}{maximum value of x-axis displayed, if not provided it will be automatically calculated}
  \item{img}{vector containing all names (must equal the label column in the dataframe data) of 
  pictures (pnm files) displayed beside the label}
  \item{aes_geom}{ggplot2 object to define the aesthetics of the geom}
  \item{formatter}{allows to format the y-axis (scale_x_continuous(formatter = formatter))}
  \item{margin_yaxis}{allows to adjust the distance from the plot to the left border of the grid for all elements
    with an y-axis and its annotation, because the difference number formats on the y-axis might 
  lead to a little displaced plot area} 
  \item{margin_yaxis2}{allows to adjust the distance from the plot to the left border of the grid for all elements
    with an y-axis but without annotation}
  \item{margin_xaxis}{allows to adjust the distance from the plot to the left border of the grid for all elements
    with an x-axis and annotation, because the difference number formats on the y-axis might 
  lead to a little displaced plot area}
  \item{margin_xaxis2}{allows to adjust the distance from the plot to the left border of the grid for all elements
    with an x-axis but without annotation}

  \item{opts}{ggplot2 object to change settings in the ggplot2 plots}
  \item{...}{further arguments passed through}
}
\author{Karin Fuerst, Alexander Kowarik, Matthias Templ}
		
\seealso{\code{\link{optimal_grid_allocation}}, \code{\link{plotGeoTable}}}
				
\examples{
### Directory of the package with flags
dirflags = paste(searchpaths()[grep("sparkTable", searchpaths())], "/etc/Flaggen/", sep="")
### EXAMPLE for EUROPE
  data(EU_data)
  order_eu = optimal_grid_allocation(EU_data[,16:17],8,7)
  colnames(EU_data)[1] <- c("country")
  EU_data[,18] <- order_eu
  colnames(EU_data)[1] <- c("country")
  colnames(EU_data)[18] <- c("order")
  data_eu = data.frame(cbind(rep(1997,34)), EU_data$unempl_f_1997, EU_data$country, EU_data$order)
  colnames(data_eu) = c("x", "y", "names", "order")
  for(year in 1998:2008){
    XX <- data.frame(cbind(rep(year,34)), EU_data[,paste("unempl_f_",year,sep="")], EU_data$country, EU_data$order)
    colnames(XX) = c("x", "y", "names", "order")
    data_eu = rbind(data_eu,XX)
  }
dirflags_eu=paste(dirflags,"EU/",sep="")
checkerplot(data_eu, cols=8, rows=7, geom="line", img=dirflags_eu,
            title = "Unemployment rate in Europe 1997 to 2008 (in perc)", title.size=18,
            ylabels=c(5,10,15,20),
			ybreaks=c(5,10,15,20), xbreaks=c(1998,2001,2004,2007), xlabels=c("98","01","04","07"),
    		margin_yaxis=-0.05, margin_yaxis2=-0.2,
    		margin_xaxis=-0.4, margin_xaxis2=0 )

### EXAMPLE for US data
  data(coordUS)
  ## rearrange states which are far away:
  coordUS[coordUS$state=="AK","x"] <- 0.4  ## rearrange Alaska
  coordUS[coordUS$state=="HI","x"] <- 0.5       ## rearrange Hawaii
  ## optimal arrangement of the states:
  order <- optimal_grid_allocation(coordUS[,1:2],13,6) 
  order <- data.frame(names=coordUS$state,order=order) 
  ## load US healthy insurance data
  data(USdata1)
  data(USdata2)
  nam <- names(USdata1)
  ## delete previous order (optional)
  USdata1 <- USdata1[,-which(names(USdata1)=="order")]
  ## combine data and ordering:
  USdata1 <- merge(USdata1,order,all.x=TRUE,all.y=FALSE)
#  USdata1[is.na(USdata1$order),"order"] <- 14
  USdata1 <- USdata1[,nam] 
  ## define directory with flags (pnm's):
  dirflags_us=paste(dirflags,"USStates/",sep="")
  USdata2[,3] <- USdata2[,3]*100
  checkerplot(USdata2[,-2], cols=13, rows=6, geom="bar", 
            title="US private health insurance (in percent of the population)",
            title.size=18, ylabels=c(30,60,90), ybreaks=c(30,60,90), 
            img=dirflags_us, margin_yaxis=-0.05, margin_yaxis2=-0.2,
    		margin_xaxis=-0.4, margin_xaxis2=0) 
}
