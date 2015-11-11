#Author: Karin Fuerst @ VUT (karin.fuerst@gmx.at)
################################################# 
#  data         dataframe (defined order: "x", "y", "label", "order") 
#  cols         number of cols in the grid, preset: 5
#  rows         number of rows in the grid, preset: 5
#  geom         geometrical representation, "bar", "line" and "point" currently available, preset: line
#  title        title of the plot (printed above), preset: NULL
#  title.size   font size, preset: 20
#  label.size   font size of the labels, preset: 11
#  ymin         minimum value of y-axis displayed, if not provided it will be automatically calculated, preset: NULL
#  ymax         maximum value of x-axis displayed, if not provided it will be automatically calculated, preset: NULL
#  img          vector containing all names (must equal the label column in the dataframe data)
#               of pictures displayed beside the label, preset: NULL  

#removed this item because it is no longer supported by scale_x_continuous
#  formatter    allows to format the y-axis (scale_x_continuous(formatter = formatter))

#  margin_yaxis allows to adjust the distance from the plot to the left border of the grid
#               for all elements with an y-axis, because the difference number formats on the 
#               y-axis might lead to a little displaced plot area 
#  aes_geom     aestetics ggplot2 object for plot => change appearance
#  opts         opts ggplot2 object for plot (e.g. axis.text, plot.margin, ...)
checkerplot <- function(data, cols=5, rows=5, geom="line", errorbar=FALSE, title=NULL, title.size=20, label.size=11, 
		                xbreaks=NULL, xlabels=NULL, ybreaks=NULL, ylabels=NULL,
		                ymin=NULL, ymax=NULL, img=NULL, aes_geom=NULL, #formatter=NULL, 
						margin_yaxis=0,margin_yaxis2=0,margin_xaxis=0,margin_xaxis2=0, opts=NULL, ...){
  x<-y<-NULL
  
  
  grid.newpage() 
  if(geom=="line" && errorbar) stop("errorbars are not supported with geom line")
  if(length(xbreaks) != length(xlabels)) stop("length of breaks and labels for the x-axis differs.")
  if(length(ybreaks) != length(ylabels)) stop("length of breaks and labels for the y-axis differs.")
  #defines the basis structure of the grid 
  margin <- unit(1, "lines") 
  pushViewport(viewport(x=margin, 
                         y=margin, 
                         width=unit(1, "npc") - 2*margin, 
                         height=unit(1, "npc") - 2*margin, 
                         just=c("left", "bottom"))) 
  
  
  #manually defines the width of the y-axis, depending on the number 
  #of columns wanted to be displayed => maybe a better way??? 
  if(cols<=3){
    width_yaxis = 0.15
  }else if(cols>3 && cols<=6){
    width_yaxis = 0.30
  }else{
    width_yaxis = 0.40
  }

  #defines the widths and heights of each basis column/line in the grid 
  widths = unit(c(width_yaxis,rep(c(0.5, 0.5, 0.01), cols)), c("null",rep(c("null", "null", "mm"), cols)))
  
  #heights depending on wether a title is provided or not 
  if(!is.null(title)){
    heights = unit(c(0.3,0.01,rep(c(1,0.7, 0.01), rows),0.1), c("null","mm",rep(c("lines", "null", "mm"),rows),"null"))  
  }else{
    heights = unit(c(rep(c(1,0.7, 0.01), rows),0.1), c(rep(c("lines", "null", "mm"),rows),"null"))
  }
  
  #calculate the number of rows and cols used in the grid to display the checkerplot 
  ncols = cols * 2 + (cols-1) + 1 
  
  #two more rows in the grid are necessary if a title is provided 
  if(!is.null(title)){
    nrows = rows * 2 + (rows-1) + 2 + 2 
  }else{
    nrows = rows * 2 + (rows-1) + 2 
  }
  
  #if the minimum value of the y-axis is not provided, the value will be determined 
  #as the minimum of all given values + a puffer (10% if not otherwise stated)
  if(is.null(ymin)){
    yLim_min = getYLim_MIN(data[,2]) #get minimum y (+puffer) to equally scale all y-axis 
  }else{
    yLim_min = ymin
  }
    
  #if the maximum value of the x-axis is not provided, the value will be determined 
  #as the maximum of all given values + a puffer (10% if not otherwise stated)
  if(is.null(ymax)){
    yLim_max = getYLim_MAX(data[,2]) #get maximum y (+puffer) to equally scale all y-axis
  }else{
    yLim_max = ymax
  }
  
  colnames(data) = c("x", "y", "label", "order") 
  
  #x-axis - determine the labels for the x-axis 
  #assumption - all entities in the data are well defined 
  #randomly choose one entity 
  #??? any better way 
  #??? maybe let the user provide the labels for the x-axis
  if(is.null(xbreaks)){
	  sample_ent <- sample(1:max(data$order), 1, replace=F)
	  xbreaks = data[which(data$order==sample_ent),1]
	  while(length(xbreaks)==0){
	    sample_ent <- sample(1:max(data$order), 1, replace=F)
	    xbreaks = data[which(data$order==sample_ent),1]
	    xbreaks = unique(xbreaks)
	  }
      n_xbreaks = length(data[data$order==sample_ent, 1])        #number of x-axis values
  } else {
	  n_xbreaks <- length(xbreaks)
  }
  #--**--**--**
  pushViewport(viewport(layout=grid.layout(nrows, ncols, widths=widths, heights=heights, respect=TRUE)))

  #display title if one is provided 
  if(!is.null(title)){
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2:(ncols)))
    grid.text(title, gp=gpar(fontsize=title.size), just="center")
    popViewport()
  }
  
  if(!is.null(title)){
    start.row = 3
  }else{
    start.row = 1
  }
    
  ind = 1  #  object (order) - order used for displaying the entities in the plot
  #for(j in start.row:(nrows-1)){ # iterate cols 
  for(j in (nrows-1):start.row){
    for(i in 1:ncols){ # iterate rows
      if(((i%%3==2 && j%%3==0 && !is.null(title)) ||  
        (i%%3==2 && j%%3==1 && is.null(title))) && j<nrows){ #rubric head
        z = i + 1
        label = unique(data[data$order==ind,3])
        pushViewport(viewport(layout.pos.row=j, layout.pos.col=i:z))
        grid.rect(gp=gpar(fill="lightgrey"))
  
        if(!is.null(img)){ #if picture available => select left viewport for label 
          popViewport()
          pushViewport(viewport(layout.pos.row=j, layout.pos.col=i))  
        }
   
        if(length(label) > 0){
          grid.text(label, gp=gpar(fontsize=label.size))
        }
        popViewport()
          
        if(!is.null(img) && length(label)>0){
          pushViewport(viewport(layout.pos.row=j, layout.pos.col=z)) 
          #! notice ".png.pnm" only necessary because I was not able to remove the .png 
          #! after the convertation process ...  
          #s = paste(img, label, ".png.pnm", sep="") 
          s = paste(img, label, ".pnm", sep="")
          p = read.pnm(s) 
          p1 <- pixmapGrob(p) 
          grid.draw(p1)
          popViewport()
        }
        
        ind = ind + 1
      }

      if((i%%3==2 && j%%3==1 && !is.null(title)) || 
         (i%%3==2 && j%%3==2 && is.null(title))){ #plot
       z = i + 1    

        pushViewport(viewport(layout.pos.row=j, layout.pos.col=i:z))
        #-*-*-*
        #----------------------------------------------------------------------------------
        #Line 
        #----------------------------------------------------------------------------------
        if(geom=="line"){
          #generate ggplot2 object and provide data and define generell aesthetics 
          label = unique(data[data$order==ind,3])
        
          if(length(label)>0){
             p = ggplot(data[data$order==ind,], aes(x=x, y=y, group=label))
          }else{
            y_blubb = rep(0, length(unique(xbreaks)))
            df_blubb = as.data.frame(cbind(unique(xbreaks), y_blubb))
            names(df_blubb) <- c("x","y")
            #p = ggplot(df_blubb, aes(x=x, y=y, group=label))
            p = ggplot(df_blubb, aes(x=x, y=y))
          }
        
          p = p + geom_line(aes_geom)  


          #------------------------------------------------------------------------------ 
          #manually define how many values are displayed as x-axis scale labels 
          #to avoid overwriting of x-axis scale labels 
          #??better way - automatically the ggplot2 function is not able to handle this 
          #------------------------------------------------------------------------------
		if(is.null(xbreaks)){
          if(n_xbreaks > 4 && cols<4){ #just display every third value of x-axis
			  xlabels = xbreaks
             x_i = seq(2, n_xbreaks-1, 2) 
             xbreaks_cut = xbreaks[x_i] 
			 xlabels <- xbreaks_cut
          }else if(n_xbreaks > 4 && cols>=4){ #just display every fifth value of x-axis
             xbreaks_cut = xbreaks
             x_i = seq(2, n_xbreaks-1, 4) 
             xbreaks_cut = xbreaks[x_i]
			 xlabels <- xbreaks_cut
          }    
	  } else {
		  xbreaks_cut <- xbreaks
	  }
        
          if(n_xbreaks > 4 && cols<4){ #just display every third value of x-axis
            p = p + scale_x_continuous(breaks=xbreaks_cut, labels=xlabels,minor_breaks=c(),limits=c(min(data$x,na.rm=TRUE),max(data$x,na.rm=TRUE)))
          }else if(n_xbreaks > 4 && cols>=4){ #just display every fifth value of x-axis
            p = p + scale_x_continuous(breaks=xbreaks_cut, labels=xlabels,limits=c(min(data$x,na.rm=TRUE),max(data$x,na.rm=TRUE)))   
#			p = p + scale_x_continuous(breaks=c(1996,2002,2008), labels=c("96","02","08"))   
          }else{
			  if(length(data[data$order==ind,]$y) > 1){
			  	p = p + scale_x_continuous(breaks=xbreaks_cut, labels=xlabels,limits=c(min(data$x,na.rm=TRUE),max(data$x,na.rm=TRUE))) 
		  	  } else{
				  u <- unique(sort(data[,1]))
				  w <- (u %in% xbreaks_cut )
				  u2 <- as.character(u)
				  u2[!w] <- ""
                  u2[w] <- xlabels
          p = p + scale_x_continuous(breaks=xbreaks_cut, labels=xlabels,limits=c(min(data$x,na.rm=TRUE),max(data$x,na.rm=TRUE)))
				  #p = p + scale_x_continuous(breaks=u, labels=u2, formatter=formatter,limits=c(min(data$x,na.rm=TRUE),max(data$x,na.rm=TRUE)))  
			  }
#			  p = p + scale_x_continuous("")
  
          }

          # make sure, that all y-axis are equally scaled 
          #if(!is.null(formatter)){
          #  p = p + scale_y_continuous(limits=c(yLim_min,yLim_max), breaks=ybreaks,labels=ylabels, formatter=formatter)
          #}else{
            p = p + scale_y_continuous(limits=c(yLim_min,yLim_max),breaks=ybreaks,labels=ylabels)
          #}
        }
        #----------------------------------------------------------------------------------
        #barchart or point 
        #----------------------------------------------------------------------------------
        if(geom=="bar" || geom=="point"){
          data[is.na(data$y), 2] = 0 
        
          label = unique(data[data$order==ind,3])
          
          if(length(label)>0){
            p = ggplot(aes(factor(x),y=y, fill=factor(x)), data=data[data$order==ind,])
          }else{
            #empty field => generate dummy data 
            #                    => axis labels can be displayed
            #                    => looks similar to all other fields (which have data)
            y_blubb = rep(0, length(xbreaks))
            df_blubb = as.data.frame(cbind(xbreaks, y_blubb))
            names(df_blubb) = c("x", "y")
            p = ggplot(aes(factor(x),y=y, fill=factor(x)), data=df_blubb)
          }
          
          if(!is.null(aes_geom)){
            p = p + aes_geom 
          }
          
          yLim_min = 0 
          p = p + ylim(yLim_min, yLim_max) 
          
          p = p + theme(legend.position = "none")
          if(geom=="bar"){
			  if(errorbar) limits <- aes(ymax = y + y*0.1, ymin=y - y*0.1)    # added
            p = p + geom_bar(stat="identity", position="dodge") 
			if(errorbar) dodge <- position_dodge(width=0.4)  # added
			if(length(data[data$order==ind,]$y) > 1 && errorbar ){
				p = p + geom_errorbar(limits, position=dodge, width=0.2)  # added
			}
          }else{ #=> geom=="point"
			  if(errorbar) limits <- aes(ymax = y + y*0.2, ymin=y - y*0.2)   # added
            p = p + geom_point(stat="identity", position="dodge")
			if(errorbar) dodge <- position_dodge(width=0.4) # added
			if(length(data[data$order==ind,]$y) > 1 && errorbar){
				p = p + geom_errorbar(limits, position=dodge, width=0.1)  # added
			}
			
			
          }
          #------------------------------------------------------------------------------ 
          #manually define how many values are displayed as x-axis scale labels 
          #to avoid overwriting of x-axis scale labels 
          #??better way - automatically the ggplot2 function is not able to handle this 
          #------------------------------------------------------------------------------
 		 if(is.null(xbreaks)){
		  if(n_xbreaks > 4 && cols<4){ #just display every third value of x-axis
             xbreaks_cut = xbreaks
             x_i = seq(2, n_xbreaks-1, 2) 
             xbreaks_cut = xbreaks[x_i] 
          }else if(n_xbreaks > 4 && cols>=4){ #just display every fifth value of x-axis
             xbreaks_cut = xbreaks
             x_i = seq(2, n_xbreaks-1, 4) 
             xbreaks_cut = xbreaks[x_i]
          }
	  	} else {
			xbreaks_cut=xbreaks
		}
                     
          if(n_xbreaks > 4 && cols<4){ #just display every third value of x-axis
            p = p + scale_x_discrete(breaks=xbreaks_cut, labels=xbreaks_cut)
          }else if(n_xbreaks > 4 && cols>=4){ #just display every fifth value of x-axis
            p = p + scale_x_discrete(breaks=xbreaks_cut, labels=xbreaks_cut)              
          }else{
            p = p + scale_x_discrete("")
          }
        }
        
        # make sure, that all y-axis are equally scaled 
        #if(!is.null(formatter)){
        #    cat("1\n")
        #    p = p + scale_y_continuous(limits=c(yLim_min,yLim_max), breaks=ybreaks,labels=ylabels, formatter=formatter)
        #    cat("2\n")
        #  }else{
        #    cat("3\n")
        #    p = p + scale_y_continuous(limits=c(yLim_min,yLim_max), breaks=ybreaks,labels=ylabels)
        #    cat("4\n")
        #}
        
        #------------------------------------------------------------------------------
        #in the following code segment the four different plot representations are defined
        #
        #1. plot x-axis labels and y-axis label (lowermost left plot-viewport) 
        #2. plot y-axis (all left plot-viewports, expect the lowermost) 
        #3. plot x-axis (all lowermost plot-viewports, expect the first left)
        #4. plot no axis (all plot-viewports (except lowermost and left plot-viewports) 
        #
        #in order to display the x/y-axis not in the plot-viewport itself the 
        #plot.margin property of the opts-function for the ggplot2 object is used 
        #------------------------------------------------------------------------------
        if(i==2 && j==(nrows-2)){ #plot y-axis labels and x-axis 
          p = p + theme(plot.margin = unit(c(0, 0, -1.8+margin_xaxis, (-2.2+margin_yaxis)), "lines"))
        }else if(i==2){ #plot y-axis
          p = p + theme(axis.text.x = element_blank())
          p = p + theme(plot.margin = unit(c(0, 0, -1.5+margin_xaxis2, (-2.2+margin_yaxis)), "lines")) 
        }else if(j==(nrows-2)){ #plot x-axis
          p = p + theme(axis.text.y = element_blank())
          p = p + theme(plot.margin = unit(c(0, 0, -1.8+margin_xaxis, -1.3+margin_yaxis2), "lines"))             
        }else { # plot no axis
          p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
          p = p + theme(plot.margin = unit(c(0, 0, -1.5+margin_xaxis2, -1.3+margin_yaxis2), "lines"))
        }
              
        # no x/y-axis labels 
        p = p + xlab("") + ylab("") +theme(legend.position = "none")
              
        #add user defined opts to ggplot2 object 
        if(!is.null(opts)){
          p = p + opts
        }
        
        if(geom=="line" || geom=="bar" || geom=="point"){
          p1 = ggplotGrob(p)
          grid.draw(p1)
          grid.lines(x=unit(c(0,1),"npc"), y=unit(c(0,0),"npc"))
          grid.lines(x=unit(c(0,1),"npc"), y=unit(c(1,1),"npc"))
          grid.lines(x=unit(c(0,0),"npc"), y=unit(c(0,1),"npc"))
          grid.lines(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"))
          popViewport()
          
          if(ind%%cols==0){
            ind = ind - cols
          }
          ind = ind + 1
        }
      }
    }
  }
}


#Funktion liefert von einem Vektor das Maximum + einem Puffer 
getYLim_MAX <- function(y, puffer=0.1){
   max_y = max(y, na.rm=TRUE) 
   max_y = max_y + max_y*puffer
   return(max_y) 
}

#Funktion liefert von einem Vektor das Minimum + einem Puffer
getYLim_MIN <- function(y, puffer=0.1){
   min_y = min(y, na.rm=TRUE) 
   min_y = min_y*(1-puffer)
   return(min_y) 
}
        
    