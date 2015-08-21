checkColors <- function(color) {
	recodeColors <- function(color) {
		c <- col2rgb(color)
		sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
	}
	res <- NULL
	out <- try(lapply(color, recodeColors), silent=TRUE)
	if ( class(out) != "try-error" )
		res <- out
	res
}

banking <- function (diff.x, diff.y)  {
	r <- 1
	if ( !is.vector(diff.x) | !is.vector(diff.y) )
		stop("vectors required as input objects\n")
	if ( length(diff.x) != length(diff.y) )
		stop("Non matching lengths")
	ids <- diff.x != 0 & diff.y != 0 & !is.na(diff.x) & !is.na(diff.y)
	if ( any(ids) )
		r <- median(abs(diff.x[ids]/diff.y[ids]))
	r
}

##Original from package xtable, Author: David Dahl dahl@stat.tamu.edu
### Only a minor adaption, namely added parameters:
#wider.columns=NULL,column.width=NULL,
#skip.columns=NULL,right.border=NULL,left.border=NULL,
### especially for use with \multicolumn and the package sparkTable
### Alexander Kowarik <alexander.kowarik@statistik.gv.at>

print.xtable2 <- function(
		x,
		type="latex",
		file="",
		append=FALSE,
		floating=TRUE,
		floating.environment="table",
		table.placement="ht",
		caption.placement="bottom",
		latex.environments=c("center"),
		tabular.environment="tabular",
		size=NULL,
		hline.after=NULL,
		NA.string="",
		include.rownames=TRUE,
		include.colnames=TRUE,
		only.contents=FALSE,
		add.to.row=NULL,
		sanitize.text.function=NULL,
		sanitize.rownames.function=sanitize.text.function,
		sanitize.colnames.function=sanitize.text.function,
		math.style.negative=FALSE,
		html.table.attributes="",
		wider.columns=NULL,column.width=NULL,
		skip.columns=NULL,transpose=FALSE,rownames=NULL,colnames=NULL,comment=FALSE,
		...) {
	pos <- 0

	if (any(hline.after < -1) | any(hline.after > nrow(x))) stop("'hline.after' must be inside [-1, nrow(x)]")

	if (!is.null(add.to.row)) {
		if (is.list(add.to.row) && length(add.to.row)==2) {
			if (is.null(names(add.to.row))) {
				names(add.to.row) <- c('pos', 'command')
			} else if (any(sort(names(add.to.row))!=c('command', 'pos'))) {
				stop("the names of the elements of 'add.to.row' must be 'pos' and 'command'")
			}
			if (is.list(add.to.row$pos) && is.vector(add.to.row$command, mode='character')) {
				if ((npos <- length(add.to.row$pos)) != length(add.to.row$command)) {
					stop("the length of 'add.to.row$pos' must be equal to the length of 'add.to.row$command'")
				}
				if (any(unlist(add.to.row$pos) < -1) | any(unlist(add.to.row$pos) > nrow(x))) {
					stop("the values in add.to.row$pos must be inside the interval [-1, nrow(x)]")
				}
			} else {
				stop("the first argument ('pos') of 'add.to.row' must be a list, the second argument ('command') must be a vector of mode character")
			}
		} else {
			stop("'add.to.row' argument must be a list of length 2")
		}
	} else {
		add.to.row <- list(pos=list(), command=vector(length=0, mode="character"))
		npos <- 0
	}

	# Add further commands at the end of rows
	if (type=="latex") {
		PHEADER <- "\\hline\n"
	} else {
		PHEADER <- ""
	}

	lastcol <- rep(" ", nrow(x)+2)
	if (!is.null(hline.after)) {
		add.to.row$pos[[npos+1]] <- hline.after
		add.to.row$command <- c(add.to.row$command, PHEADER)
	}
	if ( length(add.to.row$command) > 0 ) {
		for (i in 1:length(add.to.row$command)) {
			addpos <- add.to.row$pos[[i]]
			freq <- table(addpos)
			addpos <- unique(addpos)
			for (j in 1:length(addpos)) {
				lastcol[addpos[j]+2] <- paste(lastcol[addpos[j]+2], paste(rep(add.to.row$command[i], freq[j]), sep="", collapse=""), sep=" ")
			}
		}
	}

	if (length(type)>1) stop("\"type\" must have length 1")
	type <- tolower(type)
	if (!all(!is.na(match(type,c("latex","html"))))) stop("\"type\" must be in {\"latex\", \"html\"}")
	if (!all(!is.na(match(floating.environment,c("table","sidewaystable"))))) stop("\"type\" must be in {\"table\", \"sidewaystable\"}")
	if (!all(!is.na(match(unlist(strsplit(table.placement, split="")),c("H","h","t","b","p","!"))))) {
		stop("\"table.placement\" must contain only elements of {\"h\",\"t\",\"b\",\"p\",\"!\"}")
	}
	if (!all(!is.na(match(caption.placement,c("bottom","top"))))) stop("\"caption.placement\" must be either {\"bottom\",\"top\"}")

	if (type=="latex") {
		BCOMMENT <- "% "
		ECOMMENT <- "\n"
		if ( tabular.environment == "longtable" & floating == TRUE ) {
			warning("Attempt to use \"longtable\" with floating=TRUE. Changing to FALSE.")
			floating <- FALSE
		}
		if ( floating == TRUE ) {
			BTABLE <- paste("\\begin{", floating.environment, "}",ifelse(!is.null(table.placement),
							paste("[",table.placement,"]",sep=""),""),"\n",sep="")
			if ( is.null(latex.environments) || (length(latex.environments)==0) ) {
				BENVIRONMENT <- ""
				EENVIRONMENT <- ""
			}
			else {
				BENVIRONMENT <- ""
				EENVIRONMENT <- ""
				for ( i in 1:length(latex.environments) ) {
					if ( latex.environments[i] == "" ) next
					BENVIRONMENT <- paste(BENVIRONMENT, "\\begin{",latex.environments[i],"}\n",sep="")
					EENVIRONMENT <- paste("\\end{",latex.environments[i],"}\n",EENVIRONMENT,sep="")
				}
			}
			ETABLE <- paste("\\end{", floating.environment, "}\n", sep="")
		}
		else {
			BTABLE <- ""
			ETABLE <- ""
			BENVIRONMENT <- ""
			EENVIRONMENT <- ""
		}

		tmp.index.start <- 1
		while ( attr(x,"align",exact=TRUE)[tmp.index.start] == '|' ) tmp.index.start <- tmp.index.start + 1
		tmp.index.start <- tmp.index.start + 1
		BTABULAR <- paste("\\begin{",tabular.environment,"}{",
				paste(c(attr(x, "align",exact=TRUE)[tmp.index.start:length(attr(x,"align",exact=TRUE))], "}\n"),
						sep="", collapse=""),
				sep="")

		if (tabular.environment == "longtable" && caption.placement=="top") {
			BCAPTION <- "\\caption{"
			ECAPTION <- "} \\\\ \n"
			if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex")) BTABULAR <- paste(BTABULAR,  BCAPTION, attr(x,"caption",exact=TRUE), ECAPTION, sep="")
		}
		BTABULAR <- paste(BTABULAR,lastcol[1], sep="")
		ETABULAR <- paste("\\end{",tabular.environment,"}\n",sep="")

		if (is.null(size) || !is.character(size)) {
			BSIZE <- ""
			ESIZE <- ""
		} else {
			if(length(grep("^\\\\",size))==0){
				size <- paste("\\",size,sep="")
			}
			BSIZE <- paste("{",size,"\n",sep="")
			ESIZE <- "}\n"
		}
		BLABEL <- "\\label{"
		ELABEL <- "}\n"
		BCAPTION <- "\\caption{"
		ECAPTION <- "}\n"
		BROW <- ""
		EROW <- " \\\\ \n"
		BTH <- ""
		ETH <- ""
		STH <- " & "
		BTD1 <- " & "
		BTD2 <- ""
		BTD3 <- ""
		ETD  <- ""
		sanitize <- function(str) {
			result <- str
			result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
			result <- gsub("$","\\$",result,fixed=TRUE)
			result <- gsub(">","$>$",result,fixed=TRUE)
			result <- gsub("<","$<$",result,fixed=TRUE)
			result <- gsub("|","$|$",result,fixed=TRUE)
			result <- gsub("{","\\{",result,fixed=TRUE)
			result <- gsub("}","\\}",result,fixed=TRUE)
			result <- gsub("%","\\%",result,fixed=TRUE)
			result <- gsub("&","\\&",result,fixed=TRUE)
			result <- gsub("_","\\_",result,fixed=TRUE)
			result <- gsub("#","\\#",result,fixed=TRUE)
			result <- gsub("^","\\verb|^|",result,fixed=TRUE)
			result <- gsub("~","\\~{}",result,fixed=TRUE)
			result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
			return(result)
		}
		sanitize.numbers <- function(x) {
			result <- x
			if ( math.style.negative ) {
				for(i in 1:length(x)) {
					result[i] <- gsub("-","$-$",result[i],fixed=TRUE)
				}
			}
			return(result)
		}
		sanitize.final <- function(result) {
			return(result)
		}
	} else {
		BCOMMENT <- "<!-- "
		ECOMMENT <- " -->\n"
		BTABLE <- paste("<table ",html.table.attributes,">\n",sep="")
		ETABLE <- "</table>\n"
		BENVIRONMENT <- ""
		EENVIRONMENT <- ""
		BTABULAR <- ""
		ETABULAR <- ""
		BSIZE <- ""
		ESIZE <- ""
		BLABEL <- "<a name="
		ELABEL <- "></a>\n"
		BCAPTION <- paste("<caption align=\"",caption.placement,"\"> ",sep="")
		ECAPTION <- " </caption>\n"
		BROW <- "<tr>"
		EROW <- " </tr>\n"
		BTH <- " <th> "
		ETH <- " </th> "
		STH <- " </th> <th> "
		BTD1 <- " <td align=\""
		align.tmp <- attr(x,"align",exact=TRUE)
		align.tmp <- align.tmp[align.tmp!="|"]
		BTD2 <- matrix(align.tmp[(2-pos):(ncol(x)+1)],nrow=nrow(x),ncol=ncol(x)+pos,byrow=TRUE)
		BTD2[regexpr("^p",BTD2)>0] <- "left"
		BTD2[BTD2=="r"] <- "right"
		BTD2[BTD2=="|r"] <- "right"
		BTD2[BTD2=="r|"] <- "right"
		BTD2[BTD2=="|r|"] <- "right"
		BTD2[BTD2=="l"] <- "left"
		BTD2[BTD2=="l|"] <- "left"
		BTD2[BTD2=="|l"] <- "left"
		BTD2[BTD2=="|l|"] <- "left"
		BTD2[BTD2=="c"] <- "center"
		BTD2[BTD2=="|c"] <- "center"
		BTD2[BTD2=="c|"] <- "center"
		BTD2[BTD2=="|c|"] <- "center"
		BTD3 <- "\"> "
		ETD  <- " </td>"
		sanitize <- function(str) {
			result <- str
			result <- gsub("&","&amp ",result,fixed=TRUE)
			result <- gsub(">","&gt ",result,fixed=TRUE)
			result <- gsub("<","&lt ",result,fixed=TRUE)
			# Kurt Hornik <Kurt.Hornik@wu-wien.ac.at> on 2006/10/05 recommended not escaping underscores.
			# result <- gsub("_", "\\_", result, fixed=TRUE)
			return(result)
		}
		sanitize.numbers <- function(x) {
			return(x)
		}
		sanitize.final <- function(result) {
			result$text <- gsub("  *"," ", result$text,fixed=TRUE)
			result$text <- gsub(' align="left"', "", result$text,fixed=TRUE)
			return(result)
		}
	}

	result <- string("",file=file,append=append)
	info <- R.Version()
  if(comment){
	  result <- result + BCOMMENT + type + " table generated in " +
			info$language + " " + info$major + "." + info$minor + " by xtable " + packageDescription('xtable')$Version + " package" + ECOMMENT
	  result <- result + BCOMMENT + date() + ECOMMENT
  }
	if (!only.contents) {
		result <- result + BTABLE
		result <- result + BENVIRONMENT
		if ( floating == TRUE ) {
			if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="html" || caption.placement=="top")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
			if (!is.null(attr(x,"label",exact=TRUE)) && (type=="latex" && caption.placement=="top")) result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL
		}
		result <- result + BSIZE
		result <- result + BTABULAR
	}

	cols <- matrix("",nrow=nrow(x),ncol=ncol(x)+pos)

	disp <- function(y) {
		if (is.factor(y)) {
			y <- levels(y)[y]
		}
		if (is.list(y)) {
			y <- unlist(y)
		}
		return(y)
	}

	if( !is.matrix( attr( x, "digits",exact=TRUE ) ) ) {
		attr(x,"digits") <- matrix( attr( x, "digits",exact=TRUE ), nrow = nrow(x), ncol = ncol(x)+1, byrow = TRUE )
	}
	for(i in 1:ncol(x)) {
		ina <- is.na(x[,i])
		is.numeric.column <- is.numeric(x[,i])
		for( j in 1:nrow( cols ) ) {

			cols[j,i+pos] <-
					formatC( disp( x[j,i] ),
							format = ifelse( attr( x, "digits",exact=TRUE )[j,i+1] < 0, "E", attr( x, "display",exact=TRUE )[i+1] ), digits = abs( attr( x, "digits",exact=TRUE )[j,i+1] ), decimal.mark=options()$OutDec)
		}
		if ( any(ina) ) cols[ina,i+pos] <- NA.string
		if ( is.numeric.column ) {
			cols[,i+pos] <- sanitize.numbers(cols[,i+pos])
		} else {
			if (is.null(sanitize.text.function)) {
				cols[,i+pos] <- sanitize(cols[,i+pos])
			} else {
				cols[,i+pos] <- sanitize.text.function(cols[,i+pos])
			}
		}
	}

	cols[cols=="<h2></h2>"] <- ""
	multiplier <- 5
	full <- matrix("",nrow=nrow(x),ncol=multiplier*(ncol(x)+pos)+2)
	full[,1] <- BROW
	full[,multiplier*(0:(ncol(x)+pos-1))+2] <- BTD1
	full[,multiplier*(0:(ncol(x)+pos-1))+3] <- BTD2
	full[,multiplier*(0:(ncol(x)+pos-1))+4] <- BTD3
	full[,multiplier*(0:(ncol(x)+pos-1))+5] <- cols
	full[,multiplier*(0:(ncol(x)+pos-1))+6] <- ETD

	if(!is.null(wider.columns)&&type!="latex"&&column.width>1){
		colBTD3 <- multiplier*(0:(ncol(x)+pos-1))+4
		BTD3_new <- paste("\"  colspan=",column.width,">",sep="")
		for(i in 1:nrow(wider.columns)){
			full[wider.columns[i,1],colBTD3[wider.columns[i,2]]] <- BTD3_new
		}
	}

	paste2 <- function(x,word){
		for(r in 1:nrow(x)){
			for(c in 1:ncol(x)){
				x[r,c] <- paste(x[r,c],word)
			}
		}
		x
	}
	if(type!="latex"){
		if(!transpose){
			style <- matrix("",nrow=nrow(cols),ncol=ncol(cols))
			style[,unique(wider.columns[,2])] <- paste2(style[,unique(wider.columns[,2]),drop=FALSE],"left")
			style[unique(wider.columns[,1]),] <- paste2(style[unique(wider.columns[,1]),,drop=FALSE],"top")
			style[,ncol(style)] <- paste(style[,ncol(style)],"right")
			style[wider.columns] <- paste(style[wider.columns],"right")
			style[nrow(style),] <- paste(style[nrow(style),],"bottom")
			for(i in 1:nrow(cols)){
				for(j in 1:ncol(cols)){
					colBTD1 <- multiplier*(0:(ncol(x)+pos-1))+2
					s <- style[i,j]
					full[i,colBTD1[j]] <- paste(' <td class="',s,'" align="',sep="")
				}
			}
		}else{
			style <- matrix("",nrow=nrow(cols),ncol=ncol(cols))
			style[,unique(wider.columns[,2])] <- paste2(style[,unique(wider.columns[,2]),drop=FALSE],"left")
			style[unique(wider.columns[,1]),] <- paste2(style[unique(wider.columns[,1]),,drop=FALSE],"top")
			style[,ncol(style)] <- paste(style[,ncol(style)],"right")
			style[nrow(style),] <- paste(style[nrow(style),],"bottom")
			style[wider.columns] <- paste(style[wider.columns],"right")
			for(i in 1:nrow(cols)){
				for(j in 1:ncol(cols)){
					colBTD1 <- multiplier*(0:(ncol(x)+pos-1))+2
					s <- style[i,j]
					full[i,colBTD1[j]] <- paste(' <td class="',s,'" align="',sep="")
				}
			}
		}
	}
	if(!is.null(skip.columns)&&column.width>1){
		colBTD1 <- multiplier*(0:(ncol(x)+pos-1))+2
		colBTD2 <- multiplier*(0:(ncol(x)+pos-1))+3
		colBTD3 <- multiplier*(0:(ncol(x)+pos-1))+4
		colContent <- multiplier*(0:(ncol(x)+pos-1))+5
		colETD <- multiplier*(0:(ncol(x)+pos-1))+ 6
		for(i in 1:nrow(skip.columns)){
			full[skip.columns[i,1],colBTD1[skip.columns[i,2]]] <- ""
			full[skip.columns[i,1],colBTD2[skip.columns[i,2]]] <- ""
			full[skip.columns[i,1],colBTD3[skip.columns[i,2]]] <- ""
			full[skip.columns[i,1],colETD[skip.columns[i,2]]] <- ""
			full[skip.columns[i,1],colContent[skip.columns[i,2]]] <- ""
		}
	}
	full[,multiplier*(ncol(x)+pos)+2] <- paste(EROW, lastcol[-(1:2)], sep=" ")
	if(include.rownames&type!="latex"){
		full2 <- matrix(NA,ncol=ncol(full)+1,nrow=nrow(full))
		full2[,-2] <- full
		full2[,2] <- "<td></td>"
		full2[-unique(wider.columns[,1]),2] <-
				paste("<td><h3>",rep(rownames,length(full2[-unique(wider.columns[,1]),2])/length(rownames)),"</h3></td>",sep="")
		full <- full2
	}
	if(include.colnames&&type!="latex"){
		full2 <- matrix(NA,ncol=ncol(full),nrow=nrow(full)+1)
		full2[-1,] <- full
		full2[1,] <- full2[3,]
		rowAdds <- 0
		if(include.rownames){
			rowAdds <- 1
			full2[1,2] <- "<td></td>"
		}
		full2[1,colBTD1+rowAdds] <- "<td align=\""
		full2[1,colContent+rowAdds] <- paste("<h3>",rep(colnames,length(full2[1,colContent+rowAdds])/length(colnames)),"</h3>",sep="")
		full <- full2
	}
	if (type=="latex")
		full[,2] <- ""
	result <- result + lastcol[2] + paste(t(full),collapse="")
	if (!only.contents) {
		if (tabular.environment == "longtable") {
			result <- result + PHEADER
			if(caption.placement=="bottom"){
				if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
			}
			if (!is.null(attr(x,"label",exact=TRUE))) result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL
			ETABULAR <- "\\end{longtable}\n"
		}
		result <- result + ETABULAR
		result <- result + ESIZE
		if ( floating == TRUE ) {
			if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex" && caption.placement=="bottom")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
			if (!is.null(attr(x,"label",exact=TRUE)) && caption.placement=="bottom") result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL
		}
		result <- result + EENVIRONMENT
		result <- result + ETABLE
	}
	result <- sanitize.final(result)
	print(result)

	return(invisible(result$text))
}

"+.string" <- function(x,y) {
	x$text <- paste(x$text,as.string(y)$text,sep="")
	return(x)
}

print.string <- function(x,...) {
	cat(x$text,file=x$file,append=x$append)
	return(invisible())
}

string <- function(text,file="",append=FALSE) {
	x <- list(text=text,file=file,append=append)
	class(x) <- "string"
	return(x)
}

as.string <- function(x,file="",append=FALSE) {
	if (is.null(attr(x,"class",exact=TRUE)))
		switch(data.class(x),
				character=return(string(x,file,append)),
				numeric=return(string(as.character(x),file,append)),
				stop("Cannot coerse argument to a string"))
	if (class(x)=="string")
		return(x)
	stop("Cannot coerse argument to a string")
}

is.string <- function(x) {
	return(class(x)=="string")
}

# pixmapGrob and as.raster.pixmapRGB used to be available in gridExtra
# this is just a shameless copy to keep things working
pixmapGrob <- function (pic, x=0.5, y=0.5, scale=1, raster=FALSE, angle=0, vp=NULL, ...) {
  rast <- as.raster(pic)
  pic <- as(pic, "pixmapIndexed")
  width <- unit(scale*pic@bbox[3], "points")
  height <- unit(scale*pic@bbox[4], "points")
  Z <- pic@index[nrow(pic@index):1,,drop = FALSE]

  angle <- if(is.null(angle)) 0 else angle
  vpc <- viewport(x=x, y=y, width=width, height=height, angle=angle,
                  xscale = c(0, ncol(Z)), yscale =c(0, nrow(Z)))

  if ( raster ) {
    child <- rasterGrob(rast, vp=vpc, ...)
  } else {
    child <- imageGrob(nrow(Z), ncol(Z), cols=pic@col[Z], gp=gpar(col=pic@col[Z]), byrow=FALSE, vp=vpc, ...)
  }

  gTree(width= width[[1]], height = height[[1]], vp=vp,
        children=gList(child), childrenvp=vpc, cl="pixmap")
}

as.raster.pixmapRGB <- function(x) {
  nr <- nrow(x@red)
  r <- rgb((x@red), (x@green), (x@blue))
  dim(r) <- x@size
  r
}
