require(shinyGridster)

load("data.rdata")

# manage columns of sparkTable obj
manage.cols <- function(i, inp, varType, input) {
	if ( is.null(input) ) {
		return(inp)
	}
	v <- eval(parse(text=paste("input$col",i,sep="")))	
	if ( !is.null(v) ) {
		if ( v == "line" ) {
			inp[[i]] <- newSparkLine()
			
			# point width
			v.pw <- eval(parse(text=paste("input$pwslider",i,sep="")))	
			if ( !is.null(v.pw) ) {
				pointWidth(inp[[i]]) <- v.pw
			}		
			
		}
		if ( v == "box" ) {
			inp[[i]] <- newSparkBox()
		}		
		if ( v == "hist" ) {
			inp[[i]] <- newSparkHist()
		}	
		if ( v == "bar" ) {
			inp[[i]] <- newSparkBar()
		}			
		if ( v == "func" ) {		
			fnval <- eval(parse(text=paste("input$fn",i,sep="")))	
			if ( is.null(fnval) ) {
				fnval <- "function(x) { x }"
			}			
			fn <- eval(parse(text=fnval))
			inp[[i]] <- fn
		}							
	} 
	
	v <- eval(parse(text=paste("input$colname",i,sep="")))	
	if ( !is.null(v) ) {
		cat("something has changed!\n"); flush.console()
		names(inp)[i] <- v
	}
	return(inp)
}

manage.vars <- function(i, varType, input) {
	if ( is.null(input) ) {
		return(varType)
	}	
	v <- eval(parse(text=paste("input$varType",i,sep="")))	
	if ( !is.null(v) ) {
		varType[i] <- v	
	}
	return(varType)	
}

actionButton <- function (inputId, label, style=NULL) {
  tags$button(id = inputId, type="button", class=paste("btn action-button", style), label)
}
