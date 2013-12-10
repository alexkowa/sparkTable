shinyServer(function(input, output) {
	inputdata <- reactiveValues()
	inputdata$dat <- dat

	## export to html-button
	observe({
		input$exporthtml
		isolate({
			cat("exporthtml button was clicked!\n")
		})
	})	
	## export to latex-button
	observe({
		input$exportlatex
		isolate({
			cat("exportlatex button was clicked!\n")
		})
	})		

	## submit button: remove columns to the sparkTable
	observe({
		input$removecols
		isolate({
			if ( length(input$deletecols) > 0 ) {
				dat <- inputdata$dat
				index <- which(names(dat@tableContent) %in% input$deletecols)
				if ( length(index) > 0 ) {
					dat@varType <- dat@varType[-index]
					dat@tableContent <- dat@tableContent[-index]	
					inputdata$dat <- dat
				}
			}					
		})
	})
	
	## actionbutton: add a new column to the sparkTable
	observe({
		input$addnewcol
		isolate({
			if ( !is.null(input$addnewcol) && input$addnewcol !=0 ) {
				dat <- inputdata$dat
				
				dat@varType <- c(dat@varType, input$newvartype)
				
				if ( input$newcoltype == "func" ) {
					xx <- eval(parse(text=input$newfn))
				}
				if ( input$newcoltype== "line" ) {
					xx <- newSparkLine()
				}
				if ( input$newcoltype== "bar" ) {
					xx <- newSparkBar()
				}	
				if ( input$newcoltype== "box" ) {
					xx <- newSparkBox()
				}		
				if ( input$newcoltype== "hist" ) {
					xx <- newSparkHist()
				}					
				dat@tableContent <- append(dat@tableContent, xx)
				names(dat@tableContent)[[length(dat@tableContent)]] <- input$newcolname
				inputdata$dat <- dat				
			}
		})
	})	
	
	## current data object
	data <- reactive({
		dat <- inputdata$dat

		inp <- dat@dataObj	
		sparkO <- dat	
	
		if ( !is.null(input$groups) ) {
			inp <- inp[inp[,1] %in% input$groups,,drop=FALSE]
		}		
		sparkO@dataObj <- inp
		
		xx <- sparkO@tableContent
		vT <- sparkO@varType
		for ( i in 1:length(xx) ) {
			xx <- manage.cols(i, xx, varType, input)
			vT <- manage.vars(i, vT, input)
		}
		sparkO@tableContent <- xx
		sparkO@varType <- vT

		list(
			dat=inp, 
			sparkO=sparkO, 
			varType=vT, 
			groups=unique(dat@dataObj[,1]), 
			vars=colnames(inp)[3:ncol(inp)],
			cnames=names(sparkO@tableContent)
		)
	})
	
	## reactive values when adding a new column to the sparkTable
	add.newtype <- reactive({
		if ( is.null(input) ) {
			return("sparkline")
		} else {
			if (is.null(input$newcoltype) ) {
				return("sparkline")
			} else {
				cl <- input$newcoltype
				if ( cl == "box" ) { sel <- "boxplot" }
				if ( cl == "line" ) { sel <- "sparkline" }
				if ( cl == "hist" ) { sel <- "histogram" }		
				if ( cl == "bar" ) { sel <- "barplot" }		
				if ( cl == "func" ) { sel <- "function" }					
				return(sel)
			}				
		}
	})
	
	add.newcolname <- reactive({
		defname <- paste("newcol",length(data()$cnames)+1,sep="")
		if ( is.null(input) ) {
			return(defname)
		} else {
			if (is.null(input$newcolname) ) {
				return(defname)
			} else {
				return(input$newcolname)
			}				
		}
	})	
	add.newvartype <- reactive({		
		if ( is.null(input) ) {
			return(data()$varType[1])
		} else {
			if (is.null(input$newcolname) ) {
				return(data()$varType[1])
			} else {
				return(input$newvartype)
			}				
		}
	})		
	
	output$cgroups <- renderUI({
		if ( is.null(input$groups) ) {
			sel <- data()$groups
		} else {
			sel <- input$groups
		}
  	checkboxGroupInput("groups", label=h3("select groups"), choices=data()$groups, selected=sel)
	})

	output$origdata = renderDataTable({
				print(str(data()$dat)); flush.console()
		data()$dat
	})

	output$modify.table = renderUI({
		html <- NULL
		cur.vals <- data()$cnames
		html <- paste(html, as.character(checkboxGroupInput("deletecols", label=h3("select columns that should be deleted"), choices=data()$cnames)))
		html <- paste(html, as.character(actionButton("removecols", "remove selected columns", style="btn-danger")))
		
		html <- paste(html, h3("add columns"))
		if ( add.newtype()=="function" ) {
			html <- paste(html, as.character(div(class="row-fluid",
				div(class = "span4", as.character(textInput("newcolname", label=h4("column name"), value=add.newcolname()))),
			 	div(class = "span4", as.character(selectInput("newcoltype", h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=add.newtype()))),
			 	div(class = "span4", as.character(selectInput("newvartype", h4("variable"),choices=data()$vars, selected=add.newvartype()))),
			 	div(class = "span4", as.character(textInput("newfn", label=h4("custom function"), value=NULL)))
			)))			 	
		} else {
			html <- paste(html, as.character(div(class="row-fluid",
				div(class = "span4", as.character(textInput("newcolname", label=h4("column name"), value=add.newcolname()))),
				div(class = "span4", as.character(selectInput("newcoltype", h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=add.newtype()))),
				div(class = "span4", as.character(selectInput("newvartype", h4("variable"),choices=data()$vars, selected=add.newvartype()))),
				div(class = "span4", p())
			)))	
		}
		html <- paste(html, as.character(actionButton("addnewcol", "add another column", style="btn-primary")))
		
		#html <- paste(html, h3("(re)order columns"))
		#html <- paste(html, p("still todo"))		
		HTML(html)
	})  

	output$opts.global = renderUI({
		xx <- data()$sparkO@tableContent
		html <- NULL
		for ( i in 1:length(xx) ) {
			cl <- class(xx[[i]])			
			if ( cl == "sparkbox" ) {
				sel <- "boxplot"
			}
			if ( cl == "sparkline" ) {
				sel <- "sparkline"
			}
			if ( cl == "sparkhist" ) {
				sel <- "histogram"
			}		
			if ( cl == "sparkbar" ) {
				sel <- "barplot"
			}		
			if ( cl == "function" ) {
				sel <- "function"
			}										
			
			# header
			html <- paste(html, as.character(div(class="row-fluid",
				div(class = "span12", style="text-align: center", h2(paste("manage column",i)))
			)))						
	
			html <- paste(html, as.character(div(class="row-fluid",
				div(class = "span12", style="text-align: center", as.character(textInput(paste("colname",i,sep=""), label=h4(paste("modify the column name for column nr",i)), value=data()$cnames[i])))
			)))		
	
			html <- paste(html,as.character(div(class="row-fluid",
				div(class = "span12", style="text-align: center", h4("set plot type and variable to be plotted"))
			)))

			if ( cl == "function" ) {
				fn <- as.character(attributes(xx[[i]])$srcref)
				html <- paste(html, as.character(div(class="row-fluid",
					div(class = "span4", as.character(selectInput(paste("col",i,sep=""), h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=sel))),
					div(class = "span4", as.character(selectInput(paste("varType",i,sep=""), h4("variable"),choices=data()$vars, selected=data()$varType[i]))),
					div(class = "span4", as.character(textInput(paste("fn",i,sep=""), label=h4("custom function"), value=fn))),       
					div(class = "span4", p()))))
			} else {
				html <- paste(html,as.character(div(class="row-fluid",
  				div(class = "span4", as.character(selectInput(paste("col",i,sep=""),h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=sel))),
					div(class = "span4", as.character(selectInput(paste("varType",i,sep=""), h4("variable"),choices=data()$vars, selected=data()$varType[i]))),
					div(class = "span4", p()),       
					div(class = "span4", p()))))						
			}
			html <- paste(html,as.character(div(class="row-fluid",
				div(class = "span12", style="text-align: center", h4("set additional options"))
			)))			
			
			# optional parameters to set
			if ( cl == "sparkbox" ) {
			}
			if ( cl == "sparkline" ) {
				html <- paste(html, 
					as.character(sliderInput(
						inputId = paste("pwslider",i,sep=""), 
						label = "Point-Width",
    				min=1, 
						max=100, 
						value=pointWidth(xx[[i]]), 
						step=1)
					)
				)
				
			}
			if ( cl == "sparkhist" ) {
			}		
			if ( cl == "sparkbar" ) {
			}		
			if ( cl == "function" ) {
			}				
		}
		HTML(html)
  })
	
	output$sparkplot = renderDataTable({
		setwd("www")
   	m <- plotSparkTable(data()$sparkO,outputType="html",filename=NULL,graphNames="out")
		setwd("../")
		cbind(rownames(m),m)
  })

	# for gridster
	output$A <- renderText("A")
	output$B <- renderText("B")
	output$C <- renderText("C")
	output$D <- renderText("D")
})
