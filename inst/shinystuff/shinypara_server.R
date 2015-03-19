#options(shiny.trace=TRUE)
shinyServer(function(input, output, session) {
  inputdata <- reactiveValues()
  inputdata$dat <- dat

  # update textfield (newcolname) if either addnewcol or removecols are pressed
  observe({
    if ( !is.null(input$addnewcol) && input$addnewcol > 0 ) {
      updateTextInput(session, inputId="newcolname", value=paste("newcol_",gsub("[.]","",runif(1)*100),sep=""))
    }
    if ( !is.null(input$removecols) && input$removecols > 0 ) {
      updateTextInput(session, inputId="newcolname", value=paste("newcol_",gsub("[.]","",runif(1)*100),sep=""))
    }
  })

  ## export to html-button
  observe({
    input$updateparameter
    isolate({
      if ( !is.null(input$updateparameter) && input$updateparameter != 0 ) {
        dat <- inputdata$dat
        sparkO <- dat

        xx <- dat@tableContent
        vT <- dat@varType
        for ( i in 1:length(xx) ) {
          xx <- manage.cols(i, xx, input)
          vT <- manage.vars(i, vT, input)
        }
        sparkO@tableContent <- xx
        sparkO@varType <- vT
        inputdata$dat <- sparkO
      }
    })
  })

  ## export to html-button
  observe({
    input$exporthtml
    isolate({
      if ( input$exporthtml != 0 ) {
        setwd(outputDir)
        fn <- paste("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"), sep="")
        m <- export(data()$sparkO,outputType="html",filename=fn,graphNames="spark")
        cat("--> exported to",outputDir,"\n")
        flush.console()
        setwd(tempdir)
      }
    })
  })

  ## export to latex-button
  observe({
    input$exportlatex
    isolate({
      if ( input$exportlatex != 0 ) {
        setwd(outputDir)
        fn <- paste("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"), sep="")
        m <- export(data()$sparkO,outputType="tex",filename=fn,graphNames="spark")

        if ( Sys.which("pdflatex") != "" ) {
          texi2dvi(file=paste(fn,".tex",sep=""), pdf=TRUE, clean=TRUE, quiet=TRUE)
        }
        flush.console()
        setwd(tempdir)
      }
    })
  })

  ## submit button: remove columns from the sparkTable
  observe({
    input$removecols
    isolate({
      if ( length(input$removecols) > 0 ) {
        dat <- inputdata$dat
        index <- match(input$deletecols, names(dat@tableContent))
        if ( length(index) > 0 ) {
          dat@varType <- dat@varType[-index]
          dat@tableContent[index] <- NULL
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
    col_order <- input$sortable
    row_order <- input$sortable2
    inp <- dat@dataObj
    sparkO <- dat

    if ( !is.null(input$groups) ) {
      inp <- inp[inp[,1] %in% input$groups,,drop=FALSE]
    }
    sparkO@dataObj <- inp

    xx <- sparkO@tableContent
    vT <- sparkO@varType

    if ( !is.null(col_order) ) {
      index <- na.omit(match(col_order, names(sparkO@tableContent)))
      sparkO@tableContent <- sparkO@tableContent[index]
      sparkO@varType <- sparkO@varType[index]
    }

    if ( !is.null(row_order) ) {
      sparkO@dataObj[,1] <- as.character(sparkO@dataObj[,1])
      spl <- split(sparkO@dataObj, sparkO@dataObj[,1])
      sparkO@dataObj <- do.call("rbind", spl[row_order])
      rownames(sparkO@dataObj) <- NULL
      sparkO@dataObj[,1] <- factor(as.character(sparkO@dataObj[,1]), levels=row_order)
    }

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
      return(2)
    } else {
      if (is.null(input$newcoltype) ) {
        return(2)
      } else {
        cl <- input$newcoltype
        if ( cl == "box" ) { sel <- 1 }
        if ( cl == "line" ) { sel <- 2 }
        if ( cl == "hist" ) { sel <- 3 }
        if ( cl == "bar" ) { sel <- 4 }
        if ( cl == "func" ) { sel <- 5 }
        return(sel)
      }
    }
  })

  add.newcolname <- reactive({
    input$removecols
    #defname <- paste("newcol",length(data()$cnames)+1,sep="")
    defname <- paste("newcol_",gsub("[.]","",runif(1)*100),sep="")
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
      sel <- as.character(data()$groups)
    } else {
      sel <- as.character(input$groups)
    }
    sel <- sel[which(sel%in%data()$groups)]
    checkboxGroupInput("groups", label=h3("Select groups"), choices=as.character(data()$groups), selected=sel)
  })

  output$origdata = renderDataTable({
    data()$dat
  },escape=FALSE)

  output$modify.table = renderUI({
    html <- NULL
    cur.vals <- data()$cnames
    html <- paste(html, as.character(checkboxGroupInput("deletecols", label=h3("Select columns that should be available in the table"), choices=data()$cnames)))
    html <- paste(html, as.character(actionButton("removecols", "Remove selected columns", style="btn-danger")))

    html <- paste(html, h3("Add columns"))

    html <- paste(html, as.character(div(class="row-fluid",
      div(class = "span4", as.character(textInput("newcolname", label=h4("column name"), value=add.newcolname()))),
      div(class = "span4", as.character(selectInput("newcoltype", h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box"), selected=add.newtype()))),
      div(class = "span4", as.character(selectInput("newvartype", h4("variable"),choices=data()$vars, selected=add.newvartype()))),
      div(class = "span4", p())
    )))
    html <- paste(html, as.character(actionButton("addnewcol", "Add another column", style="btn-primary")))
    HTML(html)
  })

  output$opts.global = renderUI({
    xx <- data()$sparkO@tableContent
    html <- NULL
    for ( i in 1:length(xx) ) {
      cl <- class(xx[[i]])
      if ( cl == "sparkbox" ) {
        sel <- "box"
      }
      if ( cl == "sparkline" ) {
        sel <- "line"
      }
      if ( cl == "sparkhist" ) {
        sel <- "hist"
      }
      if ( cl == "sparkbar" ) {
        sel <- "bar"
      }
      if ( cl == "function" ) {
        sel <- "func"
      }

      # header
      html <- paste(html, as.character(div(class="row-fluid",
        div(class="span12", actionButton(inputId=data()$cnames[i], label=paste("Modify column",data()$cnames[i]), style=c("btn-success")))
      )))

      html <- paste(html, as.character(div(class="row-fluid",
        div(class = "span3", style="text-align: left", h4("Column name")),
        div(class = "span9", style="text-align: left", as.character(textInput(paste("colname",i,sep=""), label=NULL, value=data()$cnames[i])))
      )))

      html <- paste(html,as.character(div(class="row-fluid",
        div(class = "span12", style="text-align: center", h4("Set plot type and variable to be plotted"))
      )))

      if ( cl == "function" ) {
        fn <- as.character(attributes(xx[[i]])$srcref)
        html <- paste(html, as.character(div(class="row-fluid",
          div(class = "span4", as.character(selectInput(paste("col",i,sep=""), strong("Type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=sel))),
          div(class = "span4", as.character(selectInput(paste("varType",i,sep=""), strong("Variable"),choices=data()$vars, selected=data()$varType[i]))),
          div(class = "span4", as.character(textInput(paste("fn",i,sep=""), label=strong("Custom function"), value=fn))),
          div(class = "span4", p()))))
      } else {
        html <- paste(html,as.character(div(class="row-fluid",
          div(class = "span4", as.character(selectInput(paste("col",i,sep=""),strong("Type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func"), selected=sel))),
          div(class = "span4", as.character(selectInput(paste("varType",i,sep=""), strong("Variable"),choices=data()$vars, selected=data()$varType[i]))),
          div(class = "span4", p()),
          div(class = "span4", p()))))
      }
      html <- paste(html,as.character(div(class="row-fluid",
        div(class = "span12", style="text-align: center", h4("Set additional options"))
      )))

      # optional parameters to set
      if ( cl == "sparkbox" ) {
        html <- paste(html, as.character(div(class="row-fluid",
          div(class="span4", selectInput(
            inputId = paste("outcol_select",i,sep=""),
            label = strong("Outlier color"),
            choices=unique(c(colors(), xx[[i]]@outCol)),
            selected=xx[[i]]@outCol)
          ),
          div(class="span4", selectInput(
            inputId = paste("bordercol_select",i,sep=""),
            label = strong("Border color"),
            choices=unique(c(colors(), xx[[i]]@boxCol[1])),
            selected=xx[[i]]@boxCol[2])
          ),
          div(class="span4", selectInput(
            inputId = paste("maincol_select",i,sep=""),
            label = strong("Main color"),
            choices=unique(c(colors(), xx[[i]]@boxCol[2])),
            selected=xx[[i]]@boxCol[1])
          )
        )))
      }
      if ( cl == "sparkline" ) {
        html <- paste(html, as.character(div(class="row-fluid",
          div(class="span4", sliderInput(
            inputId = paste("pointwidth_slider",i,sep=""),
            label = strong("Point-Width"),
            min=1,
            max=100,
            value=sparkTable:::pointWidth(xx[[i]]),
            step=1)
          ),
          div(class="span4", sliderInput(
            inputId = paste("linewidth_slider",i,sep=""),
            label = strong("Line-Width"),
            min=1,
            max=3,
            value=sparkTable:::lineWidth(xx[[i]]),
            step=0.5)
          ),
          div(class="span4", selectInput(
            inputId = paste("bool_show_iqr",i,sep=""),
            label = strong("show IQR"),
            choices=c("yes", "no"),
            selected=ifelse(xx[[i]]@showIQR,"yes","no"))
          )
        )))
      }
      if ( cl == "sparkhist" ) {
        html <- paste(html, as.character(div(class="row-fluid",
          div(class="span4", selectInput(
            inputId = paste("histcol_2_select",i,sep=""),
            label = strong("Color of bars"),
            choices=unique(c(colors(), xx[[i]]@barCol[2])),
            selected=xx[[i]]@barCol[2])
          ),
          div(class="span4", selectInput(
            inputId = paste("histcol_3_select",i,sep=""),
            label = strong("Border color"),
            choices=unique(c(colors(), xx[[i]]@barCol[3])),
            selected=xx[[i]]@barCol[3])
          ),
          div(class="span4", sliderInput(
            inputId = paste("histspacing_slider",i,sep=""),
            label = strong("Spacing percentage"),
            min=0,
            max=5,
            value=xx[[i]]@barSpacingPerc,
            step=0.1)
          )
      )))
      }
      if ( cl == "sparkbar" ) {
        html <- paste(html, as.character(div(class="row-fluid",
          #div(class="span4", selectInput(
          # inputId = paste("barcol_1_select",i,sep=""),
          # label = strong("barcol1"),
          # choices=unique(c(colors(), xx[[i]]@barCol[1])),
          # selected=xx[[i]]@barCol[1])
          #),
          div(class="span4", selectInput(
            inputId = paste("barcol_2_select",i,sep=""),
            label = strong("Color of bars"),
            choices=unique(c(colors(), xx[[i]]@barCol[2])),
            selected=xx[[i]]@barCol[2])
          ),
          div(class="span4", selectInput(
            inputId = paste("barcol_3_select",i,sep=""),
            label = strong("Border color"),
            choices=unique(c(colors(), xx[[i]]@barCol[3])),
            selected=xx[[i]]@barCol[3])
          ),
          div(class="span4", sliderInput(
            inputId = paste("barspacing_slider",i,sep=""),
            label = strong("Spacing percentage"),
            min=0,
            max=5,
            value=xx[[i]]@barSpacingPerc,
            step=0.1)
          )
      )))
      }
      if ( cl == "function" ) {
      }
    }
    html <- paste(html, as.character(actionButton("updateparameter", "Update spark object", style="btn-primary")))
    HTML(html)
  })

  output$sparkplot = renderDataTable({
    setwd(paste(tempdir,"/www",sep=""))
    cat("\n###################\n")
    tmpfile <- tempfile()
    tmpdat <- data()$sparkO
    save(tmpdat, file=tmpfile)
    fn <- paste("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"), sep="")
    cat("the required data and R-code to re-generate the current graphical table is:\n")
    cat(paste0("# load('",paste(unlist(strsplit(tmpfile, "\\\\")), collapse="\\\\"),"')\n"))
    cat(paste0("# export(tmpdat, outputType='html', filename='",fn,"', graphNames='spark') # html\n"))
    cat(paste0("# export(tmpdat, outputType='tex', filename='",fn,"', graphNames='spark') # tex\n"))
    cat("###################\n")
    flush.console()
    m <- export(data()$sparkO,outputType="html",filename=NULL,graphNames="out")
    setwd("../")
    cbind(rownames(m),m)
  }, escape=FALSE)

  output$sort_cols <- renderUI({
    returnOrderCols("sortable", data()$cnames)
  })

  output$sort_rows <- renderUI({
    returnOrderRows("sortable2", unique(data()$sparkO@dataObj[,1]))
  })

  output$showorder <- renderPrint({
    print(input$sortable2)
  })
})
