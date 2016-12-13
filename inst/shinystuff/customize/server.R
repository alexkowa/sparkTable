library(shiny)
shinyServer(function(session, input, output) {
  for ( file in list.files("controllers") ) {
    source(file.path("controllers", file), local = TRUE)
  }

  # recovered from Options
  inputdata <- reactiveValues()
  inputdata$dat <- options("sparkTable.dat")[[1]]
  inputdata$tmpdir <- options("sparkTable.tempdir")[[1]]
  inputdata$outputDir <- options("sparkTable.outputDir")[[1]]

  outputDir <- reactive({
    inputdata$outputDir
  })

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
  observeEvent(input$updateparameter, {
    dat <- inputdata$dat
    sparkO <- dat
    xx <- dat@tableContent
    i <- match(input$selCol, names(xx))
    vT <- dat@varType
    xx <- manage.cols(i, xx, input)
    vT <- manage.vars(i, vT, input)
    sparkO@tableContent <- xx
    sparkO@varType <- vT
    inputdata$dat <- sparkO
  })

  ## export to html-button
  observeEvent(input$exporthtml, {
    setwd(inputdata$outputDir)
    fn <- paste0("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"))
    m <- export(data()$sparkO,outputType="html",filename=fn,graphNames="spark")
    cat("--> exported to",inputdata$outputDir,"\n")
    flush.console()
    #setwd(inputdata$tmpdir)
  })

  ## export to latex-button
  observeEvent(input$exportlatex, {
    setwd(inputdata$outputDir)
    fn <- paste0("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"))
    m <- export(data()$sparkO,outputType="tex",filename=fn,graphNames="spark")
    if ( Sys.which("pdflatex") != "" ) {
      texi2dvi(file=paste0(fn,".tex"), pdf=TRUE, clean=TRUE, quiet=TRUE)
    }
    cat("--> exported to",inputdata$outputDir,"\n")
    flush.console()
  })

  ## submit button: remove columns from the sparkTable
  observeEvent(input$removecols, {
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
  observeEvent(input$addnewcol, {
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
    col_order <- input$sortable1
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
})
