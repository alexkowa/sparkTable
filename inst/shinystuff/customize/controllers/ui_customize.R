output$ui_customize_global_old <- renderUI({
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
    html <- list(html, htmlTemplate("tpl_one_col.html", inp=h2(paste("Modify column",dQuote(data()$cnames[i])))))
    html <- list(html,
      htmlTemplate("tpl_two_col.html", inp1=h4("Column name"), inp2=textInput(paste("colname",i,sep=""), label=NULL, value=data()$cnames[i])))

    html <- list(html, htmlTemplate("tpl_one_col.html", inp=h4("Set plot type and variable to be plotted")))
    choices1 <- list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func")
    if ( cl == "function" ) {
      html <- list(html,
        htmlTemplate("tpl_three_col.html",
          inp1=selectInput(paste("col",i,sep=""), strong("Type"),choices=choices1, selected=sel, width="100%"),
          inp2=selectInput(paste0("varType",i), strong("Variable"),choices=data()$vars, selected=data()$varType[i], width="100%"),
          inp3=textInput(paste0("fn",i), label=strong("Custom function"), value=as.character(attributes(xx[[i]])$srcref))))
    } else {
      html <- list(html, htmlTemplate("tpl_two_col.html",
        inp1=selectInput(paste("col",i,sep=""), strong("Type"),choices=choices1, selected=sel, width="100%"),
        inp2=selectInput(paste0("varType",i), strong("Variable"),choices=data()$vars, selected=data()$varType[i], width="100%")))
    }

    html <- list(html, htmlTemplate("tpl_one_col.html", inp=h4("Set additional options")))

    # optional parameters to set
    if ( cl == "sparkbox" ) {
      html <- list(html,  htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("outcol_select",i), label=strong("Outlier color"),choices=unique(c(colors(), xx[[i]]@outCol)), selected=xx[[i]]@outCol, width="100%"),
        inp2=selectInput(inputId=paste0("bordercol_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@boxCol[1])), selected=xx[[i]]@boxCol[2], width="100%"),
        inp3=selectInput(inputId=paste0("maincol_select",i), label=strong("Main color"), choices=unique(c(colors(), xx[[i]]@boxCol[2])), selected=xx[[i]]@boxCol[1], width="100%")))
    }
    if ( cl == "sparkline" ) {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=sliderInput(inputId=paste0("pointwidth_slider",i), label=strong("Point-Width"), min=1, max=100, value=sparkTable:::pointWidth(xx[[i]]), step=1, width="100%"),
        inp2=sliderInput(inputId=paste0("linewidth_slider",i), label=strong("Line-Width"), min=1, max=3, value=sparkTable:::lineWidth(xx[[i]]), step=0.1, width="100%"),
        inp3=selectInput(inputId=paste0("bool_show_iqr",i), label=strong("show IQR"), choices=c("yes", "no"), selected=ifelse(xx[[i]]@showIQR,"yes","no"), width="100%")))
    }
    if ( cl == "sparkhist" ) {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("histcol_2_select",i), label=strong("Color of bars"), choices=unique(c(colors(), xx[[i]]@barCol[2])), selected=xx[[i]]@barCol[2], width="100%"),
        inp2=selectInput(inputId=paste0("histcol_3_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@barCol[3])), selected=xx[[i]]@barCol[3], width="100%"),
        inp3=sliderInput(inputId=paste0("histspacing_slider",i), label=strong("Spacing percentage"), min=0, max=5, value=xx[[i]]@barSpacingPerc, step=0.1, width="100%")))
    }
    if ( cl == "sparkbar" ) {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("barcol_2_select",i), label=strong("Color of bars"), choices=unique(c(colors(), xx[[i]]@barCol[2])), selected=xx[[i]]@barCol[2], width="100%"),
        inp2=selectInput(inputId=paste0("barcol_3_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@barCol[3])), selected=xx[[i]]@barCol[3], width="100%"),
        inp3=sliderInput(inputId=paste0("barspacing_slider",i), label=strong("Spacing percentage"), min=0, max=5, value=xx[[i]]@barSpacingPerc, step=0.1, width="100%")))
    }
  }
  html <- list(html, htmlTemplate("tpl_one_col.html", inp=actionButton("updateparameter", "Update spark object", style="btn-primary")))
  html
})


output$ui_customize_global <- renderUI({
  xx <- data()$sparkO@tableContent
  cn <- names(xx)
  html <- list(htmlTemplate("tpl_one_col.html", inp=h2(paste("Modify a column in the Table"))))

  sel1 <- selectInput("selCol", label=h4("Select a column"),choices=cn, selected=input$selCol, width="100%")
  html <- list(html, htmlTemplate("tpl_three_col.html",
    inp1=NULL, inp2=sel1, inp3=NULL))

  if (!is.null(input$selCol)) {
    i <- match(input$selCol, cn)

    cl <- class(xx[[input$selCol]])
    if (cl=="sparkbox") {
      sel <- "box"
    }
    if (cl == "sparkline") {
      sel <- "line"
    }
    if (cl == "sparkhist") {
      sel <- "hist"
    }
    if (cl == "sparkbar") {
      sel <- "bar"
    }
    if (cl == "function") {
      sel <- "func"
    }

    html <- list(html,
      htmlTemplate("tpl_two_col.html", inp1=h4("Change/Modify name"), inp2=textInput(paste("colname",i,sep=""), label=NULL, value=data()$cnames[i])))

    html <- list(html, htmlTemplate("tpl_one_col.html", inp=h4("Set plot type and variable to be plotted")))
    choices1 <- list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box", "function"="func")
    if (cl == "function") {
      html <- list(html,
        htmlTemplate("tpl_three_col.html",
          inp1=selectInput(paste("col",i,sep=""), strong("Type"),choices=choices1, selected=sel, width="100%"),
          inp2=selectInput(paste0("varType",i), strong("Variable"),choices=data()$vars, selected=data()$varType[i], width="100%"),
          inp3=textInput(paste0("fn",i), label=strong("Custom function"), value=as.character(attributes(xx[[i]])$srcref))))
    } else {
      html <- list(html, htmlTemplate("tpl_two_col.html",
        inp1=selectInput(paste("col",i,sep=""), strong("Type"),choices=choices1, selected=sel, width="100%"),
        inp2=selectInput(paste0("varType",i), strong("Variable"),choices=data()$vars, selected=data()$varType[i], width="100%")))
      html <- list(html, htmlTemplate("tpl_one_col.html", inp=h4("Set additional options")))
    }

    # optional parameters to set
    if (cl == "sparkbox") {
      html <- list(html,  htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("outcol_select",i), label=strong("Outlier color"),choices=unique(c(colors(), xx[[i]]@outCol)), selected=xx[[i]]@outCol, width="100%"),
        inp2=selectInput(inputId=paste0("bordercol_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@boxCol[1])), selected=xx[[i]]@boxCol[2], width="100%"),
        inp3=selectInput(inputId=paste0("maincol_select",i), label=strong("Main color"), choices=unique(c(colors(), xx[[i]]@boxCol[2])), selected=xx[[i]]@boxCol[1], width="100%")))
    }
    if (cl == "sparkline") {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=sliderInput(inputId=paste0("pointwidth_slider",i), label=strong("Point-Width"), min=1, max=100, value=sparkTable:::pointWidth(xx[[i]]), step=1, width="100%"),
        inp2=sliderInput(inputId=paste0("linewidth_slider",i), label=strong("Line-Width"), min=1, max=3, value=sparkTable:::lineWidth(xx[[i]]), step=0.1, width="100%"),
        inp3=selectInput(inputId=paste0("bool_show_iqr",i), label=strong("show IQR"), choices=c("yes", "no"), selected=ifelse(xx[[i]]@showIQR,"yes","no"), width="100%")))
    }
    if (cl == "sparkhist") {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("histcol_2_select",i), label=strong("Color of bars"), choices=unique(c(colors(), xx[[i]]@barCol[2])), selected=xx[[i]]@barCol[2], width="100%"),
        inp2=selectInput(inputId=paste0("histcol_3_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@barCol[3])), selected=xx[[i]]@barCol[3], width="100%"),
        inp3=sliderInput(inputId=paste0("histspacing_slider",i), label=strong("Spacing percentage"), min=0, max=5, value=xx[[i]]@barSpacingPerc, step=0.1, width="100%")))
    }
    if (cl == "sparkbar") {
      html <- list(html, htmlTemplate("tpl_three_col.html",
        inp1=selectInput(inputId=paste0("barcol_2_select",i), label=strong("Color of bars"), choices=unique(c(colors(), xx[[i]]@barCol[2])), selected=xx[[i]]@barCol[2], width="100%"),
        inp2=selectInput(inputId=paste0("barcol_3_select",i), label=strong("Border color"), choices=unique(c(colors(), xx[[i]]@barCol[3])), selected=xx[[i]]@barCol[3], width="100%"),
        inp3=sliderInput(inputId=paste0("barspacing_slider",i), label=strong("Spacing percentage"), min=0, max=5, value=xx[[i]]@barSpacingPerc, step=0.1, width="100%")))
    }
  }
  html <- list(html, htmlTemplate("tpl_one_col.html", inp=actionButton("updateparameter", "Update spark object", style="btn-primary")))
  html
})
