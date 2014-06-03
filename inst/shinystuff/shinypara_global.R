require(tools)

load("data.rdata")
load("dirs.rdata")

outputDir <- dirs[1]
tempdir <- dirs[2]

# manage columns of sparkTable obj
#manage.cols <- function(i, inp, varType, input) {
manage.cols <- function(i, inp, input) {
  if ( is.null(input) ) {
    return(inp)
  }
  v <- eval(parse(text=paste("input$col",i,sep="")))
  if ( !is.null(v) ) {
    if ( v == "line" ) {
      inp[[i]] <- newSparkLine()

      # point width
      v.pw <- eval(parse(text=paste("input$pointwidth_slider",i,sep="")))
      if ( !is.null(v.pw) ) {
        sparkTable:::pointWidth(inp[[i]]) <- v.pw
      }
      v.lw <- eval(parse(text=paste("input$linewidth_slider",i,sep="")))
      if ( !is.null(v.lw) ) {
        sparkTable:::lineWidth(inp[[i]]) <- v.lw
      }
      v.show_iqr <- eval(parse(text=paste("input$bool_show_iqr",i,sep="")))
      if ( !is.null(v.lw) ) {
        inp[[i]]@showIQR <- ifelse(v.show_iqr=="yes", TRUE, FALSE)
      }
    }
    if ( v == "box" ) {
      inp[[i]] <- newSparkBox()

      v.outcol <- eval(parse(text=paste("input$outcol_select",i,sep="")))
      if ( !is.null(v.outcol) ) {
        inp[[i]]@outCol <- v.outcol
      }
      v.bordercol <- eval(parse(text=paste("input$bordercol_select",i,sep="")))
      if ( !is.null(v.bordercol) ) {
        inp[[i]]@boxCol[1] <- v.bordercol
      }
      v.maincol <- eval(parse(text=paste("input$maincol_select",i,sep="")))
      if ( !is.null(v.maincol) ) {
        inp[[i]]@boxCol[2] <- v.maincol
      }
    }
    if ( v == "hist" ) {
      inp[[i]] <- newSparkHist()
      v.histcol2 <- eval(parse(text=paste("input$histcol_2_select",i,sep="")))
      if ( !is.null(v.histcol2) ) {
        inp[[i]]@barCol[2] <- v.histcol2
      }
      v.histcol3 <- eval(parse(text=paste("input$histcol_3_select",i,sep="")))
      if ( !is.null(v.histcol3) ) {
        inp[[i]]@barCol[3] <- v.histcol3
      }
      v.histspacing <- eval(parse(text=paste("input$histspacing_slider",i,sep="")))
      if ( !is.null(v.histspacing) ) {
        inp[[i]]@barSpacingPerc <- v.histspacing
      }
    }
    if ( v == "bar" ) {
      inp[[i]] <- newSparkBar()
      #v.barcol1 <- eval(parse(text=paste("input$barcol_1_select",i,sep="")))
      #if ( !is.null(v.barcol1) ) {
      # inp[[i]]@barCol[1] <- v.barcol1
      #}
      v.barcol2 <- eval(parse(text=paste("input$barcol_2_select",i,sep="")))
      if ( !is.null(v.barcol2) ) {
        inp[[i]]@barCol[2] <- v.barcol2
      }
      v.barcol3 <- eval(parse(text=paste("input$barcol_3_select",i,sep="")))
      if ( !is.null(v.barcol3) ) {
        inp[[i]]@barCol[3] <- v.barcol3
      }
      v.barspacing <- eval(parse(text=paste("input$barspacing_slider",i,sep="")))
      if ( !is.null(v.barspacing) ) {
        inp[[i]]@barSpacingPerc <- v.barspacing
      }
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
    #cat("something has changed!\n"); flush.console()
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

textInput <- function (inputId, label, value = "") {
  if ( is.null(label) ) {
    tagList(tags$input(id = inputId, type = "text", value = value))
  } else {
    tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, type = "text", value = value))
  }
}

html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for( i in vars ) {
    hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  }
  paste0(hl, "</ul>")
}

returnOrderCols <- function(inputId, vars) {
  tagList(
    h3("Please drag and drop the columns into the desired order"),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}
returnOrderRows <- function(inputId, vars) {
  tagList(
      h3("Please drag and drop the rows (groups) into the desired order"),
      HTML(html_list(vars, inputId)),
      tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}
