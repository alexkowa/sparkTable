output$ui_input <- renderUI({
  output$cgroups <- renderUI({
    if (is.null(input$groups)) {
      sel <- as.character(data()$groups)
    } else {
      sel <- as.character(input$groups)
    }
    sel <- sel[which(sel%in%data()$groups)]
    cb <- checkboxGroupInput("groups", label=h3("Select groups"), choices=as.character(data()$groups), selected=sel, inline=TRUE)
    htmlTemplate("tpl_one_col.html", inp=cb)
  })
  output$origdata = renderDataTable({
    df <- data()$dat
    if (is.null(df) | nrow(df)==0) {
      return(NULL)
    }
    df
  })
  list(uiOutput("cgroups"), htmlTemplate("tpl_one_col.html", inp=dataTableOutput("origdata")))
})
