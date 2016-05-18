output$sort_cols <- renderUI({
  id <- "sortable1"
  html <- htmlTemplate("tpl_one_col.html", inp=h4("Drag and drop the columns into the desired order"))
  html <- list(html,htmlTemplate("tpl_one_col.html", inp=HTML(html_list(vars=data()$cnames, id=id))))
  html <- list(html, tags$script(paste0("$(function() {$( '#",id,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",id,"' ).disableSelection(); });")))
  html
})

output$sort_rows <- renderUI({
  id <- "sortable2"
  html <- htmlTemplate("tpl_one_col.html", inp=h4("Drag and drop the rows into the desired order"))
  html <- list(html,htmlTemplate("tpl_one_col.html", inp=HTML(html_list(vars=unique(data()$sparkO@dataObj[,1]), id=id))))
  html <- list(html, tags$script(paste0("$(function() {$( '#",id,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",id,"' ).disableSelection(); });")))
  html
})

#output$show_colorder <- renderPrint({
#  print(input$sortable1)
#})


#output$show_roworder <- renderPrint({
#  print(input$sortable2)
#})

output$ui_sort_reorder <- renderUI({
  list(
    uiOutput("sort_rows"), #verbatimTextOutput("show_roworder"),
    uiOutput("sort_cols"))#, verbatimTextOutput("show_colorder"))
})
