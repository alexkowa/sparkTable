output$modify_table <- renderUI({
  cur.vals <- data()$cnames

  cb1 <- checkboxGroupInput("deletecols", label=h3("Select columns that should be available in the table"), choices=data()$cnames, width="100%")
  btn1 <- actionButton("removecols", "Remove selected columns", style="btn-danger")

  html <- list(
    htmlTemplate("tpl_one_col.html", inp=cb1),
    htmlTemplate("tpl_one_col.html", inp=btn1)
  )

  html <- list(html, htmlTemplate("tpl_one_col.html", inp=h3("Add a new column")))
  txt3 <- textInput("newcolname", label=h4("Select a column-name"), value=add.newcolname())
  sel3a <- selectInput("newcoltype", h4("type"),choices=list("sparkline"="line", "histogram"="hist", "barplot"="bar", "boxplot"="box"), selected=add.newtype(), width="100%")
  sel3b <- selectInput("newvartype", h4("variable"),choices=data()$vars, selected=add.newvartype(), width="100%")
  btn3 <- actionButton("addnewcol", "Add another column", style="btn-primary")
  html <- list(html,
    htmlTemplate("tpl_three_col.html", inp1=txt3, inp2=sel3a, inp3=sel3b),
    htmlTemplate("tpl_one_col.html", inp=btn3))

  html
})
