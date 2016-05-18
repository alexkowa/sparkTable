library(shiny)
shinyUI(navbarPage("Customize a SparkTable",
  tabPanel("Input", uiOutput("ui_input")),
  tabPanel("Add/Delete rows/columns", uiOutput("modify_table")),
  tabPanel("Customize", uiOutput("ui_customize_global")),
  tabPanel("Sort/Re-order", uiOutput("ui_sort_reorder")),
  tabPanel("Plot and Export", uiOutput("ui_plot_export")),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="bootswatch_yeti.css"),
    tags$script(src="js/jquery-ui.min.js"),
    tags$script(src='js/sort.js')
    #tags$link(rel='stylesheet', type='text/css', href='sort.css')
  )
))
