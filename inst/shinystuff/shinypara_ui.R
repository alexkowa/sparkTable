shinyUI(
  bootstrapPage(
		tags$head(
			tags$script(src = "js/jquery-ui.min.js"),
  		tags$script(src='js/sort.js'),
   		tags$link(rel='stylesheet', type='text/css', href='sort.css'),
			tags$link(rel='stylesheet', type='text/css', href='shared/slider/css/jquery.slider.min.css'),
			tags$script(src='shared/slider/js/jquery.slider.min.js')
		),		
    headerPanel("plotSparkTable using shiny..."),
    mainPanel(
      tabsetPanel(
        tabPanel("importData",
					sidebarPanel(
  					 uiOutput("cgroups")
					),		
					dataTableOutput("origdata")
				),
				tabPanel("modify table", uiOutput("modify.table")),				
				tabPanel("global options",
					uiOutput("opts.global")						
				),
				tabPanel("sortable",
   				uiOutput('sort_cols'),
					uiOutput('sort_rows'),
  				verbatimTextOutput('showorder')					
				),				
				tabPanel("sparkTable-Plot", 
					dataTableOutput("sparkplot"),
					actionButton("exporthtml", "Export to html", style="btn-primary"),
					actionButton("exportlatex", "Export to latex", style="btn-success")
				)
      )
    )
  )
)
