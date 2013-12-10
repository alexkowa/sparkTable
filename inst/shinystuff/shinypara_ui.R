shinyUI(
  bootstrapPage(
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
				tabPanel("sparkTable-Plot", 
					dataTableOutput("sparkplot"),
					actionButton("exporthtml", "Export to html", style="btn-primary"),
					actionButton("exportlatex", "Export to latex", style="btn-success")
				),
        tabPanel("gridster",  
        gridster(shift_larger_widgets_down=FALSE, 
						width = 20, height = 20, 
						min.cols=2, max.cols=2, 
						marginx=20, marginy=20,
					 gridsterItem(col = 1, row = 1, sizex = 1, sizey = 1,
					 	textOutput("A")			
					 ),
					 gridsterItem(col = 2, row = 1, sizex = 1, sizey = 1,
					  textOutput("B")			
					 ),
					 gridsterItem(col = 1, row = 2, sizex = 1, sizey = 1,
					  textOutput("C")				
					 ),
 					 gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
 					 	textOutput("D")					 						 
					 )
        	)
        )
      )
    )
  )
)
