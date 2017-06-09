#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project													#
#FileName		-	ui.r								  											#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/7/2017									  									#
#																	  								#
#Purpose:		-	Shiny App																		#
#Notes:			- 																					#
#																									#
#####################################################################################################

#ui.R

require(data.table)
require(shiny)

options(scipen=20)	
options(warn= -1)

fluidPage(

# Application title
titlePanel("American Community Survey - Statistical Snapshot - 2015"),

    # ACS Viewer - United States
	
	#United States Layout
	sidebarLayout(
		sidebarPanel(width = 2,
			helpText("All States")
			,helpText("All Map graphs can be zoomed by click dragging a box and double-clicking on the area. 
							Please allow several seconds for each plot and each tab to render.
							The data is very big.")
			
		),
		
		mainPanel(
			tabsetPanel(
				#Income
				tabPanel("Income",
						splitLayout(cellWidths = c("50%", "50%"),
							  plotOutput("plot3b", height="400px", width="100%",
								dblclick = "plot3b_dblclick",
								brush = brushOpts(
									id = "plot3b_brush",
									resetOnNew = TRUE
								)
							),
							plotOutput("plot12b", height="400px", width="100%")  #Wage Distribution 
						),
						fluidRow(title="Top 5 and Bottom 5 Average Wages by PUMA",
						  tableOutput("tbl3b")
						) 
					),
				#Population
				tabPanel("Population",
						splitLayout(cellWidths = c("50%", "50%")
							, plotOutput("plot1b", height="400px", width="100%",
								dblclick = "plot1b_dblclick",
								brush = brushOpts(
									id = "plot1b_brush",
									resetOnNew = TRUE
								)
							)
							,plotOutput("plot2b", height="400px", width="100%",
								dblclick = "plot2b_dblclick",
								brush = brushOpts(
									id = "plot2b_brush",
									resetOnNew = TRUE
								)
							)
						),
						fluidRow(title="Top 5 and Bottom 5 Population Growth Rates by PUMA",
						  tableOutput("tbl1b")
						) 
					),
				#Unemployment
				tabPanel("Unemployment",
						splitLayout(cellWidths = c("50%", "50%")
							, plotOutput("plot4b", height="400px", width="100%",
								dblclick = "plot4b_dblclick",
								brush = brushOpts(
									id = "plot4b_brush",
									resetOnNew = TRUE
								)
							)
							,plotOutput("plot5b", height="400px", width="100%",
								dblclick = "plot5b_dblclick",
								brush = brushOpts(
									id = "plot5b_brush",
									resetOnNew = TRUE
								)
							)
						),
						fluidRow(title="Top 5 and Bottom 5 Unemployment Rates by PUMA",
						  tableOutput("tbl4b")
						) 
					),	
				#Other Graphics
				tabPanel("Age",
					fluidRow(
						plotOutput("plot6b", height="400px", width="1000px")  #Age Pyramid
					),
					fluidRow(
					    plotOutput("plot7b", height="400px", width="1000px")  #Age Unemployment Pyramid
					)
				),
				tabPanel("Occupation",
					fluidRow(
							plotOutput("plot8b", height="400px", width="1000px")  #Occupation Wages
					),
					fluidRow(
							plotOutput("plot9b", height="400px", width="1000px")  #Occupation Employment				
					)
				),
				tabPanel("Industry",
					fluidRow(
							plotOutput("plot10b", height="400px", width="1000px")  #Industry Wages
					),
					fluidRow(
							plotOutput("plot11b", height="400px", width="1000px")  #Industry Employment					
					)	
				)
			)
		)	
	),
	#State Layout
	sidebarLayout(
		sidebarPanel(width = 2,
			helpText("Select State"),	
			selectInput("State", "State:", 
                  choice = list("State")	
			)

		),
		mainPanel(
			tabsetPanel(
				#Income
				tabPanel("Income",
					splitLayout(cellWidths = c("50%", "50%"),
						 plotOutput("plot3a", height="400px", width="100%",
							dblclick = "plot3a_dblclick",
							brush = brushOpts(
								id = "plot3a_brush",
								resetOnNew = TRUE
							)
						),
						plotOutput("plot12a", height="400px", width="100%")  #Wage Distribution 
					),
				fluidRow(title="Top 5 and Bottom 5 Average Wages by PUMA",
					 tableOutput("tbl3a")
					) 
				),
				#Population
				tabPanel("Population",
					splitLayout(cellWidths = c("50%", "50%")
						, plotOutput("plot1a", height="400px", width="100%",
							dblclick = "plot1a_dblclick",
							brush = brushOpts(
								id = "plot1a_brush",
								resetOnNew = TRUE
							)
						)
						,plotOutput("plot2a", height="400px", width="100%",
							dblclick = "plot2a_dblclick",
							brush = brushOpts(
								id = "plot2a_brush",
								resetOnNew = TRUE
							)
						)
					),
				fluidRow(title="Top 5 and Bottom 5 Population Growth Rates by PUMA",
					 tableOutput("tbl1a")
					) 
				),	
				#Unemployment
				tabPanel("Unemployment",
					splitLayout(cellWidths = c("50%", "50%")
						, plotOutput("plot4a", height="400px", width="100%",
							dblclick = "plot4a_dblclick",
							brush = brushOpts(
								id = "plot4a_brush",
								resetOnNew = TRUE
							)
						)
						,plotOutput("plot5a", height="400px", width="100%",
							dblclick = "plot5a_dblclick",
							brush = brushOpts(
								id = "plot5a_brush",
								resetOnNew = TRUE
							)
						)
					),
				fluidRow(title="Top 5 and Bottom 5 Unemployment Rates by PUMA",
					 tableOutput("tbl4a")
					) 
				),
				#Other Graphics
				tabPanel("Age",
					fluidRow(
						plotOutput("plot6a", height="400px", width="1000px")  #Age Pyramid
					),
					fluidRow(
					    plotOutput("plot7a", height="400px", width="1000px")  #Age Unemployment Pyramid
					)
				),
				tabPanel("Occupation", 
					fluidRow(
						plotOutput("plot8a", height="400px", width="1000px")  #Occupation Wages
					),
					fluidRow(
						plotOutput("plot9a", height="400px", width="1000px")  #Occupation Employment				
					)
				),
				tabPanel("Industry",
					fluidRow(
						plotOutput("plot10a", height="400px", width="1000px")  #Industry Wages
					),
					fluidRow(
						plotOutput("plot11a", height="400px", width="1000px")  #Industry Employment					
					)					
				)
			)
		)
	)
)  #End Fluid Page
