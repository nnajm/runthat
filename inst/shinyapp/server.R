# Define server logic for random distribution application
library(testthat)  ## TODO : remove this line 
shinyServer(function(input, output) {
	reporter <- HtmlReporter$new()
	# Reactive expression to generate the requested distribution. This is 
	# called whenever the inputs change. The output renderers defined 
	# below then all used the value computed from this expression
	
	
	
	
	
	
	
	
	# Generate an HTML table view of the data
	output$table <- function()
	{
		if(input$runButton == 0)
			return(NULL)
		isolate(test_file(input$dirPath, reporter = reporter))
		reporter$render()
	}
})

