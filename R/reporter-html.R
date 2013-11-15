library(testthat)

setClass("testExport")

HtmlReporter <- setRefClass("HtmlReporter", contains = "Reporter",
														fields = list("results" = "list", 
																					"timer" = "ANY",
																					"render"="ANY"),                         
														methods = list(
															initialize = function(...){
																render <<- function() "<table> <tr> <td> dummy row </td></tr> </table>"
																callSuper(...)
															},
															start_reporter = function() {
																callSuper()
																results <<- list()
																timer <<- proc.time()
																context <<- "(ungrouped)"
															},
															add_result = function(result) {
																
																result$time <- round((proc.time() - timer)[["elapsed"]], 2)
																timer <<- proc.time()
																result$test <- if (is.null(test) || test == "") "(unnamed)" else test
																result$call <- if (is.null(result$call)) "(unexpected)" else result$call
																results[[context]] <<- append(results[[context]], list(result))
															},
															
															end_reporter = function() {
																output <- "<table>"
																lapply(results[[context]],function(result){
																	
																	output <<- paste(output,paste('<tr>' ,
																		    '<td> ',result$test,'</td>',
																		    '<td> ',ifelse(result$passed,"OK","NO"),'</td>',
																	     '</tr>',sep='\n'),sep='\n')
																	
																})
																output <- paste(output,"</table>",sep="\n")
																render <<- function()output
# 																lapply(results[[context]],function(result){
# 																	if (result$passed) {
# 																		cat(colourise(".", fg = "light green"))
# 																	} else {
# 																		failed <<- TRUE
# 																		cat(paste0("labels: ",length(labels), '\n'))
# 																		cat(paste0("test: ",test, '\n'))
# 																		cat(paste0("passed: ",result$passed, ', '))
# 																		cat(paste0("error: ",result$error, '\n'))
# 																		cat(paste0("message:\n", result$message ,'\n'))
# 																		cat(paste0("elapsed time :", result$time,' seconds\n'))
# 																		
# 																	}
															}
														)
)

#file <- 'F:/Projects/R/testthat/test_example.r'
#test_file(file, reporter = HtmlReporter$new())