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
																results[[context]][[test]] <<- append(results[[context]][[test]], list(result))
																#results[[context]] <<- append(results[[context]], list(result))
															},
															
															end_reporter = function() {
																output <- '<table cellpading="0" cellspacing="0">'
																mapply(function(context_name, context_results){
																	#browser()
																	output <<- paste(output, '<tr><td><div class="runthat-context-name"> Context: ', context_name, '</div></td></tr>')
																	mapply(function(test_name, test_results){
																		output <<- paste(output, '<tr><td><ul><li class="runthat-test-name">&#9193;&nbsp;', test_name ,'</li>')
																		output <<- paste(output, 
																										 paste('<li><table class="runthat-context-results" cellpading="0" cellspacing="0">',
																										 			'<tr><th></th><th></th><th></th><th>time</th></tr>'
																										 ))
																		lapply(test_results,function(result){
																			#browser()
																			output <<- paste(output,paste('<tr>' ,
																																		#'<td class="runthat-checkbox"><input type="checkbox"/></td>',
																																		'<td class="runthat-checkbox"></td>',
																																		'<td class="runthat-result-', ifelse(result$passed,"OK","NOK"), '">', ifelse(result$passed,"&#10004;","&#10008;"),'</td>',
																																		'<td>', ifelse(result$passed,result$success_msg, gsub("\n", " ", result$failure_msg)),'</td>',
																																		'<td class="runthat-result-time"> ',result$time,'</td>',																																	
																																		'</tr>',sep='')
																											 ,sep='\n')
																		})
																		output <<- paste(output, '</table></li></ul></td></tr>')
																	}, names(context_results), context_results)
																	
																}, names(results), results)
																output <<- paste(output,"</table>",sep="\n")
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