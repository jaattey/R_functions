
#'---
#'title: "`r output$report_title`"
#'author: "`r output$name `"
#'date: "`r Sys.Date()`"
#'output: html_document
#'---
#'
#'
#+  echo = FALSE, message = FALSE, warning = FALSE, results = "asis"
#output = get_jira("AUR-7024")
write_full_report(output = output)
#append ### to content, * to values and cat the shit
#paste("*" , values[[18]])
