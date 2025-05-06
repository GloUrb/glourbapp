#' Format a table for better display on shiny interface
#'
#' @param tib the table to format
#' @param large the names of columns to display as large (600px) columns. Defaults to c("text_en","text","context").
#' @param medium the names of columns to display as medium (300px) columns. Defaults to c("snippet","trans_snippet").
#' @export
format_table=function(tib, large=c("text_en","text","context","lemmatext"), medium=c("snippet","trans_snippet")){
  cols_all=1:ncol(tib)
  cols_large=which(colnames(tib) %in% large)
  cols_medium=which(colnames(tib) %in% medium)
  tib=tib %>%
    DT::datatable(escape=FALSE,selection="single") %>%
    DT::formatStyle(columns =cols_all, verticalAlign="top") %>%
    DT::formatStyle(columns =cols_large, width='600px') %>%
    DT::formatStyle(columns =cols_medium, width='300px')
  return(tib)
}
