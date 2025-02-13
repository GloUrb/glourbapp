#' Format a safe SQL query
#'
#' @param search_string the query search condition
#' @param column the column in which the words are searched
#' @export
process_query <- function(search_string, column) {
  # Remplacer OR et AND tout en gardant les parenthèses
  search_string <- gsub("\\bOR\\b", "OR", search_string, ignore.case = TRUE)
  search_string <- gsub("\\bAND\\b", "AND", search_string, ignore.case = TRUE)

  # Diviser en termes pour ajouter ILIKE individuellement
  terms <- unlist(strsplit(search_string, "(\\sOR\\s|\\sAND\\s)"))
  terms <- trimws(terms)  # Nettoyage des espaces

  # Construire la condition SQL avec ILIKE pour chaque terme
  conditions <- sapply(terms, function(term) {
    if (grepl("[()]", term)) {
      # Si le terme contient des parenthèses, le conserver tel quel
      return(term)
    } else {
      return(glue::glue("{column} ILIKE '%{term}%'"))
    }
  })

  # Remplacer les termes dans la requête originale
  sql_query <- search_string
  for (i in seq_along(terms)) {
    sql_query <- sub(terms[i], conditions[i], sql_query, fixed = TRUE)
  }

  return(sql_query)
}
