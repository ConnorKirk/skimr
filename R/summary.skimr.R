#' Summary function for skim_df. This is a method of the generic function \code{summary}
#' 
#' @param df a skim dataframe
#' @return A summary of the dataframe \code{df}
#' @export
#' @examples
#' 
#'a <- skim(mtcars)
#'summary(a)
#' 
#'
summary.skim_df <- function(object, ...){

  if (is.null(object)) {
    stop("dataframe is null.")
  }
  
  n_rows <- paste0("Number of Rows: ", attr(object, "data_rows"), "\n")
  n_cols <- paste0("Number of Columns: ", attr(object, "data_cols"), "\n")
  df_name <- paste0("Name: ", attr(object, "df_name"), "\n")
  
  types <- unique(object$type)
  
  type_frequency <- table(object$type)
  type_frequency_string <- paste0(names(type_frequency), ": ", type_frequency, collapse = "\n")

  
  cat("A skim object\n\n",
      df_name, n_rows, 
      n_cols, 
      "\nColumn Types\n",
      type_frequency_string
      ,sep = "")
  
}
