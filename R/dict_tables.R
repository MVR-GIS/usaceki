#' @title Get Dictionary Tables
#' @description Split a dict_df into a named list of data.frames, one per table/entity.
#' @param dict_df tibble returned by build_dictionary()
#' @return named list where names are table labels
#' @export
dict_tables <- function(dict_df) {
  if (nrow(dict_df) == 0) {
    return(list())
  }
  tables <- split(dict_df, dict_df$table_label)
  # ensure names are table labels
  names(tables) <- vapply(
    tables,
    function(x) unique(x$table_label),
    character(1)
  )
  tables
}
