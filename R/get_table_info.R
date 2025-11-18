#' @title Get Table Info
#' @description Get the columns for each table.
#' @param db_connection    DBI::dbconnect object; A database connection.
#' @param table_vector     character; A character vector of table names as
#'                         returned by the get_tables function.
#' @returns A list of tables containing a data frame of columns names(for
#'          each table).
#' @export
#' @importFrom ROracle dbGetQuery
get_table_info <- function(db_connection, table_vector) {
  table_info <- lapply(table_vector, function(tbl) {
    dbGetQuery(
      db_connection,
      paste0(
        "
        SELECT column_name, data_type, nullable 
        FROM user_tab_columns 
        WHERE table_name = '",
        tbl,
        "'"
      )
    )
  })

  names(table_info) <- table_vector

  return(table_info)
}
