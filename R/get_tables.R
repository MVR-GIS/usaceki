#' @title Get Tables
#' @description  Retrieve a list of tables from the database connection.
#' @param db_connection    A DBI::dbconnect object; A database connection.
#' @param exclude_pattern  character; A regex character string matching tables
#'                         names to exclude. The default includes all tables.
#' @returns A character vector of table names.
#' @export
#' @importFrom ROracle dbGetQuery
#' @importFrom stringr str_subset
#'
get_tables <- function(db_connection, exclude_pattern = "(?!)") {
  tables <- ROracle::dbGetQuery(
    db_connection,
    "SELECT table_name FROM user_tables"
  )$TABLE_NAME

  # Exclude tables that don't represent the data model
  tables_dm <- stringr::str_subset(
    tables,
    pattern = exclude_pattern,
    negate = TRUE
  )

  # Verify tables to include in the data model
  sort(tables_dm)
}
