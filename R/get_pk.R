#' @title Get Primary Keys
#' @description Get data base primary keys
#' @param db_connection    DBI::dbconnect object; A database connection.
#' @returns A data frame list the primary key column for each table.
#' @export
#' @importFrom ROracle dbGetQuery
get_pk <- function(db_connection) {
  pk_info <- dbGetQuery(
    db_connection,
    "
    SELECT c.table_name, cc.column_name
    FROM user_constraints c
    JOIN user_cons_columns cc ON c.constraint_name = cc.constraint_name
    WHERE c.constraint_type = 'P'
  "
  )
}
