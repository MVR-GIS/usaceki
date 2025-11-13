#' @title Get Foreign Keys
#' @description Get foreign keys.
#' @param db_connection    DBI::dbconnect object; A database connection.
#' @returns A data frame.
#' @export
#' @importFrom ROracle dbGetQuery
get_fk <- function(db_connection) {
  fk_info <- dbGetQuery(
    con_roracle,
    "
    SELECT a.table_name AS child_table,
           a.column_name AS fk_column,
           c_pk.table_name AS parent_table
    FROM user_cons_columns a
    JOIN user_constraints c 
         ON a.constraint_name = c.constraint_name
    JOIN user_constraints c_pk 
         ON c.r_constraint_name = c_pk.constraint_name
    WHERE c.constraint_type = 'R'
    "
  )
}
