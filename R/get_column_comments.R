#' @title Get Column Comments
#' @description Get column comments.
#' @param db_connection    DBI::dbconnect object; A database connection.
#' @returns
#'
#' @export
#' @examples
get_column_comments <- function(db_connection) {
  col_comments <- dbGetQuery(
    db_connection,
    "
    SELECT table_name, column_name, comments
    FROM user_col_comments
    WHERE comments IS NOT NULL
    "
  )
}
