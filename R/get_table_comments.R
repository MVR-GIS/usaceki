#' @title Get Table Comments
#' @description Get table comments.
#' @param db_connection    DBI::dbconnect object; A database connection.
#' @returns A data frame of table comments.
#' @export
#' @importFrom ROracle dbGetQuery
get_table_comments <- function(db_connection) {
  tab_comments <- dbGetQuery(
    con_roracle,
    "
    SELECT table_name, comments
    FROM user_tab_comments
    WHERE comments IS NOT NULL
    "
  )
}
