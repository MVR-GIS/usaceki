#' @title Get QRI Database Connection
#' @description Get a database connection to the USACE QRI schema. Only works
#'   on the USACE network CorpsNet.
#' @param set_key  boolean; Should the user be prompted to use the
#'                 keyring::key_set function to create the key_service on this
#'                 computer? If FALSE, uses the password store on the
#'                 computer.
#' @returns An OraConnection object whose class extends DBIConnection. This
#'   object is used to execute SQL queries on the USACE QRI database.
#' @importFrom keyring key_set key_get
#' @importFrom DBI dbDriver
#' @importFrom ROracle dbConnect
get_qri_connection <- function(set_key = TRUE) {
  # Use the `keyring` package to save the database username and password in the
  # system credential store
  key_service <- "mvr-egis-db-qri"
  user_name <- "QRI"

  if (set_key) {
    keyring::key_set(service = key_service, username = user_name)
  }

  # Make Oracle connection
  drv <- DBI::dbDriver("Oracle")
  host <- "egis-db"
  port <- "1521"
  sid <- "B5SDEDP1"
  connect_string <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=",
    host,
    ")(PORT=",
    port,
    "))",
    "(CONNECT_DATA=(SID=",
    sid,
    ")))"
  )

  con_roracle <- ROracle::dbConnect(
    drv,
    username = user_name,
    password = keyring::key_get(key_service, user_name),
    dbname = connect_string
  )
}
