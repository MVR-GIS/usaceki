# Use the `keyring` package to save the database username and password in the
# system credential store
key_service <- "egis-db-restoration-features"
user_name <- "QRI"
# Set once on each computer prior to running tests
#keyring::key_set(service = key_service, username = user_name)

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
