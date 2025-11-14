#' @title Check for a Valid URI
#' @description Check for a valid URI.
#' @param uri_string  character; A URI string.
#' @returns boolean; TRUE if valid, FALSE if not valid
#' @export
#' @importFrom httr parse_url
is_valid_uri <- function(uri_string) {
  # The parse_url function breaks the URL into components
  parsed <- tryCatch(
    httr::parse_url(uri_string),
    error = function(e) return(NULL)
  )

  # A valid absolute URL generally requires a scheme and a hostname/host
  if (
    !is.null(parsed) && !is.null(parsed$scheme) && !is.null(parsed$hostname)
  ) {
    # Optional: Further check if the scheme is specifically http or https
    if (parsed$scheme %in% c("http", "https")) {
      return(TRUE)
    }
  }

  return(FALSE)
}
