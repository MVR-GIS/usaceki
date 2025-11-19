#' @title Doc and Load
#' @description Convienience function to run devtools::document() and
#'   devtools::load_all() since buttons are no longer available in Postron
#' @returns Nothing
doc_load <- function() {
  devtools::document()
  devtools::load_all()
}
