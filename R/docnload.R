#' @title Doc and Load
#' @description Convienience function to regularly run key dev functions
#'   since buttons are no longer available in Postron.
#' @returns Nothing
docnload <- function() {
  devtools::document()
  attachment::att_amend_desc()
  devtools::load_all()
}
