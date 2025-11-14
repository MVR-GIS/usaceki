#' @title Create Absolute Uniform Resource Identifier (URI)
#' @description Constructs an absolute Uniform Resource Identifier (URI) from
#'   the specified namespace.
#' @param namespace  named character vector; A "namespace" or RDF vocabulary
#'                   is a collection of namespace Internationalized
#'                   Resource Identifiers (IRI) used to construct URIs. Must be
#'                   specified as a named vector of the form:
#'                   namespace prefix = namespace IRI
#' @param prefix     character; Nmaespace prefix
#' @param local      character;
#' @returns a URI built from the
#' @details See https://www.w3.org/TR/rdf11-concepts for definitions of these
#'   terms.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rlang is_named
uri <- function(namespace, prefix, local) {
  assert_that(
    rlang::is_named(namespace),
    msg = "namespace must be a named character vector"
  )

  paste0(namespace[[prefix]], local)
}
