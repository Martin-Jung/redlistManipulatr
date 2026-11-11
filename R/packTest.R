#' Check that a required package is installed
#' @description Internal helper that stops execution when a package required by a function is unavailable.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param pack Package name as a character string.

packTest <-
  function(
    pack
  ){
    if (!requireNamespace(pack, quietly = TRUE)) {
      stop(paste0("Package \"", pack, "\" is needed for this function to work. Please install it."),
           call. = FALSE)
    }
  }
