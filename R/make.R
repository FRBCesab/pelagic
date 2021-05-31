#' Run an R Makefile
#'
#' This function sources the `_make.R` script located at the root of the project.
#' 
#' @importFrom here here
#' @export
#' 
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @examples
#' make()


make <- function() {
  
  source(here::here("_make.R"))
}

