#' Run an R makefile
#'
#' Sources the `_make.R` script located at the root of the project.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' make()
#' }


make <- function() source(here::here("_make.R"))
