#' Say hello
#'
#' Prints a hello message
#' @param name A character string
#' @return A greeting
#' @export
hello <- function(name = "world") {
  paste("Hello,", name, "!")
}
