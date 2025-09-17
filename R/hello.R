#' Say hello
#'
#' Prints (or returns) a hello message.
#'
#' @param name A character string.
#' @return A greeting string.
#' @examples
#' hello()
#' hello("Andrew")
#' @export
hello <- function(name = "world") {
  paste("Hello,", name, "!")
}
