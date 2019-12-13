#' @title `%||%`
#' @description if not null then A, else B

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

