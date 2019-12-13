#' @title `%||%`
#' @description if not null then A, else B

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @title hdr_boxes
#' @description set up the dimensions for the HDR boxes
#' @param hdr HDR probabilities
#' @rdname hdr_boxes

hdr_boxes <- function(hdr) {
  box <- split(hdr$hdr, col(hdr$hdr) %% 2)
  matrix(
    c(box[[2]], box[[1]]), ncol = 2,
    dimnames = list(NULL, c("lower", "upper"))
  )
}
