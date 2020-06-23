`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @title hdr_boxes
#' @description set up the dimensions for the HDR boxes
#' @param hdr HDR probabilities
#' @rdname hdr_boxes
hdr_boxes <- function(hdr) {
  box <- split(hdr$hdr, col(hdr$hdr) %% 2)
  matrix(
    c(box[[2]], box[[1]]),
    ncol = 2,
    dimnames = list(NULL, c("lower", "upper"))
  )
}

#' @title col2hex
#' @description converts colors to RGB
#' @param col colors
#' @return RGB colors
#' @rdname col2hex
#' @export
#' @importFrom grDevices rgb
col2hex <- function(col) {
  grDevices::rgb(col, maxColorValue = 255)
}

#' @title darken_fill
#' @description darken fill colors for probability ranges
#' @param col colors
#' @param prob probability values
#' @rdname darken_fill
#' @export
#' @importFrom farver convert_colour
#' @importFrom grDevices col2rgb
darken_fill <- function(col, prob) {
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  n_prob <- length(unique(prob))
  col[, 3] <- seq(90 - (n_prob - 1) * 10, 90,
    length.out = n_prob
  )[match(prob, sort(unique(prob)))]
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}
