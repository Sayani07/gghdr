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
    c(box[[2]], box[[1]]), ncol = 2,
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
col2hex <- function(col){
  grDevices::rgb(col,  maxColorValue = 255)
}

#' @title darken_fill
#' @description darken fill colors for probability ranges
#' @param col colors
#' @param prob probability values
#' @rdname darken_fill
#' @export
#' @importFrom grDevices grey.colors
#' @importFrom grDevices colorRampPalette
darken_fill <- function(col, prob) {
  n_prob <- length(unique(prob))
  if (col[1] == "white"){
    # code breaks, i.e. won't give us distinct colour, if col is on the grey scale.
    # Will handle those cases separately
    col <- grDevices::grey.colors(n = n_prob, rev=ifelse(col[1]=="white", FALSE, TRUE))
  } else {
    palette  <- grDevices::colorRampPalette(colors=c(col[1], "white"))
    col <- palette(c(n_prob+5))[n_prob:1]
    }
return(col)
}
