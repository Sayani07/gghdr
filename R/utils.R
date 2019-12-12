"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

darken_fill <- function(col, prob){
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  col[,3] <- prob*100
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}
