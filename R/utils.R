"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

darken_fill <- function(col, prob){
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  col[,3] <- seq(0, 20, length.out = length(unique(prob)))[match(prob, sort(unique(prob)))] + 70
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}
