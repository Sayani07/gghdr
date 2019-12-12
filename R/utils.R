"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

darken_fill <- function(col, prob){
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  n_prob <- length(unique(prob))
  col[,3] <- seq(90 - (n_prob - 1)*10, 90, length.out = n_prob)[match(prob, sort(unique(prob)))]
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}
