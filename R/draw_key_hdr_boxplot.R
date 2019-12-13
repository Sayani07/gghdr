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
#' @importFrom farver convert_colour
#' @importFrom grDevices col2rgb

darken_fill <- function(col, prob) {
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  n_prob <- length(unique(prob))
  col[,3] <- seq(90 - (n_prob - 1)*10, 90, length.out = n_prob)[match(prob, sort(unique(prob)))]
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}

#' @title draw_key_hdr_boxplot
#' @description draw legend key for HDR box plot
#' @importFrom grid rectGrob grobTree gpar
#' @importFrom grDevices col2rgb
#' @export

draw_key_hdr_boxplot <- function(data, params, size) {
  fill_colour <- data$fill %||% "grey20"
  fill_colour <- darken_fill(rep(fill_colour, 2), c(0.5, 0.9))

  grid::grobTree(
    grid::rectGrob(height = 0.5, width = 0.75,
                   gp = grid::gpar(fill = fill_colour[2])),
    grid::rectGrob(height = 0.25, width = 0.75,
                   gp = grid::gpar(fill = fill_colour[1])),
    gp = grid::gpar(col = NA,
              lwd = (data$size %||% 0.5) * .pt,
              lty = data$linetype %||% 1
    )
  )
}
