#' @title Draw legend key
#' @description draw legend key for HDR box plot
#' @param data data
#' @param params parameters
#' @param size size of legend key
#' @importFrom grid rectGrob grobTree gpar
#' @importFrom grDevices col2rgb
draw_key_hdr_boxplot <- function(data, params, size) {
  fill_colour <- data$fill %||% "grey20"
  fill_colour <- darken_fill(rep(fill_colour, 2), c(0.5, 0.9))

  grid::grobTree(
    grid::rectGrob(
      height = 0.5, width = 0.75,
      gp = grid::gpar(fill = fill_colour[2])
    ),
    grid::rectGrob(
      height = 0.25, width = 0.75,
      gp = grid::gpar(fill = fill_colour[1])
    ),
    gp = grid::gpar(
      col = NA,
      lwd = (data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    )
  )
}
