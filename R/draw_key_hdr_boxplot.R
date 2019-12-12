#' @export
#' @importFrom grDevices rgb
col2hex <- function(col){
  grDevices::rgb(col,  maxColorValue=255)
}

#' @importFrom  grid rectGrob
#' @importFrom farver convert_colour
#' @importFrom grDevices col2rgb
#' @export
draw_key_hdr_boxplot <- function(data, params, size) {
  fill_colour <- data$fill %||% "grey20"
  fill_colour2 <- farver::convert_colour(t(
    grDevices::col2rgb(fill_colour)), "RGB", "HSL")
  fill_colour2[1,3] <- fill_colour2[1,3]*0.5
  fill_colour2 <- farver::convert_colour(fill_colour2, "HSL", "RGB")
  fill_colour2 <- col2hex(fill_colour2)
  grid::grobTree(
    grid::rectGrob(height = 0.5, width = 0.75,
                   gp = grid::gpar(fill = fill_colour)),
    grid::rectGrob(height = 0.25, width = 0.75,
                   gp = grid::gpar(fill = fill_colour2)),
    gp = grid::gpar(col = "black",
              lwd = (data$size %||% 0.5) * .pt,
              lty = data$linetype %||% 1
    )
  )
}
