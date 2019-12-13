#' @title geom_hdr_boxplot
#' @details A box plot for the highest density region
#' Calculates and plots the boxplot of highest density regions in one dimension.
#' @rdname geom_hdr_boxplot
#' @importFrom ggplot2 layer aes
#' @param mapping
#' @importFrom ggplot2 layer aes
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param prob Probability coverage required for HDRs
#'
#' @export
#' @examples
#' ggplot2::ggplot(faithful, ggplot2::aes(y = eruptions)) +
#'   geom_hdr_boxplot()
geom_hdr_boxplot <- function(mapping = NULL, data = NULL,
                             stat = "hdr", position = "dodge2",
                             ...,
                             varwidth = FALSE, # do we want this?
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             prob = c(0.5, 0.95, 0.99)) {

  # Add basic input checks if needed

  if (stat == "hdr") {
    if (!inherits(mapping, "uneval")) {
      mapping <- ggplot2::aes()
    }
    mapping$prob <- quote(..prob..)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHdrBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      varwidth = varwidth,
      na.rm = na.rm,
      probs = prob,
      ...
    )
  )
}

#' @importFrom ggplot2 ggproto Geom
#' @export
GeomHdrBoxplot <- ggproto("GeomHdrBoxplot", Geom,

                       # need to declare `width` here in case this geom is used with a stat that
                       # doesn't have a `width` parameter (e.g., `stat_identity`).
                       extra_params = c("na.rm", "width"),

                       setup_data = function(data, params) {
                         data$width <- data$width %||%
                           params$width %||% (resolution(data$x, FALSE) * 0.9)

                         data$xmin <- data$x - data$width / 2
                         data$xmax <- data$x + data$width / 2

                         data$width <- NULL
                         data$outliers <- NULL
                         data
                       },

                       draw_group = function(data, panel_params, coord, varwidth = FALSE,
                                             prob = c(0.5, 0.95, 0.99)) {
                         ## if values passed to 'prob' are integers instead of
                         ## decimals, convert them to decimals
                         ## ex. 5 >>> 0.05, 95 >>> 0.95
                         if(any(prob > 100 | prob < 0)) {
                           stop(
                             "Probability values should not exceed 100 or be below 0. Please make sure the values are between 0 and 1.",
                             call. = FALSE
                           )
                         }

                         if(any(prob > 1)) {
                           warning(
                             "Probability values should be on a scale between 0 to 1. If not, values will be converted to decimal values.",
                             call. = FALSE
                           )
                           prob <- prob / 100
                         }

                         fill_shade <- darken_fill(rep_len(data$fill, length(data$prob[[1]])), data$prob[[1]])
                         common <- list(
                           colour = data$colour,
                           size = data$size,
                           linetype = data$linetype,
                           group = data$group,
                           alpha = NA
                         )

                         box <- tibble::as_tibble(c(
                           list(

                             xmin = data$xmin + 0.1* (data$xmax-data$xmin),
                             xmax = data$xmax - 0.1* (data$xmax-data$xmin),
                             ymin = data$ymin_real[[1]],
                             ymax = data$ymax_real[[1]],
                             fill = scales::alpha(fill_shade, data$alpha)
                           ),
                           common
                         ))

                         #mode <- transform(data, x = xmin, xend = xmax, yend = y, size = size , alpha = NA)
                         #browser()
                         mode <- tibble::as_tibble(c(
                           list(
                             x = data$xmin,
                             xend = data$xmax,
                             y = data$mode[[1]],
                             yend = data$mode[[1]]
                           ),
                           common
                         ), n = length(data$mode[[1]]))

                         ggplot2:::ggname("geom_hdr_boxplot", grid::grobTree(
                           ggplot2::GeomRect$draw_panel(box, panel_params, coord),
                           ggplot2::GeomSegment$draw_panel(mode, panel_params, coord)
                         ))
                       },

                       draw_key = draw_key_hdr_boxplot,

                       default_aes = aes(weight = 1, colour = "grey20", fill = "black", size = 0.5,
                                         alpha = NA, shape = 19, linetype = "solid", prob = NA),

                       required_aes = c("ymax", "ymin"),
                       optional_aes = "prob"
)
