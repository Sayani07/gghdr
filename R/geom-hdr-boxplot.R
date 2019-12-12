# is this the right function name?
#' @importFrom ggplot2 layer aes
#' @export
geom_hdr_boxplot <- function(mapping = NULL, data = NULL,
                             stat = "boxplot", position = "dodge2",
                             ...,
                             varwidth = FALSE, # do we want this?
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             prob = c(0.5, 0.95, 0.99)) {

  # Add basic input checks if needed

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
      prob = prob,
      ...
    )
  )
}

#' @importFrom ggplot2 ggproto Geom
#' @export
GeomHdrBoxplot <- ggproto("GeomBoxplot", Geom,

                       # If we're doing custom width, we need this:
                       # need to declare `width` here in case this geom is used with a stat that
                       # doesn't have a `width` parameter (e.g., `stat_identity`).
                       extra_params = c("na.rm", "width"),

                       setup_data = function(data, params) {
                         #
                         data
                       },

                       draw_group = function(data, panel_params, coord, fatten = 2,
                                             notch = FALSE, notchwidth = 0.5, varwidth = FALSE,
                                             prob = c(0.5, 0.95, 0.99)) {

                         # this may occur when using geom_boxplot(stat = "identity")
                         if (nrow(data) != 1) {
                           stop(
                             "Can't draw more than one boxplot per group. Did you forget aes(group = ...)?",
                             call. = FALSE
                           )
                         }

                         common <- list(
                           colour = data$colour,
                           size = data$size,
                           linetype = data$linetype,
                           fill = alpha(data$fill, data$alpha),
                           group = data$group
                         )

                         whiskers <- vctrs::new_data_frame(c(
                           list(
                             x = c(data$x, data$x),
                             xend = c(data$x, data$x),
                             y = c(data$upper, data$lower),
                             yend = c(data$ymax, data$ymin),
                             alpha = c(NA_real_, NA_real_)
                           ),
                           common
                         ), n = 2L)

                         box <- vctrs::new_data_frame(c(
                           list(
                             xmin = data$xmin,
                             xmax = data$xmax,
                             ymin = data$lower,
                             y = data$middle,
                             ymax = data$upper,
                             ynotchlower = ifelse(notch, data$notchlower, NA),
                             ynotchupper = ifelse(notch, data$notchupper, NA),
                             notchwidth = notchwidth,
                             alpha = data$alpha
                           ),
                           common
                         ))

                         if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                           outliers <- vctrs::new_data_frame(list(
                             y = data$outliers[[1]],
                             x = data$x[1],
                             colour = outlier.colour %||% data$colour[1],
                             fill = outlier.fill %||% data$fill[1],
                             shape = outlier.shape %||% data$shape[1],
                             size = outlier.size %||% data$size[1],
                             stroke = outlier.stroke %||% data$stroke[1],
                             fill = NA,
                             alpha = outlier.alpha %||% data$alpha[1]
                           ), n = length(data$outliers[[1]]))
                           outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
                         } else {
                           outliers_grob <- NULL
                         }

                         ggname("geom_boxplot", grobTree(
                           #outliers_grob,
                           #ggplot2::GeomSegment$draw_panel(whiskers, panel_params, coord),
                           ggplot2::GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
                         ))
                       },

                       draw_key = ggplot2::draw_key_boxplot,

                       default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                         alpha = NA, shape = 19, linetype = "solid"),

                       required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)
