#' @title geom_hdr_rug
#' @description rug visualization for HDR plot
#' @param varwidth width, Default: FALSE
#' @param prob PARAM_DESCRIPTION, Default: c(0.5, 0.95, 0.99)
#' @return geom_hdr_rug
#' @rdname geom_hdr_rug
#' @examples
#' ggplot(faithful, aes(y = eruptions)) +
#'   geom_hdr_rug()
#' @export
#' @importFrom ggplot2 aes layer

geom_hdr_rug <- function(mapping = NULL, data = NULL,
                             stat = "hdr", position = "identity",
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
    geom = GeomHdrRug,
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

#' @title GeomHdrRug
#' @rdname GeomHdrRug
#' @importFrom ggplot2 ggproto Geom
#' @export
GeomHdrRug <- ggproto("GeomHdrRug", Geom,

                          # If we're doing custom width, we need this:
                          # need to declare `width` here in case this geom is used with a stat that
                          # doesn't have a `width` parameter (e.g., `stat_identity`).
                          extra_params = c("na.rm", "width"),

                          setup_data = function(data, params) {
                            data
                          },

                          draw_group = function(data, panel_params, coord, varwidth = FALSE,
                                                prob = c(0.5, 0.95, 0.99)) {

                            sides <- "bl"
                            rugs <- list()

                            # box <- tibble::as_tibble(c(
                            #   list(
                            #
                            #     xmin = unit(0,"npc"),
                            #     xmax = unit(0.03, "npc"),
                            #     ymin = data$box_y[[1]][,"lower"],
                            #     ymax = data$box_y[[1]][,"upper"],
                            #     fill = scales::alpha(rep(fill_shade, length.out = nrow(data$box_y[[1]])), data$alpha),
                            #     colour = NA
                            #   ),
                            #   common
                            # ))
                            #
                            # mode <- tibble::as_tibble(c(
                            #   list(
                            #     x = unit(0,"npc"),
                            #     xend = unit(0.03, "npc"),
                            #     y = data$mode[[1]],
                            #     yend = data$mode[[1]],
                            #     colour = data$colour
                            #   ),
                            #   common
                            # ), n = length(data$mode[[1]]))

                            fill_shade <- darken_fill(rep_len(data$fill, length(data$prob[[1]])), data$prob[[1]])
                            gp <- gpar(col = alpha(data$colour, data$alpha), fill = fill_shade,
                                       lty = data$linetype, lwd = data$size * .pt)
                            if (!is.null(data$box_x)) {
                              box <- data$box_x[[1]]
                              browser()
                              width <- box[,"upper"] - box[,"lower"]
                              if (grepl("b", sides)) {
                                rugs$x_b <- rectGrob(
                                  x = box[,"lower"], width = box[,"upper"]-box[,"lower"],
                                  y = rep(unit(0.0, "npc"), nrow(box)), height = rep(unit(0.03, "npc"), nrow(box)),
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )

                                # rugs$x_b <- segmentsGrob(
                                #   x0 = unit(box[,"lower"], "native"), x1 = unit(box[,"upper"], "native"),
                                #   y0 = unit(0, "npc"), y1 = unit(0.03, "npc"),
                                #   gp = gp
                                # )
                              }
                              #
                              # if (grepl("t", sides)) {
                              #   rugs$x_t <- segmentsGrob(
                              #     x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
                              #     y0 = unit(1, "npc"), y1 = rug_length$max,
                              #     gp = gp
                              #   )
                              # }
                            }

                            if (!is.null(data$box_y)) {
                              box <- data$box_y[[1]]
                              width <- box[,"upper"] - box[,"lower"]
                              if (grepl("l", sides)) {
                                rugs$y_l <- rectGrob(
                                  x = rep(unit(0.0, "npc"), nrow(box)), width = rep(unit(0.03, "npc"), nrow(box)),
                                  y = box[,"lower"], height = box[,"upper"]-box[,"lower"],
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )
                              }

                              # if (grepl("r", sides)) {
                              #   rugs$y_r <- segmentsGrob(
                              #     y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
                              #     x0 = unit(1, "npc"), x1 = rug_length$max,
                              #     gp = gp
                              #   )
                              # }
                            }




                            ggplot2:::ggname("geom_hdr_rug", grid::grobTree(
                              rugs$x_b, rugs$y_l
                            ))
                          },

                          draw_key = draw_key_hdr_boxplot,

                          default_aes = aes(weight = 1, colour = "grey20", fill = "black", size = 0.5,
                                            alpha = NA, shape = 19, linetype = "solid", prob = NA),

                          optional_aes = c("x", "y", "prob")
)
