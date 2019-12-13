#' @title geom_hdr_rug
#' @description rug visualization for HDR plot
#' @param varwidth width, Default: FALSE
#' @param prob PARAM_DESCRIPTION, Default: c(0.5, 0.95, 0.99)
#' @return geom_hdr_rug
#' @rdname geom_hdr_rug
#' @examples
#' library(ggplot2)
#'
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
#' @description ggproto object for geom_hrdr_rug
#' @rdname GeomHdrRug
#' @importFrom ggplot2 ggproto Geom
#' @importFrom grid segmentsGrob
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

                            sides <- "btlr"
                            rugs <- list()

                            fill_shade <- darken_fill(rep_len(data$fill, length(data$prob[[1]])), data$prob[[1]])
                            gp <- gpar(col = alpha(data$colour, data$alpha), fill = fill_shade,
                                       lty = data$linetype, lwd = data$size * .pt)
                            if (!is.null(data$box_x)) {
                              box <- data$box_x[[1]]
                              box <- coord$transform(data.frame(xmin = box[,"lower"], xmax = box[,"upper"]), panel_params)
                              if (grepl("b", sides)) {
                                rugs$x_b <- rectGrob(
                                  x = box$xmin, width = box$xmax - box$xmin,
                                  y = rep(unit(0, "npc"), nrow(box)), height = rep(unit(0.03, "npc"), nrow(box)),
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )
                              }

                              if (grepl("t", sides)) {
                                rugs$x_t <- rectGrob(
                                  x = box$xmin, width = box$xmax - box$xmin,
                                  y = rep(unit(0.97, "npc"), nrow(box)), height = rep(unit(0.03, "npc"), nrow(box)),
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )
                              }
                            }

                            if (!is.null(data$box_y)) {
                              box <- data$box_y[[1]]
                              box <- coord$transform(data.frame(ymin = box[,"lower"], ymax = box[,"upper"]), panel_params)
                              if (grepl("l", sides)) {
                                rugs$y_l <- rectGrob(
                                  x = rep(unit(0.0, "npc"), nrow(box)), width = rep(unit(0.03, "npc"), nrow(box)),
                                  y = box$ymin, height = box$ymax-box$ymin,
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )
                              }

                              if (grepl("r", sides)) {
                                rugs$y_r <- rectGrob(
                                  x = rep(unit(0.97, "npc"), nrow(box)), width = rep(unit(0.03, "npc"), nrow(box)),
                                  y = box$ymin, height = box$ymax-box$ymin,
                                  just = c(0,0),
                                  gp = gp,
                                  default.units = "native"
                                )
                              }
                            }

                            ggplot2:::ggname("geom_hdr_rug", do.call(grid::grobTree, rugs))
                          },

                          draw_key = draw_key_hdr_boxplot,

                          default_aes = aes(weight = 1, colour = "grey20", fill = "black", size = 0.5,
                                            alpha = NA, shape = 19, linetype = "solid", prob = NA),

                          optional_aes = c("x", "y", "prob")
)
