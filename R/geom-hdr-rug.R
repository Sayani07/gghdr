#' @title Rug plot for the highest density region
#' @description rug visualization for HDR plot
#' @param rug_width width of rugs, Default: unit(0.03, "npc")
#' @param sides position to put rugs
#' @param prob PARAM_DESCRIPTION, Default: c(0.5, 0.95, 0.99)
#' @param mapping Default: NULL
#' @param ... ...
#' @param na.rm Default: FALSE
#' @param show.legend Default: NA
#' @param inherit.aes Default: TRUE
#' @param data data
#' @param stat stat
#' @param position Default: "identity"
#' @return Calculates and plots the rug plot of highest density regions.
#' @rdname geom_hdr_rug
#' @examples
#' library(ggplot2)
#'
#' ggplot(faithful, aes(x = waiting, y = eruptions)) +
#'   geom_point() +
#'   geom_hdr_rug()
#' @export
#' @importFrom ggplot2 aes layer

geom_hdr_rug <- function(mapping = NULL, data = NULL,
                         stat = "hdr", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         sides = "bl",
                         rug_width = unit(0.03, "npc"),
                         prob = c(0.5, 0.95, 0.99)) {

  # Add basic input checks if needed

  # Add basic input checks if needed
  ## if values passed to 'prob' are integers instead of
  ## decimals, convert them to decimals
  ## ex. 5 >>> 0.05, 95 >>> 0.95
  if (any(prob > 100 | prob < 0)) {
    stop(
      "Probability values should not exceed 100 or be below 0.
      Please make sure the values are between 0 and 1.",
      call. = FALSE
    )
  }

  if (any(prob > 1)) {
    warning(
      "Probability values should be on a scale between 0 to 1.
      If not, values will be converted to decimal values.",
      call. = FALSE
    )
    prob <- prob / 100
  }

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
      na.rm = na.rm,
      sides = sides,
      rug_width = rug_width,
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
#' @keywords internal
GeomHdrRug <- ggproto("GeomHdrRug", Geom,

  # If we're doing custom width, we need this:
  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width"),

  setup_data = function(data, params) {
    data
  },

  draw_group = function(data, panel_params, coord,
                        sides = sides,
                        rug_width = rug_width,
                        prob = c(0.5, 0.95, 0.99)) {
    if (!inherits(rug_width, "unit")) {
      stop("'length' must be a 'unit' object.", call. = FALSE)
    }
    rugs <- list()

    fill_shade <- darken_fill(
      rep_len(data$fill, length(data$prob[[1]])),
      data$prob[[1]]
    )
    gp <- gpar(
      col = NA, fill = alpha(fill_shade, data$alpha),
      lty = data$linetype, lwd = data$size * .pt
    )

    line_gp <- gpar(
      col = data$colour,
      lty = data$linetype,
      lwd = data$size * .pt,
      lineend = "butt"
    )
    if (!is.null(data$box_x)) {
      box <- data$box_x[[1]]
      box <- coord$transform(
        data.frame(
          xmin = box[, "lower"],
          xmax = box[, "upper"]
        ),
        panel_params
      )

      modes <- coord$transform(data.frame(x = data$mode_x[[1]]), panel_params)

      if (grepl("b", sides)) {
        rugs$x_b <- grid::gTree(
          children = grid::gList(
            rectGrob(
              x = box$xmin, width = box$xmax - box$xmin,
              y = rep(unit(0, "npc"), nrow(box)),
              height = rep(rug_width, nrow(box)),
              just = c(0, 0),
              gp = gp,
              default.units = "native"
            ),
            segmentsGrob(
              x0 = modes$x,
              x1 = modes$x,
              y0 = rep(unit(0, "npc"), nrow(modes)),
              y1 = rep(rug_width, nrow(modes)),
              gp = line_gp
            )
          )
        )
      }

      if (grepl("t", sides)) {
        rugs$x_t <- grid::gTree(
          children = grid::gList(
            rectGrob(
              x = box$xmin, width = box$xmax - box$xmin,
              y = rep(unit(1, "npc") - rug_width, nrow(box)),
              height = rep(rug_width, nrow(box)),
              just = c(0, 0),
              gp = gp,
              default.units = "native"
            ),
            segmentsGrob(
              x0 = modes$x,
              x1 = modes$x,
              y0 = rep(unit(1, "npc"), nrow(modes)),
              y1 = rep(unit(1, "npc") - rug_width, nrow(modes)),
              gp = line_gp
            )
          )
        )
      }
    }

    if (!is.null(data$box_y)) {
      box <- data$box_y[[1]]
      box <- coord$transform(data.frame(
        ymin = box[, "lower"],
        ymax = box[, "upper"]
      ), panel_params)

      modes <- coord$transform(data.frame(y = data$mode_y[[1]]), panel_params)

      if (grepl("l", sides)) {
        rugs$y_l <- grid::gTree(
          children = grid::gList(
            rectGrob(
              x = rep(unit(0.0, "npc"), nrow(box)),
              width = rep(rug_width, nrow(box)),
              y = box$ymin, height = box$ymax - box$ymin,
              just = c(0, 0),
              gp = gp,
              default.units = "native"
            ),
            segmentsGrob(
              x0 = rep(unit(0.0, "npc"), nrow(modes)),
              x1 = rep(rug_width, nrow(modes)),
              y0 = modes$y,
              y1 = modes$y,
              gp = line_gp
            )
          )
        )
      }

      if (grepl("r", sides)) {
        rugs$y_r <- grid::gTree(
          children = grid::gList(
            rectGrob(
              x = rep(unit(1, "npc") - rug_width, nrow(box)),
              width = rep(rug_width, nrow(box)),
              y = box$ymin, height = box$ymax - box$ymin,
              just = c(0, 0),
              gp = gp,
              default.units = "native"
            ),
            segmentsGrob(
              x0 = rep(unit(1, "npc"), nrow(modes)),
              x1 = rep(unit(1, "npc") - rug_width, nrow(modes)),
              y0 = modes$y,
              y1 = modes$y,
              gp = line_gp
            )
          )
        )
      }
    }

    ggplot2:::ggname("geom_hdr_rug", do.call(grid::grobTree, rugs))
  },

  draw_key = draw_key_hdr_boxplot,

  default_aes = aes(
    weight = 1, colour = "grey20", fill = "black", size = 0.5,
    alpha = NA, shape = 19, linetype = "solid", prob = NA
  ),

  optional_aes = c("x", "y", "prob")
)
