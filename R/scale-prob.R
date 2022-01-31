#' Probability colour scales
#'
#' This set of scales defines new scales for prob geometries equivalent to the
#' ones already defined by ggplot2. This allows the shade of confidence
#' intervals
#' to work with the legend output.
#' @return A ggproto object inheriting from `[ggplot2::Scale]`
#' @family scale_prob_*
#' @name scale_prob
#' @inheritParams ggplot2::scale_colour_gradient
#' @export
scale_prob_identity <- function(..., guide = "prob") {
  prob_scale("prob", "identity", identity, guide = guide, ...)
}

ScaleProb <- ggplot2::ggproto(NULL, ggplot2::ScaleDiscrete, map = identity)

#' @importFrom ggplot2 waiver discrete_scale
prob_scale <- function(...) {
  scale <- ggplot2::discrete_scale(..., super = ScaleProb)
  scale$range <- prob_range()
  scale
}

#' @title Probability shade bar guide
#' @description The guide shows the colour scales mapped to different probability coverage required for HDRs
#' @inheritParams ggplot2::guide_colourbar
#' @param ... Further arguments passed onto either
#' \code{\link[ggplot2]{guide_colourbar}} or \code{\link[ggplot2]{guide_legend}}
#' @return A guide object
#' @export
guide_prob <- function(title = waiver(), ...) {
  structure(list(
    title = title,
    available_aes = "prob",
    args = list(...)
  ),
  class = c("guide", "prob_guide")
  )
}

#' @title Helper methods for guides
#' @export
#' @rdname guide-helpers
#' @importFrom ggplot2 guide_colourbar guide_train guide_legend
#' @importFrom digest digest
#' @keywords internal
#' @return A guide object

guide_train.prob_guide <- function(guide, scale, aesthetic) {
  args <- append(guide[!(names(guide) %in% c("args"))], guide$args)
  probs <- scale$range$probs
  if (length(probs) == 0 || all(is.na(probs))) {
    return()
  }
  guide <- do.call("guide_legend", args)
  class(guide) <- c("guide", "guide_prob")
  breaks <- probs

  breaks_mapped <- darken_fill(rep("black", length(breaks)), breaks)
  key <- as.data.frame(stats::setNames(list(breaks_mapped), aesthetic %||%
    scale$aesthetics[1]), stringsAsFactors = FALSE)
  key$.label <- scales::percent(breaks) # scale$get_labels(breaks)
  if (!scale$is_discrete()) {
    limits <- scale$get_limits()
    noob <- !is.na(breaks) & limits[1] <= breaks & breaks <=
      limits[2]
    key <- key[noob, , drop = FALSE]
  }
  if (guide$reverse) {
    key <- key[nrow(key):1, ]
  }
  guide$key <- key
  guide$hash <- with(guide, digest::digest(list(
    title, key$.label,
    direction, name
  )))
  guide
}

#' @export
#' @importFrom ggplot2 guide_geom guide_legend
#' @rdname guide-helpers
guide_geom.guide_prob <- function(guide, layers, default_mapping) {
  class(guide) <- c("guide", "legend")
  guide <- guide_geom(guide, layers, default_mapping)
  guide$geoms <- lapply(guide$geoms, function(x) {
    x$draw_key <- ggplot2::ggproto(NULL, NULL,
      draw_key = function(data, params, size) {
        lwd <- min(data$size, min(size) / 4)
        fillcol <- data$prob
        grid::rectGrob(
          width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
          height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
          gp = grid::gpar(
            col = fillcol,
            fill = scales::alpha(fillcol, data$alpha),
            lty = data$linetype,
            lwd = lwd * ggplot2::.pt,
            linejoin = "mitre"
          )
        )
      }
    )$draw_key
    x
  })
  guide
}
