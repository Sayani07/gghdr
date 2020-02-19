#' @title stat_hdrc (rug)
#' @description stat for hdr (rug)
#' @param geom PARAM_DESCRIPTION, Default: 'hdr_boxplot'
#' @param position PARAM_DESCRIPTION, Default: 'dodge2'
#' @param mapping Default: NULL
#' @param data Default: NULL
#' @param ... ...
#' @param coef Default: 1.5
#' @param na.rm Default: FALSE
#' @param show.legend Default: NA
#' @param inherit.aes Default: TRUE
#' @details
#' Hyndman's (1996) density quantile algorithm is used for calculation.
#' @references Hyndman, R.J. (1996) Computing and graphing highest density
#' regions. \emph{American Statistician}, \bold{50}, 120-126.
#'
#' Samworth, R.J. and Wand, M.P. (2010). Asymptotics and optimal bandwidth
#' selection for highest density region estimation.  \emph{The Annals of
#' Statistics}, \bold{38}, 1767-1792.
#'
#' Wand, M.P., Marron, J S., Ruppert, D. (1991) Transformations in density
#' estimation. \emph{Journal of the American Statistical Association},
#' \bold{86}, 343-353.
#' @rdname stat_hdr
#' @export

stat_hdr <- function(mapping = NULL, data = NULL,
                     geom = "hdr_rug", position = "dodge2",
                     ...,
                     coef = 1.5,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatHdr,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
}

#' @title stat_hdr
#' @description stat for hdr
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
#' @rdname StatHdr
#' @export

StatHdr <- ggproto("StatHdr", Stat,
                       optional_aes = c("x", "y"),
                       # non_missing_aes = "weight",

                       # setup_params = ggplot2::StatBoxplot$setup_params,

                       setup_data = function(data, params) {
                         # How are missing values handled?
                         data
                       },

                       compute_group = function(data, scales, width = NULL, probs = NULL, all.modes = TRUE, na.rm = FALSE) {
                         # initialise 1 row data.frame
                         df <- structure(list(), .Names = character(0), row.names = c(NA, -1L), class = "data.frame")

                         has_x <- !is.null(data$x)
                         has_y <- !is.null(data$y)
                         # imported from hdrcde
                         if (has_x) {
                           hdr_x <- hdrcde::hdr(data$x, prob = probs*100, all.modes = all.modes)
                           df$box_x <- list(hdr_boxes(hdr_x))
                           df$mode_x <- list(hdr_x$mode)
                           df$f_alpha_x <- list(hdr_x$falpha)
                           df$xmax <- max(c(df$box_x[[1]][,"upper"]), na.rm = TRUE)
                           df$xmin <- min(c(df$box_x[[1]][,"lower"]), na.rm = TRUE)
                         }
                         if (has_y) {
                           hdr_y <- hdrcde::hdr(data$y, prob = probs*100, all.modes = all.modes)
                           df$box_y <- list(hdr_boxes(hdr_y))
                           df$mode_y <- list(hdr_y$mode)
                           df$f_alpha_y <- list(hdr_y$falpha)
                           df$ymax <- max(c(df$box_y[[1]][,"upper"]), na.rm = TRUE)
                           df$ymin <- min(c(df$box_y[[1]][,"lower"]), na.rm = TRUE)
                         }


                         df$prob <- list(sort(probs, decreasing = TRUE))
                         df$width <- width
                         df
                       }
)
