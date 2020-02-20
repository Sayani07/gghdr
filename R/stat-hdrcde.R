#' @title stat_hdrcde
#' @description layer for hdrcde
#' @param geom PARAM_DESCRIPTION, Default: 'hdr_boxplot'
#' @param position PARAM_DESCRIPTION, Default: 'dodge2'
#' @param mapping Default: NULL
#' @param data Default: NULL
#' @param ... ...
#' @param coef Default: 1.5
#' @param na.rm Default: FALSE
#' @param show.legend Default: NA
#' @param inherit.aes Default: TRUE
#' @rdname stat_hdrcde
#' @export
stat_hdrcde <- function(mapping = NULL, data = NULL,
                        geom = "hdr_boxplot", position = "dodge2",
                        ...,
                        coef = 1.5,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHdrcde,
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

#' @title StatHdrcde
#' @description stat for hdrcde
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
#' @rdname StatHdrcde
#' @export

StatHdrcde <- ggproto("StatHdrcde", Stat,
  required_aes = c("y"),
  # non_missing_aes = "weight",

  setup_params = ggplot2::StatBoxplot$setup_params,

  setup_data = function(data, params) {
    # How are missing values handled?
    data$x <- data$x %||% 0
    data
  },

  compute_group = function(data, scales, width = NULL, probs = NULL,
                           all.modes = TRUE, na.rm = FALSE) {
    if (length(unique(data$x)) > 1) {
      width <- diff(range(data$x)) * 0.9
    }

    if (!all(data$x == data$x[1])) {
      stop("Conditional density estimation is not yet supported.
           Make sure each plot group contains only one x value.")
    }

    # imported from hdrcde
    hdr_stats <- hdrcde::hdr(data$y, prob = probs * 100, all.modes = all.modes)

    hdr <- hdr_stats$hdr

    # number of boxes (for all probabilities max number of boxes will be
    # shown although it has got NA values)
    max_boxes <- ncol(hdr) / 2

    # initialise 1 row data.frame
    df <- structure(list(), .Names = character(0), row.names = c(NA, -1L),
                    class = "data.frame")

    box <- split(hdr, col(hdr) %% 2)
    is_box <- complete.cases(box)
    df$prob <- list(rep(sort(probs, decreasing = TRUE), max_boxes)[is_box])
    df$box <- list(matrix(
      c(box[[2]], box[[1]]),
      ncol = 2,
      dimnames = list(NULL, c("lower", "upper"))
    )[is_box, ])
    df$ymax <- max(box[[1]], na.rm = TRUE)
    df$ymin <- min(box[[2]], na.rm = TRUE)

    # keep only modes within boxes
    mode_proxy <- matrix(0, length(hdr_stats$mode), df$ymax)
    for (i in seq_len(length(hdr_stats$mode)))
    {
      for (j in seq_len(length(df$ymax))) {
        if (hdr_stats$mode[i] <= df$ymax[j] & hdr_stats$mode[i] >= df$ymin[j]) {
          mode_proxy[i, j] <- 0
        }
        else {
          mode_proxy[i, j] <- 1
        }
      }
    }
    hdr_mode <- hdr_stats$mode[which(rowSums(mode_proxy) == 0)]
    df$mode <- list(hdr_mode)

    df$width <- width
    df$f_alpha <- list(hdr_stats$falpha)
    df$x <- unique(data$x) # FIX LATER
    df
  }
)
