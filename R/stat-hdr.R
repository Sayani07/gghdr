#' @title stat_hdrcde
#' @description stat for hdrcde
#' @param geom PARAM_DESCRIPTION, Default: 'hdr_boxplot'
#' @param position PARAM_DESCRIPTION, Default: 'dodge2'
#' @rdname stat_hdrcde
#' @export
#' @importFrom ggplot2 layer

stat_hdrcde <- function(mapping = NULL, data = NULL,
                         geom = "hdr_boxplot", position = "dodge2",
                         ...,
                         coef = 1.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
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


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
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

                       compute_group = function(data, scales, width = NULL, probs = NULL, all.modes = TRUE, na.rm = FALSE) {
                         if (length(unique(data$x)) > 1)
                           width <- diff(range(data$x)) * 0.9

                         if(!all(data$x == data$x[1])){
                           stop("Conditional density estimation is not yet supported. Make sure each plot group contains only one x value.")
                         }

                         # imported from hdrcde
                         hdr_stats <- hdrcde::hdr(data$y, prob = probs*100, all.modes = all.modes)

                         hdr <- hdr_stats$hdr

                         # number of boxes (for all probabilities max number of boxes will be shown although it has got NA values)
                         max_boxes <- ncol(hdr)/2

                         # initialise 1 row data.frame
                         df <- structure(list(), .Names = character(0), row.names = c(NA, -1L), class = "data.frame")

                         # each box showing a mode
                         # earlier we had one mode or multiple modes


                         # den <- density(data$y, bw = hdrbw(data$y, mean(prob)), n=1001)
                         # box = array(NA, nrow(df))
                         # for(i in 1: nrow(df)) {
                         #   box <- den$x[i]#dplyr::filter()
                         #
                         #   den$x[df$ymin<=den$x[i] & df$ymax>=den$x[i]]
                         #
                         #
                         #   n <- length(box[i])
                         #   y <- c(0, den$y)
                         #   idx <- ((y[2:(n-1)] > y[1:(n-2)]) & (y[2:(n-1)] > y[3:n])) | (den$y == max(den$y))
                         #   mode[i] <- den$x[idx]
                         # }
                         #
                         # # which(hdr_stats$mode==den$x)
                         # df$mode <- array()
                         #
                         # if (length(hdr_stats$mode) == 1) {
                         #   unimode <- hdr_stats$mode
                         #   #idx_lower <- min(which(unimode > df$ymin))
                         #
                         #   idx <- min(which(unimode > df$ymin))
                         #   df$mode <- df$mode[(idx*(length(probs)) - 0:2),]
                         # }
                         # else{
                         #   mode <- rep(hdr_stats$mode, each = length(prob))
                         # }

                         box <- split(hdr, col(hdr) %% 2)
                         is_box <- complete.cases(box)
                         df$prob <- list(rep(sort(probs, decreasing = TRUE), max_boxes)[is_box])
                         df$box <- list(matrix(
                           c(box[[2]], box[[1]]), ncol = 2,
                           dimnames = list(NULL, c("lower", "upper"))
                         )[is_box,])
                         df$ymax <- max(box[[1]], na.rm = TRUE)
                         df$ymin <- min(box[[2]], na.rm = TRUE)
                         df$mode <- list(hdr_stats$mode)
                         df$width <- width
                         df$f_alpha <- list(hdr_stats$falpha)
                         df$x <- unique(data$x) # FIX LATER
                         df
                       }
)
