#' @export
stat_hdr <- function(mapping = NULL, data = NULL,
                         geom = "hdr_boxplot", position = "dodge2",
                         ...,
                         coef = 1.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
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


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
#' @export
StatHdr <- ggproto("StatHdr", Stat,
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
                         df$prob <- list(rep(sort(probs, decreasing = TRUE), max_boxes))
                         df$box_num <- list(rep(seq_len(max_boxes), each = length(probs)))
                         df[c("ymax_real","ymin_real")] <- lapply(split(hdr, col(hdr) %% 2), list)
                         df$ymax <- vapply(df$ymax_real, max, double(1L), na.rm = TRUE)
                         df$ymin <- vapply(df$ymin_real, min, double(1L), na.rm = TRUE)
                         df$width <- width

# manipulation to make sure that no mode should lie outside the box
                         mode_proxy <- matrix(0, length(hdr_stats$mode), df$ymax)
                         for(i in 1:length(hdr_stats$mode))
                         {
                           for(j in 1:length(df$ymax))
                           if(hdr_stats$mode[i]<=df$ymax[j] & hdr_stats$mode[i]>= df$ymin[j])
                           {
                            mode_proxy[i, j] <- 0
                           }
                           else mode_proxy[i, j] <- 1
                         }
                         hdr_mode <- hdr_stats$mode[which(rowSums(mode_proxy)==0)]
                         df$mode <- list(hdr_mode)

                         df$f_alpha <- list(hdr_stats$falpha)
                         df$x <- unique(data$x) # FIX LATER
                         df
                       }
)
