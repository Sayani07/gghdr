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
                       setup_data = function(data, params) {
                         # How are missing values handled?
                         data
                       },

                       compute_group = function(data, scales, width = NULL, prob = NULL, all.modes = TRUE, na.rm = FALSE) {
                         # ???
                         #browser()

                         # imported from hdrcde
                         hdr_stats <- hdrcde::hdr(data$y, prob = prob*100, all.modes = all.modes)

                         hdr <- hdr_stats$hdr

                         # number of boxes (for all probabilities max number of boxes will be shown although it has got NA values)
                         max_boxes <- ncol(hdr)/2

                         df <- as.data.frame(c(
                           # repitition of probs through the length of cutoff vectors
                           box_probs = list(rep(sort(prob, decreasing = TRUE), max_boxes)),
                           # tagging the boxes through the length of cutoff vectors
                           box_num = list(rep(seq_len(max_boxes), each = length(prob))),
                           # splitting the hdr into lower and upper cutoffs vector
                           split(hdr, ifelse(col(hdr) %% 2 == 0, "ymax", "ymin"))
                         ))

                         df <- df[!is.na(df$ymin),]

                         mode <- rep(hdr_stats$mode, each = length(prob))

                         #df_output <- list(df = df, mode = hdr_stats$mode)
                         df_output <- cbind(df, mode)
                       }
)
