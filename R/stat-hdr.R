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
#' @export
StatHdr <- ggproto("StatHdr", Stat,
                       required_aes = c("y"),
                       # non_missing_aes = "weight",
                       setup_data = function(data, params) {
                         # How are missing values handled?
                         data
                       },

                       compute_group = function(data, scales, width = NULL, na.rm = FALSE) {
                         # ???
                       }
)
