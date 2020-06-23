#' @title Binning highest density regions in one or two dimensions
#' @param x Numeric vector
#' @param y Numeric vector of same length as x.
#' @param prob Probability coverage required for HDRs
#' @param ... ...
#' @return probability coverage for each element of the numeric vectors.
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
#'   geom_point(aes(colour = hdr_bin(x = waiting, y = eruptions)))
#' @export

hdr_bin <- function(x, y = NULL, prob = c(0.5, 0.9, 0.99), ...) {
  if (is.null(y)) {
    hdr_x <- hdrcde::hdr(x, prob = prob * 100, ...)
    hdr_box <- hdr_boxes((hdr_x))
    prob <- rep(sort(prob, decreasing = TRUE), length.out = nrow(hdr_box))
    df <- cbind(hdr_box, prob)[order(prob), ]

    within_box <- function(x, df) {
      for (box in split(df, row(df))) {
        if (x >= box[1] && x <= box[2]) {
          return(scales::percent(box[3]))
        }
      }
      return(paste0(">", scales::percent(max(df[, 3]))))
    }

    vapply(x, within_box, character(1L), df = df)
  }
  else {
    prob <- sort(prob)
    hdr_xy <- hdrcde::hdr.2d(x, y, prob = prob * 100, ...)
    region <- rep(-1, length(x))
    for (i in seq_along(prob)) {
      region[hdr_xy$fxy > hdr_xy$falpha[i]] <- rev(prob)[i]
    }
    factor(region,
      levels = c(prob, -1),
      labels = c(
        scales::percent(prob, 1),
        paste0(">", scales::percent(max(prob), 1))
      )
    )
  }
}
