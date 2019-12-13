#' @title Binning highest density regions in one or two dimensions
#' @param x Numeric vector
#' @param y Numeric vector of same length as x.
#' @param prob Probability coverage required for HDRs
#' @param ... ...
#' @return probability coverage for each element of the numeric vectors.
#' @export
hdr_bin <- function(x, y, prob = c(0.5, 0.9, 0.99), ...){
  hdr_x <- hdrcde::hdr(x, prob = prob*100)
  hdr_box <- hdr_boxes((hdr_x))
  prob <- rep(sort(prob, decreasing = TRUE), length.out = nrow(hdr_box))
  df <- cbind(hdr_box, prob)[order(prob),]

  within_box <- function(x, df){
    for (box in split(df, row(df))){
      if(x >= box[1] && x <= box[2])
        return(scales::percent(box[3]))
    }
    return(paste0(">", scales::percent(max(df[,3]))))
  }

  vapply(x, within_box, character(1L), df=df)
}


