# #' @title prob_range
# #' @description proto object for probability range
#' @importFrom ggplot2 ggproto
prob_range <- function() {
  ggplot2::ggproto(NULL, RangeProb)
}

RangeProb <- ggplot2::ggproto(NULL, NULL,
  range = NULL,
  levels = NULL,
  reset = function(self) {
    self$range <- NULL
    self$levels <- NULL
  },
  train = function(self, x, ...) {
    self$range <- scales::train_continuous(x[[1]], self$range)
    self$probs <- unique(c(x[[1]][!is.na(x[[1]])], self$range))
  }
)
