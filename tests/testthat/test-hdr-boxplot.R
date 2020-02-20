context("hdr-box-plot")
library(ggplot2)
set.seed(5)
test_that("hdr box plot draws correctly", {
  hdr_boxplot <- ggplot(faithful, aes(y = eruptions)) +
    geom_hdr_boxplot() +
    theme_bw()

  vdiffr::expect_doppelganger(
    "hdr boxplot",
    hdr_boxplot
  )
})
