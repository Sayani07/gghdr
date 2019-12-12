library(ggplot2)

context("hdr-box-plot")

test_that("hdr box plot draws correctly", {

  hdr_boxplot <- ggplot(faithful, aes(y = eruptions)) +
    geom_hdr_boxplot(prob = c(0.5, 0.9)) +
    theme_test()
  vdiffr::expect_doppelganger("geom_hdr_boxplot", hdr_boxplot)

  expect_equal(hdr_boxplot, ggplot(faithful, aes(y = eruptions)) +
                 geom_hdr_boxplot(prob = c(0.5, 0.9)) +
                 theme_test())
})

context("Histograms")

test_that("histogram test", {

  disp_hist_base <- function() hist(mtcars$disp)
  disp_hist_ggplot <- ggplot(mtcars, aes(disp)) +
    geom_histogram() +
    theme_minimal()

  vdiffr::expect_doppelganger("Base graphics histogram", disp_hist_base)
  vdiffr::expect_doppelganger("ggplot2 histogram", disp_hist_ggplot)

})
