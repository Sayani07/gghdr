context("hdr_bin")

library(ggplot2)

test_that("hdr_bin outputting right stuff", {
  faithful_binned <- hdr_bin(x = faithful$waiting, y = faithful$eruptions)

  expect_equal(length(faithful_binned), 272)

  expect_equal(class(faithful_binned), "factor")

  expect_equal(length(levels(faithful_binned)), 4)

  expect_equal(as.character(faithful_binned[[5]]), "50%")

  ## different probability values
  faithful_binned <- hdr_bin(
    x = faithful$waiting, y = faithful$eruptions,
    prob = c(0.12, 0.6, 0.84)
  )

  expect_equal(length(faithful_binned), 272)

  expect_equal(class(faithful_binned), "factor")

  expect_equal(length(levels(faithful_binned)), 4)

  expect_equal(as.character(faithful_binned[[5]]), "60%")

  ## With plot
  set.seed(5)
  binned_plot <- ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
    geom_point(aes(colour = hdr_bin(x = waiting, y = eruptions)))
  vdiffr::expect_doppelganger("binned plot", binned_plot)

  binned_plot2 <- ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
    geom_point(aes(colour = hdr_bin(
      x = waiting, y = eruptions,
      prob = c(0.12, 0.6, 0.84)
    )))
  vdiffr::expect_doppelganger("binned plot 2", binned_plot2)
})
