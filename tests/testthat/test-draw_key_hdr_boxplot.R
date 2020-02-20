# Setting of legend key glyphs has to be tested visually
context("Legend key glyphs")
library(ggplot2)
test_that("alternative key glyphs work", {
  set.seed(5)
  hdr_boxplot <- ggplot(faithful, aes(y = eruptions)) +
    geom_hdr_boxplot(key_glyph = draw_key_hdr_boxplot) +
    theme_bw()

  # specify key glyph by function
  vdiffr::expect_doppelganger(
    "hdr key glyphs",
    hdr_boxplot
  )
})
