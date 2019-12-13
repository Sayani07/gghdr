# Setting of legend key glyphs has to be tested visually
context("Legend key glyphs")


test_that("alternative key glyphs work", {
  # specify key glyph by name
  expect_doppelganger("hdr boxplot key glyphs",
                      ggplot(df, aes(x, y)) +
                        geom_hdr_boxplot(key_glyph = "timeseries")
  )

  # specify key glyph by function
  expect_doppelganger("rectangle and dotplot key glyphs",
                      ggplot(df, aes(x, y)) +
                        geom_line(aes(color = "line"), key_glyph = draw_key_rect) +
                        geom_point(aes(fill = z), pch = 21, size = 3, stroke = 2, key_glyph = draw_key_dotplot)
  )
})
