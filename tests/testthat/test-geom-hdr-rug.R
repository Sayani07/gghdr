test_that("hdr rug plot draws correctly", {

  hdr_rugplot <- ggplot(faithful, aes(x = eruptions)) +
    geom_hdr_rug() +
    theme_bw()

  vdiffr::expect_doppelganger("hdr rugplot",
                              hdr_rugplot)
})
