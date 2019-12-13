library(ggplot2)
disp_hdr_boxplot <- ggplot(faithful, aes(y = eruptions)) +
  geom_hdr_boxplot(prob = c(0.5, 0.9))
vdiffr::expect_doppelganger("Basic geom_hdr_boxplot", disp_hdr_boxplot)
