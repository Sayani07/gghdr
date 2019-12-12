library(ggplot2)
ggplot(faithful, aes(x = eruptions)) +
  stat_density(geom = "line") + xlim(0, 6)

ggplot(faithful, aes(x = eruptions)) +
  geom_hdr_rug()

