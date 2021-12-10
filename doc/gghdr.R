## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message =  FALSE
)

## ----prework------------------------------------------------------------------
library(ggplot2)
library(hdrcde)
# we will set the ggplot2 theme to use theme_minimal() for the vignette
theme_set(theme_minimal())
ggplot(data = faithful, aes(y = eruptions)) + 
  geom_boxplot() 
# fixme? show the interquartile range with labels on the plot

## ----hist---------------------------------------------------------------------
ggplot(data = faithful, aes(x = eruptions)) + geom_density()

## ----densityHDR2, fig.width = 5, echo=TRUE, eval=FALSE------------------------
#  hdr.den(faithful$eruptions,
#          col = c("skyblue", "slateblue2", "slateblue4"))

## ----densityHDR, fig.width = 5, echo=FALSE,  fig.height = 4-------------------
invisible(hdr.den(faithful$eruptions, 
                  col = c("skyblue", "slateblue2", "slateblue4")))

## ----hdrboxplot, fig.width = 5, fig.height = 4--------------------------------
hdr.boxplot(faithful$eruptions, 
            prob = c(99, 95, 50),
            col = c("skyblue", "slateblue2", "slateblue4"))

## ----setup--------------------------------------------------------------------
library(gghdr)
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot() 


## ----speccols-----------------------------------------------------------------
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot(fill = c("blue")) 

## ----diftProbs----------------------------------------------------------------
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot(prob = c(0.25, 0.5, 0.75, 0.95, 0.99),
                   fill = c("blue")) 

## ----percentProbs-------------------------------------------------------------
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot(prob = c(25, 50, 75, 95, 99),
                   fill = c("blue")) 

## ----percentProbs2------------------------------------------------------------
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot(prob = c(2.55, 50, 75, 95, 99),
                   fill = c("blue")) 

## ----withJitter---------------------------------------------------------------
ggplot(data = faithful, 
       aes(y = eruptions)) + 
  geom_hdr_boxplot(fill = c("blue")) + 
  geom_jitter(aes(x = 0))

## ----withRug------------------------------------------------------------------
ggplot(data = faithful, aes(y = eruptions)) + 
  geom_hdr_boxplot(fill = c("blue")) + 
  geom_rug()

## ----hdrRug, fig.width = 5, fig.height = 4------------------------------------
ggplot(data = faithful, aes(x = waiting, y = eruptions)) + 
  geom_point() +
  geom_hdr_rug(fill = "blue")

## ----hdrRugmultprob, fig.width = 5, fig.height = 4----------------------------
ggplot(data = faithful, aes(x = waiting, y = eruptions)) + 
  geom_point() +
  geom_hdr_rug(prob = c(10, 20, 50, 99), fill = "blue")

## ----showMPG, fig.width = 5, fig.height = 4-----------------------------------
ggplot(data = mpg, 
       aes(x = hwy, fill = as.factor(cyl))) +
  facet_grid(as.factor(cyl)~.) + 
  geom_histogram(bins = 50) 

## ----mpgBox1, fig.width = 5, fig.height = 4-----------------------------------
ggplot(data = mpg, 
       # make sure to change x to y from geom_density to geom_hdr_boxplot
       aes(y = hwy, fill = as.factor(cyl))) + 
  geom_hdr_boxplot()

## ----binned_scatterplot, fig.width = 5, fig.height = 4------------------------
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point(aes(colour = hdr_bin(x = waiting, y = eruptions))) +
  scale_colour_viridis_d(direction = -1)

## ----all_together_now, fig.width = 5, fig.height = 4--------------------------
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point(aes(colour = hdr_bin(x = waiting, y = eruptions))) +
  geom_hdr_rug() +
  scale_colour_viridis_d(direction = -1)

