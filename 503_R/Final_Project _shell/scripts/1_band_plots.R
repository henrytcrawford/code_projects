# Band plots
# run '0_data_wrangle' first

# load libraries
library(knitr)
library(here)
library(bookdown)
library(dplyr)
library(tidyverse)
library(ggplot2)

# read in neccassary files
raw_data <- read.csv(here::here("data", "fan_data_raw.csv"), stringsAsFactors = TRUE)
load(file = here::here("data", "all_fans.rda"))
load(file = here::here("data", "all_fans_long.rda"))

# set up 
all_fans_band <- all_fans_long[c("B1","B2","B3", "B4","B5","B6", "B7"),] %>% 
  mutate("Band" = c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5", "Band 6","Band 7"))

surface_colors <- c("#23155D", "#183571", "#1B7585", 
                    "#1D9A6C", "#44B057", "#85C56C")

ylims = c(100, 140)
xlabs = ""
ylabs = "Relative Brightness"


# ggplot
rel_bright_merge <- ggplot(data = all_fans_band, aes(x = Band, y = Q4)) +
  geom_line(aes(y = Q4, col = "Q4"), group = 1) +
  #geom_point(aes(y = Q4)) +
  
  geom_line(aes(y = Q3, col = "Q3"), group = 1) +
  #geom_point(aes(y = Q3, col = "Q3")) +
  
  geom_line(aes(y = Q2c, col = "Q2c"), group = 1) +
  #geom_point(aes(y = Q2c)) +
  
  geom_line(aes(y = Q2b, col = "Q2b"), group = 1) +
  #geom_point(aes(y = Q2b)) +
  
  geom_line(aes(y = Q2a, col = "Q2a"), group = 1) +
  #geom_point(aes(y = Q2a)) +
  
  geom_line(aes(y = Q1b, col = "Q1"), group = 1) +
  #geom_point(aes(y = Q1b)) +
  
  scale_linetype_manual(values = 1,
                        labels = c("Q4" = "Q4", "Q3" = "Q3", "Q2c" = "Q2c",
                                   "Q2b" = "Q2b", "Q2a" = "Q2a", "Q1b" = "Q1b")) +
  
  scale_colour_manual(values = surface_colors,
                      guide = guide_legend(
                        title = "")) +
  
  ylim(ylims) +
  labs(x = xlabs, y = ylabs) +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))  

# save rel_bright_merge as jpeg
jpeg(file = here::here("outputs", "rel_bright_merge.jpeg"),units="in", width=5, height=4, res=300)
rel_bright_merge
dev.off()

# multiple plots base r, 
# no loop for label/axis management
# set up limits
all_bands_sd <- all_fans_long[c("B1_sd","B2_sd","B3_sd", "B4_sd","B5_sd","B6_sd", "B7_sd"),] %>% 
  mutate("Band" = c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5", "Band 6","Band 7"))
x<-c(1,2,3,4,5,6,7)

# plot settings
ylims = c(90, 150)
xlabs = ""
ylabs = ""
ylabs_txt <- "Relative Brightness"
b_labels <- c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5", "Band 6", "Band 7")


# set jpeg for saving
jpeg(file = here::here("outputs", "rel_bright_lims.jpeg"),
     units="in", width= 5, height=10, res=300)

# set up plot frame
# 7th plot as blank for X-axis label
par(mfrow = c(7,1), mar = c(.25,2.5,.5,1))
plot(all_fans_band$Q4, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q4 + all_bands_sd$Q4, 
          rev(all_fans_band$Q4 - all_bands_sd$Q4)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q4, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE, tck = 0.04)
axis(side = 2, labels = FALSE, tck = 0.02)
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q4", 
       bty = "n")

plot(all_fans_band$Q3, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q3 + all_bands_sd$Q3, 
          rev(all_fans_band$Q3 - all_bands_sd$Q3)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q3, type = "o", pch = 16,
      ylim = ylims,
      xlab = xlabs,
      ylab = ylabs,
      xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE, tck = 0.04)
axis(side = 2, labels = FALSE, tck = 0.02)
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q3", 
       bty = "n")

plot(all_fans_band$Q2c, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q2c + all_bands_sd$Q2c, 
          rev(all_fans_band$Q2c - all_bands_sd$Q2c)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q2c, type = "o", pch = 16,
      ylim = ylims,
      xlab = xlabs,
      ylab = ylabs,
      xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE, tck = 0.04)
axis(side = 2, labels = FALSE, tck = 0.02)
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q2c", 
       bty = "n")

plot(all_fans_band$Q2b, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q2b + all_bands_sd$Q2b, 
          rev(all_fans_band$Q2b - all_bands_sd$Q2b)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q2b, type = "o", pch = 16,
      ylim = ylims,
      xlab = xlabs,
      ylab = ylabs,
      xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE, tck = 0.04)
axis(side = 2, labels = FALSE, tck = 0.02)
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q2b", 
       bty = "n")

plot(all_fans_band$Q2a, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     xaxt = "n", yaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q2a + all_bands_sd$Q2a, 
          rev(all_fans_band$Q2a - all_bands_sd$Q2a)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q2a, type = "o", pch = 16,
      ylim = ylims,
      xlab = xlabs,
      ylab = ylabs,
      xaxt = "n", yaxt = "n")
axis(side = 1, labels = FALSE, tck = 0.04)
axis(side = 2, labels = FALSE, tck = 0.02)
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q2a", 
       bty = "n")

plot(all_fans_band$Q1b, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     yaxt = "n",
     xaxt = "n")
polygon(c(x, rev(x)), 
        c(all_fans_band$Q1b + all_bands_sd$Q1b, 
          rev(all_fans_band$Q1b - all_bands_sd$Q1b)), 
        col = "lightgrey", border = "lightgrey")
lines(all_fans_band$Q1b, type = "o", pch = 16,
     ylim = ylims,
     xlab = xlabs,
     ylab = ylabs,
     yaxt = "n",
     xaxt = "n")
axis(side = 2, labels = FALSE, tck = 0.02)
axis(side = 1, cex.axis = 1.5,font = 2, family = 'serif')
title(ylab = ylabs_txt, mgp = c(1,1,0))
legend("topright", 
       legend = "Q1", 
       bty = "n")

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',
     main = "Landsat 8 Band", line = -4, cex.main = 2,
     font = 2, family = 'serif')
legend("bottom", legend = "", lty = 0,
       col = "grey",
       pch = 15, bty ='n', cex = 1.5,
       pt.cex = 4, y.intersp = 1.2, x.intersp = 4,
       xjust = 0)
legend("bottom", legend = expression(paste(""%+-%""~"1"~sigma)), 
       lty = 1, lwd = 1.3, 
       col = "black" , 
       pch = c(20,NA), cex = 1.5,
       y.intersp=1.2, bty = 'n', x.intersp = .5,
       seg.len = 1.265, xjust =1)


dev.off() # end jpeg write

