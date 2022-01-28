# Explore the regression
# run '0_data_wrangle' first

# load libraries
library(knitr)
library(here)
library(bookdown)
library(dplyr)
library(tidyverse)
library(ggplot2)

# read in data, calculate B6/B2, export df
raw <- read.csv(here::here("data", "fan_data_raw.csv"), stringsAsFactors = TRUE) %>% 
  mutate("B6B2" = B6/B2)
save(raw, file = here::here("data", "fan_data_raw_B6B2.rda"))

load(file = here::here("data", "all_fans.rda"))
B6B2 <- all_fans$B6/all_fans$B2
all_fans <- cbind(all_fans, B6B2)
save(all_fans, file = here::here("data", "all_fans_B6B2.rda"))
all_fans[nrow(all_fans)+ rep(26,1),] <- NA # make same length as raw data for plotting

load(file = here::here("data", "all_fans_long.rda"))
all_fans_long <- rbind(all_fans_long, B6B2)
row_names <- c("age", "age_sd", "B1", "B1_sd", "B2", "B2_sd", 
  "B3", "B3_sd", "B4", "B4_sd","B5", "B5_sd", 
  "B6", "B6_sd", "B7", "B7_sd", "tot_spots", "B6B2")
all_fans_long <- data.frame(all_fans_long,row.names = row_names)
save(all_fans_long, file = here::here("data", "all_fans_long_B6B2.rda"))


# colors
my_grey <- rgb(0.13, 0.13, 0.13, alpha = 0.5) 
my_red <- rgb(.6, 0.1, 0.07, alpha = 0.5) 
my_red <- rgb(.6, 0.1, 0.07, alpha = 0.5)
my_blue <- rgb(.2, 0.2, 0.7, alpha = 0.7)

ylims <- c(.7,1.4)

# linear model raw
mod_lin <- lm(B6B2 ~ age, data = raw)
summary(mod_lin)
resids <- residuals(mod_lin)

# linear model merge
mod_lin_merge <- lm(B6B2 ~ age, data = all_fans)
summary(mod_lin_merge)
resids_merge <- residuals(mod_lin_merge)
resids_merge <- c(resids_merge, rep(NA,26))

# plot all four on one
jpeg(file = here::here("outputs", "plot_lm_all.jpeg"),
     units="in", width= 7, height=7, res=300)
par(mfrow = c(2,2), mar = c(4, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))

# plot linear model
#jpeg(file = here::here("outputs", "plot_lm.jpeg"),
     #units="in", width= 7, height=4, res=300)
#par(mfrow = c(1,2), mar = c(4, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
points(raw$age, raw$B6B2,
       pch = 20, col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
abline(mod_lin, col = my_grey)
abline(mod_lin_merge, col = my_red)
legend("topleft", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("Linear Model", cex.main = .9)

# residuals
# loess fit
mod_lo <- loess(resids ~ age, data = raw)
xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- predict(mod_lo, new = data.frame(age = xp))

plot(raw$age, resids,
     type = "n",
     ylim = c(-.2,.2),
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Residuals "["(B6 / B2)"])
     )
points(raw$age, resids,
       pch = 20, col = my_grey)
points(all_fans$age, resids_merge,
       pch = 24, bg = my_red)
abline(h = 0, lty = 2)
lines(xp, yp, col = "black") #loess
legend("topright", legend = "Loess Fit", lty = 1, pch = 20, col = my_grey,
       bty = "n", pt.cex = 1, cex = 0.8)

#dev.off()

# plot diagnostics
#jpeg(file = here::here("outputs", "plot_lm_diagnostics.jpeg"),
   #  units="in", width= 7, height=4, res=300)
#par(mfrow = c(1,2), mar = c(4, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
hist(resids,
     xlab = expression("Residuals "["(B6 / B2)"]),
     col = my_grey,
     main = "")     
qqnorm(resids,
       ylab = expression("Residuals "["(B6 / B2)"]),
       pch = 20, col = my_grey,
       main = "")
qqline(resids)

dev.off()

#plot loess model
mod_lo <- loess(B6B2 ~ age, data = raw)
xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- predict(mod_lo, new = data.frame(age = xp))

jpeg(file = here::here("outputs", "plot_lm_loess.jpeg"),
     units="in", width= 7, height=5, res=300)

par(mfrow = c(1,1), mar = c(4, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
points(raw$age, raw$B6B2,
       pch = 20, col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
lines(xp, yp, col = "black")
legend("topleft", legend = c("Individual fan surfaces","Loess fit", "Stratigraphic units"),
       pch = c(20,NA,24), col = c(my_grey, "black", "black"), lty = c(NA, 1, NA),
       pt.bg = c(NA, NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("Loess Fit", cex.main = .9)

dev.off()

acf(resids)

