library(knitr)
library(here)
library(bookdown)
library(dplyr)
library(tidyverse)
library(ggplot2)

load(file = here::here("data", "fan_data_raw_B6B2.rda"))
load(file = here::here("data", "all_fans_B6B2.rda"))
load(file = here::here("data", "all_fans_long_B6B2.rda"))

#all_fans <- na.omit(all_fans)

# colors
my_grey <- rgb(0.13, 0.13, 0.13, alpha = 0.5) 
my_red <- rgb(.6, 0.1, 0.07, alpha = 0.5) 
my_red <- rgb(.6, 0.1, 0.07, alpha = 0.5)
my_blue <- rgb(.2, 0.2, 0.7, alpha = 0.7)

ylims <- c(.7,1.4)

# Quadratic
mod_quad <- lm(B6B2 ~ age + I(age^2), data = raw)
xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- predict.lm(mod_quad, new = data.frame(age = xp))

b0_quad <- coef(mod_quad)[1]
b1_quad <- coef(mod_quad)[2]
b2_quad <- coef(mod_quad)[3]

mod_quad_m <- lm(B6B2 ~ age + I(age^2), data = all_fans)
xp_m <- seq(min(all_fans$age), max(all_fans$age), by = 1)
yp_m <- predict.lm(mod_quad_m, new = data.frame(age = xp_m))

b0_quad_m <- coef(mod_quad)[1]
b1_quad_m <- coef(mod_quad)[2]
b2_quad_m <- coef(mod_quad)[3]

jpeg(file = here::here("outputs", "mod_quad.jpeg"),
     units="in", width= 4, height= 3.5, res=300)
par(mfrow = c(1,1), mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
grid()
points(raw$age, raw$B6B2,
       pch = 20, col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)

lines(xp, yp, col = "black")
lines(xp_m, yp_m, col = my_red)

legend("topleft", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("Quadratic Fit", cex.main = .9)

dev.off()

summary(mod_quad)
summary(mod_quad_m)

# log transformation all fans
y <- log(raw$B6B2)
x <- log(raw$age)
mod_log <- lm(y ~ x)
summary(mod_log)
b1 <- coef(mod_log)[2]
b0 <- coef(mod_log)[1]

# generate vectors to plot fitted relation
xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- exp(b0)*xp^b1

xp_log <- seq(min(raw$age), max(raw$age), by = 1)
yp_log <- exp(b0)*xp^b1

# log transformation all fans
y_m <- log(all_fans$B6B2)
x_m <- log(all_fans$age)
mod_log_m <- lm(y_m ~ x_m)
summary(mod_log_m)
b1_m <- coef(mod_log_m)[2]
b0_m <- coef(mod_log_m)[1]

# generate vectors to plot fitted relation
xp_m <- seq(min(all_fans$age), max(all_fans$age), by = 1)
yp_m <- exp(b0_m)*xp_m^b1_m

# plot log transformed
jpeg(file = here::here("outputs", "mod_log.jpeg"),
     units="in", width= 7, height= 3.5, res=300)
par(mfrow = c(1,2), mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))

plot(raw$age, raw$B6B2,
     log = "xy",
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
grid()
points(raw$age, raw$B6B2,
       pch = 20,
       col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
lines(xp, yp, col = "black")
lines(xp_m, yp_m, col = my_red)
legend("topleft", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("Log-log transformation (log)", cex.main = .9)

par(mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
grid()
points(raw$age, raw$B6B2,
       pch = 20,
       col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
lines(xp, yp, col = "black")
lines(xp_m, yp_m, col = my_red)
legend("topleft", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("Log-log transformation (arithmetic)", cex.main = .9)

dev.off()

# Nonlinear least squares: without weights
alpha_start <- exp(b0)
gamma_start <- b1
mod_nls <- nls(B6B2 ~ alpha*age^gamma, data = raw,
               start = list(alpha = alpha_start, gamma = gamma_start))
# extract estimated coefficients
alpha_nls <- summary(mod_nls)$coefficients[1, 1]
gamma_nls <- summary(mod_nls)$coefficients[2, 1]

xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- alpha_nls*xp^gamma_nls

# NLS: without weights merged
alpha_start_m <- exp(b0_m)
gamma_start_m <- b1_m
mod_nls_m <- nls(B6B2 ~ alpha*age^gamma, data = all_fans,
               start = list(alpha = alpha_start_m, gamma = gamma_start_m))
# extract estimated coefficients
alpha_nls_m <- summary(mod_nls_m)$coefficients[1, 1]
gamma_nls_m <- summary(mod_nls_m)$coefficients[2, 1]

xp_m <- seq(min(all_fans$age), max(all_fans$age), by = 1)
xp_m <- c(xp_m, rep(NA,4)) # make same length as xp for plotting
yp_m <- alpha_nls_m*xp^gamma_nls_m


jpeg(file = here::here("outputs", "NLS_unweighted.jpeg"),
     units="in", width= 5, height= 3.5, res=300)
par(mfrow = c(1,1), mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))

plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
grid()
points(raw$age, raw$B6B2,
       pch = 20,
       col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
lines(xp, yp, col = "black")
lines(xp_m, yp_m, col = my_red)
legend("topleft", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("NLS: Without weights", cex.main = .9)

dev.off()

#NLS with weights, iterative
alpha_start <- exp(b0)
gamma_start <- b1
max_iter <- 1000
delta <- 1e-6
for (i in 1:max_iter) {
        mod_nlsw2 <- nls(B6B2 ~ alpha*age^gamma, data = raw,
                         start = list(alpha = alpha_start, gamma = gamma_start),
                         weights = 1/(alpha_start*age^gamma_start)^2)
        alpha_nlsw2 <- summary(mod_nlsw2)$coefficients[1, 1]
        gamma_nlsw2 <- summary(mod_nlsw2)$coefficients[2, 1]
        if ((alpha_nlsw2 - alpha_start)/alpha_start < delta & 
            (gamma_nlsw2 - gamma_start)/gamma_start < delta) break
        alpha_start <- alpha_nlsw2
        gamma_start <- gamma_nlsw2
}
# number of iterations and final parameter estimates
cat("Number of iterations = ", i)
cat("Final alpha = ", alpha_nlsw2)
cat("Final gamma =", gamma_nlsw2)

#NLS weighted oterative merge
alpha_start_m <- exp(b0_m)
gamma_start_m <- b1_m
max_iter <- 1000
delta <- 1e-6
for (i in 1:max_iter) {
        mod_nlsw2_m <- nls(B6B2 ~ alpha*age^gamma, data = all_fans,
                         start = list(alpha = alpha_start_m, gamma = gamma_start_m),
                         weights = 1/(alpha_start_m*age^gamma_start_m)^2)
        alpha_nlsw2_m <- summary(mod_nlsw2_m)$coefficients[1, 1]
        gamma_nlsw2_m <- summary(mod_nlsw2_m)$coefficients[2, 1]
        if ((alpha_nlsw2_m - alpha_start_m)/alpha_start_m < delta & 
            (gamma_nlsw2_m - gamma_start_m)/gamma_start_m < delta) break
        alpha_start_m <- alpha_nlsw2_m
        gamma_start_m <- gamma_nlsw2_m
}

# number of iterations and final parameter estimates
cat("Number of iterations = ", i)
cat("Final alpha = ", alpha_nlsw2_m)
cat("Final gamma =", gamma_nlsw2_m)

# set up for plot
xp <- seq(min(raw$age), max(raw$age), by = 1)
yp <- alpha_nlsw2*xp^gamma_nlsw2
#merged
xp_m <- seq(min(all_fans$age), max(all_fans$age), by = 1)
xp_m <- c(xp_m, rep(NA,4)) # make same length as xp for plotting
yp_m <- alpha_nlsw2_m*xp^gamma_nlsw2_m

#plot
jpeg(file = here::here("outputs", "NLS_weighted.jpeg"),
     units="in", width= 5, height= 3.5, res=300)
par(mfrow = c(1,1), mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))

plot(raw$age, raw$B6B2,
     type = "n",
     ylim = ylims,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Brightness Ratio "["(B6 / B2)"]))
grid()
points(raw$age, raw$B6B2,
       pch = 20,
       col = my_grey)
points(all_fans$age, all_fans$B6B2,
       pch = 24, bg = my_red)
lines(xp, yp, col = "black")
lines(xp_m, yp_m, col = my_red)
legend("topleft", legend = c("Individual fan surfaces, iterations (3)", "Stratigraphic units, iterations (3)"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.8)
title("NLS: Iterative with weights", cex.main = .9)
dev.off()

# compare

RSE <- function(mod) {
        RSE_mod <- sqrt(deviance(mod)/df.residual(mod))
        print(RSE_mod)
}

R2_adj <- function(mod) {
        R2 <- summary(mod)$adj.r.squared
        print(R2)
}

method <- c("log-log regression (indv.)", "log-log regression (strat. unit)", 
            "Quadradic (indv.)", "Quadradic (strat. unit)",
            "NLS without weights (indv.)","NLS without weights(strat. unit)",
            "NLS iterative weights (indv.)", "NLS iterative weights (strat. unit)")

alpha <- c(exp(b0), exp(b0_m), b0_quad, b0_quad_m, 
           alpha_nls, alpha_nls_m, alpha_nlsw2, alpha_nlsw2_m)

gamma <- c(b1, b1_m, b1_quad, b1_quad_m, 
           gamma_nls,gamma_nls_m, gamma_nlsw2, gamma_nlsw2_m)

beta2 <- c(NA, NA, b2_quad, b2_quad_m, NA, NA, NA, NA)

RSE <- c(RSE(mod_log), RSE(mod_log_m), RSE(mod_quad), RSE(mod_quad_m),
         RSE(mod_nls),RSE(mod_nls_m),RSE(mod_nlsw2),RSE(mod_nlsw2_m))

DF <- c(31,5,30,4,31,5,31,5)

R2_adj <- c(R2_adj(mod_log), R2_adj(mod_log_m), R2_adj(mod_quad), R2_adj(mod_quad_m),
            R2_adj(mod_nls),R2_adj(mod_nls_m),R2_adj(mod_nlsw2),R2_adj(mod_nlsw2_m))


model_stats <- data.frame(method, alpha, gamma, beta2, RSE, DF, R2_adj)
names(model_stats) <- c("Method", "$\\alpha$", "$\\gamma$", "$\\beta$~2~" ,"RSE", "df", "R^2")

save(model_stats, file = here::here("data", "model_stats.rda"))
knitr::kable(model_stats, digits = c(NA,3,3,5,4,1,3))


summary(mod_log)
summary(mod_quad)
summary(mod_nls)
summary(mod_nlsw2)

summary(mod_log_m)
summary(mod_quad_m)
summary(mod_nls_m)
summary(mod_nlsw2_m)
        
# Log Transformation Diagnostics
resids <- residuals(mod_log)
resids_m <- residuals(mod_log_m)

jpeg(file = here::here("outputs", "Log_residuals.jpeg"),
     units="in", width= 5, height= 4, res=300)
par(mfrow = c(1,1), mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))

plot(raw$age, resids,
     type = "n",
     ylim = c(-.2,.2),
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Residuals "["(B6 / B2)"])
)
points(raw$age, resids,
       pch = 20, col = my_grey)
points(all_fans$age, resids_m,
       pch = 24, bg = my_red)
abline(h = 0, lty = 2)
legend("topright", legend = c("Individual fan surfaces", "Stratigraphic units"),
       pch = c(20,24), col = c(my_grey, "black"), pt.bg = c(NA, my_red), 
       bty = "n", pt.cex = 1, cex = 0.7)
title("Log Transformation Residuals", cex.main = .9)

dev.off()

# Predicted vs. Observed Log Trasform
# Baskerville (1972) back transformation applied
y <- log(raw$B6B2)
x <- log(raw$age)
mod_log <- lm(x ~ y, na.action = na.exclude)
resids <- residuals(mod_log)
yhat <- mod_log$fitted.values
age_pred <- data.frame(exp(yhat))
age <- raw[3]
ages <- data.frame(age,age_pred)

se <- summary(mod_log)$sigma
age_pred <- exp(yhat + se^2/2)
age <- raw[3]
ages <- data.frame(age,age_pred)

# index stats
age <- raw$age
age_diff <- age_pred - age
rmse <- sqrt(mean(age_diff^2))
mae <- mean(abs(age_diff))
mbe <- mean(age_diff)
log_trans_stats <- data.frame(
        Index = c("RMSE", "MAE", "MBE"),
        Value = c(rmse, mae, mbe))
save(log_trans_stats, file = here::here("data", "log_trans_stats.rda"))
knitr::kable(log_trans_stats, digits = 1)

#pred vs. observed ages
jpeg(file = here::here("outputs", "Age-Age_plots.jpeg"),
     units="in", width= 7, height= 6.2, res=300)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(mar = c(3, 4, 1, 1), tcl = -0.2, mgp = c(2, 0.5, 0))
plot(ages,
     pch = 20, col = my_blue,
     xlab = expression("Predicted Age" ~ "(Ka)"),
     ylab = expression("Observed Age" ~ "(Ka)"))
grid()
abline(coef = c(0,1))
legend("topleft", bty = "n", lty = 1, col = "black",
       legend = "1:1 line")

# age error scatterplot
plot(y = age_diff, x = age,
     pch = 20, col = my_blue,
     xlab = expression("Age" ~ "(Ka)"),
     ylab = expression("Error" ~ "(Ka)"))
abline(h=0)

# age error histogram
hist(age_diff,
     breaks = 10,
     col = my_blue,
     main = "",
     xlab = expression("Predicted - Observed Age (Ka)"))

dev.off()
