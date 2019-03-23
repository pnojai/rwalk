library(tidyverse)
library(quantmod)

fil <- "./input/181015_10mg-kgAMPH_50mM Nimo_2_outlier_scrub.csv"

sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

n <- nrow(dat)
# Slopes of segments.
dat$slope[2:n] <- dat$electrode[2:n] - dat$electrode[1:(n-1)]
# Change in slopes.
dat$delta_slope[3:n] <- dat$slope[3:n] - dat$slope[2:(n-1)]
# Window them to shake out premature outliers. Moving average.
dat$delta_slope_smooth <- mavg(dat$delta_slope, n = 3)

# Largest change in slope is assumed beginning of stimulus.
# max_delta_slope_smooth <- max(dat$delta_slope_smooth, na.rm = TRUE)

# Return the prior index.
# idx_max_delta_slope_smooth <- which(dat$delta_slope_smooth == max_delta_slope_smooth) - 1

# idx_max_delta_slope_smooth

# Doesn't work
spikes <- head(arrange(dat, desc(delta_slope_smooth)), n = 50)
spikes <- arrange(spikes, time_sec)
plot(dat$time_sec, dat$electrode, type = "l")
abline(v = spikes$time_sec)

# http://r.789695.n4.nabble.com/peak-finding-td849615.html
elec <- smooth.spline(dat$electrode)
head(elec)

plot(dat$electrode, type = "l")
smoothed.dx <- predict(elec, deriv = 1)$y
plot(smoothed.dx, type = "l")
which(c(smoothed.dx,NA) < 0 & c(NA, smoothed.dx) > 0) 

# What about smoothing the electrode data first?
# Right idea, maybe, but this didn't work. Do it with the arguments in smooth.spline().
dat$electrode_smoothed <- mavg(dat$electrode, n = 250)
plot(dat$time_sec, dat$electrode_smoothed, type = "l")
elec_smoothed <- smooth.spline(dat[!is.na(dat$electrode_smoothed), "electrode_smoothed"])
smoothed_smoothed.dx <- predict(elec_smoothed, deriv = 1)$y
# smoothed_smoothed.dx <- mavg(smoothed_smoothed.dx, n = 200)
# smoothed_smoothed.dx[abs(smoothed_smoothed.dx) < 3] <- 0
plot(smoothed_smoothed.dx, type = "l")
which(c(smoothed_smoothed.dx,NA) > 0 & c(NA, smoothed_smoothed.dx) < 0) 

# Try args in smooth.spline(). That did it!
plot(dat$time_sec, dat$electrode, type = "l")
elec <- smooth.spline(dat$electrode, spar = .5)
smoothed.dx <- predict(elec, deriv = 1)$y
plot(smoothed.dx, type = "l")
abline(h = 0)
peaks <- which(c(smoothed.dx,NA) < 0 & c(NA, smoothed.dx) > 0) 

plot(dat$time_sec, dat$electrode, type = "l")
peaks <- find_stim_peaks(dat)
for (i in peaks) {
        abline(v = i)
}

# What about loess. Nope.
plot(dat$time_sec, dat$electrode, type = "l")
elec <- predict(loess(formula = electrode ~ time_sec, dat, span = 0.05))
plot(elec, type = "l")
elec <- smooth.spline(elec)
smoothed.dx <- predict(elec, deriv = 1)$y
plot(smoothed.dx, type = "l")
which(c(smoothed.dx,NA) < 0 & c(NA, smoothed.dx) > 0) 


par(mfrow = c(2, 1), mar = c(4, 4, 0, 1), oma = c(0, 0, 0, 0))
plot(dat$electrode[1500:3000], type = "l")
plot(smoothed.dx[1500:3000], type = "l")
abline(h = 0)


head(dat)
head(smoothed.dx[1500:3000])

# window()

str(smoothed.dx)

smoothed_logical <- smoothed.dx > 6
plot(smoothed.dx[smoothed_logical], type = "l")
smoothed.dx_trim <- smoothed.dx[smoothed_logical]
which(c(smoothed.dx_trim,NA) > 0 & c(NA, smoothed.dx_trim) < 0) 

head(smoothed.dx)
head(c(NA, smoothed.dx))

dat_ts <- ts(data = dat$electrode, start = min(dat$time_sec), end = max(dat$time_sec), frequency = 10)
window(dat_ts, start = 0, end = 10)

head(dat, n = 20)
tail(dat)


str(dat_ts)
plot(dat_ts)

elec_maxima <- vector(mode = "numeric", length = 0)
elec_minima <- vector(mode = "numeric", length = 0)
for (i in unique(floor(dat$time_sec))) {
        electrode <- dat[dat$time_sec >= (i) & dat$time_sec < (i + 1), "electrode"]
        elec_maxima <- c(elec_maxima, max(electrode))
        elec_minima <- c(elec_minima, min(electrode))
        
        # if (i >= ) {
        #         print(i)
        #         print(electrode)
        #         print(max(electrode))
        #         print(min(electrode))
        # }
        
}

elec_maxima <- sort(elec_maxima, decreasing = TRUE)
elec_minima <- sort(elec_minima)

head(elec_maxima - elec_minima, n = 30)
tail(elec_maxima - elec_minima)

dat_sub <- dat[dat$time_sec < 130, ]
plot(dat_sub$time_sec, dat_sub$electrode, type = "l")
plot(elec_maxima, type = "l")
plot(elec_minima, type = "l")

plot(elec_maxima - elec_minima, type = "l")

win_len <- 1200
wins <- seq(from = 1, to = nrow(dat), by = win_len)
elec_maxima <- vector(mode = "numeric", length = 0)
elec_minima <- vector(mode = "numeric", length = 0)
plot(dat$electrode, type = "l")
for (i in wins) {
        first <- i
        last <- min((i + win_len -1), nrow(dat))
        
        electrode <- dat[first:last, "electrode"]
        elec_maxima <- c(elec_maxima, max(electrode))
        elec_minima <- c(elec_minima, min(electrode))
        
        if (i >= 7201 & i < 10800) {
                print(paste0("electrode count: ", length(electrode)))
                print(paste0(first, ":", last, " max = ", max(electrode), " idx = ", max(which(dat$electrode == max(electrode)))))
                print(electrode)
                # print(max(electrode))
                # print(min(electrode))
                abline(h = max(electrode))
                abline(v = max(which(dat$electrode == max(electrode))))
        }
        
        # plot(electrode, type = "l", main = i)
        # abline(v = max(which(dat$electrode == max(electrode))))
        
}

elec_maxima_idx <- which(dat$electrode == elec_maxima)

sapply(elec_maxima, function(x) {max(which(dat$electrode == x))})
which(dat$electrode == 100781.0)

plot(dat$time_sec, dat$electrode, type = "l")
abline(v = 480)

win_len <- 1200
wins <- seq(from = 1, to = nrow(dat), by = win_len)
dat_list <- list()
for (i in wins) {
        first <- i
        last <- min((i + win_len -1), nrow(dat))
        
        stim <- dat[first:last, ]
        #print(str(stim))

        if (nrow(stim) == win_len) {
                dat_list <- c(dat_list, list(stim))
        }
        # str(dat_list)
        # plot(electrode, type = "l", main = i)
        # abline(v = max(which(dat$electrode == max(electrode))))
}

lapply(dat_list, function(x) {plot(x$time_sec, x$electrode, type = "l")})

plot(dat[dat$time_sec < 120, ])

x <- c(14,15,12,11,12,13,14,15,16,15,14,13,12,11,14,12) 
plot(x, type = "l")
smoothed.dx <- predict(smooth.spline(x), deriv=1)$y 
plot(smoothed.dx, type = "l")
which(c(smoothed.dx,NA) < 0 & c(NA, smoothed.dx) > 0) 

# synthesis of a 440 Hz sound with background noise
n <- noisew(d=1,f=8000)
s <- synth(d=1,f=8000,cf=440)
ns <- n+s
# remove noise (but low frequency content still there)
a <- rmnoise(ns,f=8000)

dat_list <- split_stims(dat)

plot(dat$time_sec, dat$electrode, type = "l")

lapply(dat_list, function(x) {plot(x$time_sec, x$electrode, type = "l")})

class(dat_list[[1]])
