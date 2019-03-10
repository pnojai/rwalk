library(tidyverse)
library(quantmod)

fil <- "./Data/181015_10mg-kgAMPH_50mM Nimo_2_outlier_scrub.csv"

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
which(c(smoothed.dx,NA) > 0 & c(NA, smoothed.dx) < 0) 

par(mfrow = c(2, 1), mar = c(4, 4, 0, 1), oma = c(0, 0, 0, 0))
plot(dat$electrode[1500:3000], type = "l")
plot(smoothed.dx[1500:3000], type = "l")
abline(h = 0)


head(dat)
head(smoothed.dx[1500:3000])

window()

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

plot(elec_maxima - elec_minima, type = "l")
