fil <- "./tests/testdata/180430_DA_saline_1.csv"
sample_rate <- 100
dat <- read_experiment_csv(fil, sr = sample_rate)

lead_time <- 10
win_len <- 119
wins <- seq(from = lead_time, to = max(dat$time_sec), by = win_len)

plot(dat$time_sec, dat$electrode, "l")
for (i in wins) {
        abline(v = i)
}

for (i in wins) {
        plot(dat$time_sec[dat$time_sec >= i & dat$time_sec < i + win_len],
             dat$electrode[dat$time_sec >= i & dat$time_sec < i + win_len], type = "l",
             main = i)
}

wins

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
