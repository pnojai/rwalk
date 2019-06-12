fil <- "./tests/testdata/180430_DA_saline_1.csv"
fil <- "./input/180920_10mg-kgAMPH_1.csv"
sample_rate <- 100
dat <- read_experiment_csv(fil, sr = sample_rate)

lead_time <- 10
win_len <- 120
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

fil <- "./tests/testdata/180430_DA_saline_1.csv"
sample_rate <- 100
dat <- read_experiment_csv(fil, sr = sample_rate)

lead_time <- 10
win_len <- 120

lead_rows <- lead_time / (sample_rate * 10^-3)
win_rows <- win_len / (sample_rate * 10^-3)

wins <- seq(from = lead_rows + 1, to = nrow(dat), by = win_rows)

plot(dat$time_sec, dat$electrode, type = "l")
nrow(dat)
dat_list <- split_stims(df = dat, lead_time_sec = lead_time, win_length_sec = win_len, sr = sample_rate)

library(openxlsx)
fil_par <- read.xlsx("./exp/file_params.xlsx")
fil_par

fils <- unique(fil_par$filename)
for (stim in fil_par$stimulus[fil_par$filename == fils[1]]) {
        print(stim)
}

fils[1]
fil <- "./input/180920_10mg-kgAMPH_1.csv"
dat <- read_experiment_csv(fil, sr = sample_rate)

work_fil <- fil_par[fil_par$filename == fils[1] , c("stimulus", "start")]

df_list <- list()

max_stim <- max(work_fil$stimulus)

for (stim in work_fil$stimulus) {
        start_idx <- work_fil[stim, "start"]
        if (stim == max_stim) {
                top_row_idx <- nrow(dat)
        } else {
                top_row_idx <- work_fil[(stim + 1), "start"] - 1
        }
        
        df_list[[stim]] <- dat[start_idx:top_row_idx, ]
}

library(ggplot2)
library(openxlsx)
fn <- "180920_10mg-kgAMPH_1"

fil_par <- read.xlsx("./exp/file_params.xlsx")
work_fil <- fil_par[fil_par$filename == fils[1] , c("stimulus", "start")]
df_list <- list()
max_stim <- max(work_fil$stimulus)
for (stim in work_fil$stimulus) {
        start_idx <- work_fil[stim, "start"]
        if (stim == max_stim) {
                top_row_idx <- nrow(dat)
        } else {
                top_row_idx <- work_fil[(stim + 1), "start"] - 1
        }
        
        df_list[[stim]] <- dat[start_idx:top_row_idx, ]
}
qplot(dat$time_sec, dat$electrode, geom = "line")
for (i in 1:(length(df_list) - 0)) {
        p <- qplot(df_list[[i]]$time_sec, df_list[[i]]$electrode, geom = "line",
              main = paste0(fn, "_", i))
        print(p)
}
