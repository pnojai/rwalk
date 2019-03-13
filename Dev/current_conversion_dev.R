fil <- "Data/AMPH_test.csv"
sample_rate <- 100
dat <- read_experiment_csv(fil, sr = sample_rate)

head(dat)
tail(dat)

tail(current_to_concentration(dat))

electrode_first <- dat[1, 2]
electrode_last <- dat[nrow(dat), 2]
timestamp_last <- dat[nrow(dat), 1]

electrode_scale <- (electrode_last - electrode_first) / timestamp_last

dat_conversion <- dat$electrode - (electrode_first + (electrode_scale * dat$time_sec))

head(dat_conversion)

dat$electrode_scaled <- dat_conversion
head(dat)
tail(dat)

plot(dat$time_sec, dat$electrode_scaled)

dat_concentration <- current_to_concentration(dat)

plot(dat_concentration$time_sec, dat_concentration$electrode)
