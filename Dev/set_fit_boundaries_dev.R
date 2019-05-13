fil <- "./input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 0
km <- .4
pulses <- 30
pulse_freq <- 5
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                    bin_size, electrode_distance, dead_space_distance,
                    diffusion_coefficient,
                    convert_current, calibration_current,
                    calibration_concentration)

fb <- set_fit_boundaries(mg, range = "f", base_tolerance = 0.1)

left <- tapply(mg$electrode, mg$src, max)

left

class(left[1])
class(left)

left["simulation"]

mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "simulation"])]
mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "experiment"])]

max(mg$time_sec[mg$electrode >= 0.1 & mg$src == "simulation"])


# Troubleshooting test.
fil <- "./tests/testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 0
km <- .4
pulses <- 30
pulse_freq <- 50
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                    bin_size, electrode_distance, dead_space_distance,
                    diffusion_coefficient,
                    convert_current, calibration_current,
                    calibration_concentration)

fb <- set_fit_boundaries(mg, range = "f", base_tolerance = 0.01)

fb
fb2 <- c(11.84074, 25.15926)

class(fb)
str(fb)
class(c(11.84074, 25.15926))
str(c(11.84074, 25.15926))

expect_equal(fb, fb2, tolerance = 1.0e-6)
fb == fb2
fb[1]
fb2[1]
fb[1] == fb2[1]
fb2[1] - fb[1]

# Rising phase
fil <- "./tests/testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 0
km <- .4
pulses <- 30
pulse_freq <- 50
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                    bin_size, electrode_distance, dead_space_distance,
                    diffusion_coefficient,
                    convert_current, calibration_current,
                    calibration_concentration)

mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "simulation"])]
mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "experiment"])]

fb <- get_fit_boundaries(mg, fit_region = "r")
fb
expect_equal(fb, c(10.71481, 13))

# Rising. Experiment peaks first.
fil <- "./tests/testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 0
km <- .4
pulses <- 30
pulse_freq <- 5
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                    bin_size, electrode_distance, dead_space_distance,
                    diffusion_coefficient,
                    convert_current, calibration_current,
                    calibration_concentration)

mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "simulation"])]
mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "experiment"])]

fb <- get_fit_boundaries(mg, fit_region = "r", base_tolerance = 0.1)
fb
expect_equal(fb, c(13, 32.3), tolerance = 1.0e-6)
