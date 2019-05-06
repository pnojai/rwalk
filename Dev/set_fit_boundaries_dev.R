fil <- "./input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
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

left <- tapply(mg$electrode, mg$src, max)

left

class(left[1])
class(left)

left["simulation"]

mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "simulation"])]
mg$time_sec[mg$electrode == max(mg$electrode[mg$src == "experiment"])]
