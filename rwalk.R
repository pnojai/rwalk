rwalk_tut <- function(bins = 7, iters = 5, mols = 7000, smooth = 4) {
        # Initialize a matrix. Give it an extra row for time = 0,
        # and an extra column for smoothing the reflecting surface.
        rw <- matrix(rep(0, (bins + 1) * (iters + 1)), iters + 1, bins + 1)
        
        # Release molecules from the reflecting surface at time 0 (row 1)
        rw[1, 1] <- mols
        
        # Iterate in time
        for (i in 2:(iters + 1)) {
                rw[i, 1] <- mean(rw[(i - 1), 1:2])
                
                # Diffuse the molecules until you're two away from the electrode.
                for (j in 2:(bins - 2)) {
                        rw[i, j] <- mean(c(rw[i - 1, j - 1], rw[i - 1, j + 1]))
                }
                
                # Diffuse the molecules next to the electrode.
                rw[i, bins -1] <- .5 * rw[i - 1, bins - 2]
                
                # And at the electrode.
                rw[i, bins] <- .5 * rw[i - 1, bins - 1]
                
        }
        
        # Smooth the counts at the electrode.
        for (i in 1:(iters + 1 - smooth + 1)) {
                rw[i, bins + 1] <- mean(rw[i:(i + smooth - 1), bins])
        }
        
        #Return the random walk matrix.
        rw
}

micmen <- function(x, vmax = 4.57, km = .78, duration) {
        # Correct for uptake according to the Michaelis-Menten equation.
        x - ((( vmax * x ) / ( km + x )) * duration )
        
}

# Test micmen()
# Inputs
# x: 1.375
# vmax: 4.57 (default)
# km: .78 (default)
# duration: .007407
# Expected result: 1.353
micmen(x = 1.375, duration = .007407)

iteration_duration <- function(diffusion_coefficient, bin_size) {
        # diffusion_coefficient: square centimeters / second.
        # bin_size: micrometres.
        # t = x^2/2D
        
        ((bin_size / 10000.0)^2) / (2 * diffusion_coefficient)
}

rwalk_amp <- function(vmax = 4.57, km = .78, release = 2.75, bin_size = 2.0,
                      bin_number_displace = 25, dead_space_displace = 2,
                      diffusion_coefficient = .0000027, iterations = 128) {
        # Amperometry simulation
        # Author: Jai Jeffryes
        # Email: jai@jeffryes.net
        
        # Parameters
        # vmax: Micro M / sec.
        # km: Micro M.
        # release: Micro M.
        # bin_size: Micrometres.
        # bin_number_left: Number of bins left of the electrode.
        # bin_number_right: Number of bins right of the electrode.
        # diffusion_coefficient: square centimeters / second.
        # iterations:
        
        # Calculate the duration of an iteration.
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        
        # Initialize a matrix. Give it an extra row for time = 0.
        # Bins = specified columns to the left of the electrode, to the right, and electrode in the middle.
        # No extra column for smoothing the reflecting surface. That will be a separate data structure.
        bins <- 2 * bin_number_displace  + 1
        rw <- matrix(rep(0.0, (bins) * (iterations + 1)), iterations + 1, bins)
        
        # Position the electrode and the dead space
        electrode_pos <- bin_number_displace + 1

        # Identify dead spaces in a logical vector        
        dead_space_range <- (bin_number_displace - dead_space_displace + 1):(bin_number_displace + dead_space_displace + 1)
        dead_space_bin <- rep(FALSE, bins)
        dead_space_bin[dead_space_range] <- TRUE
        
        # Release at time 0 (row 1). Don't release to dead space.
        rw[1, !dead_space_bin] <- release

        # Iterate in time
        for (i in 2:(iterations + 1)) {
                # Fill extreme bins.
                # Outside bins take from inside neighbor only
                curr_bin <- 1
                inside_neighbor <- curr_bin + 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, 1] <- val
                
                #Really, could assume this is the same, but hey, I'm cautious
                mirror_bin <- bins
                inside_neighbor <- mirror_bin - 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <-  val
                
                # 2nd bins in take .711 from outside neighbor, .5 from inside
                curr_bin <- 2
                inside_neighbor <- curr_bin + 1
                outside_neighbor <- curr_bin - 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                
                # Same, but cautious
                mirror_bin <- bins - 1
                inside_neighbor <- mirror_bin - 1
                outside_neighbor <- mirror_bin + 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val
                
                # Diffuse the molecules until you're one away from the electrode.
                # Think about it like you're working inwards along the displacements from the electrode.
                for (j in 3:(electrode_pos - 2)) {
                outside_neighbor <- j - 1
                inside_neighbor <- j + 1
                
                val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, j] <- val
                
                # Do it at the mirror bin. Don't fry your brain on the indexes.
                mirror_bin <- bins - j + 1
                outside_neighbor <- mirror_bin + 1
                inside_neighbor <- mirror_bin - 1
 
                val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val
                
                }

                # Diffuse the molecules next to the electrode. Receives .5 from outside.
                curr_bin <- electrode_pos - 1
                outside_neighbor <- curr_bin - 1
                
                val <- .5 * rw[i - 1, outside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                
                mirror_bin <- electrode_pos + 1
                outside_neighbor <- curr_bin - 1
                
                val <- .5 * rw[i - 1, outside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val

                # And at the electrode.
                val <- .5 * rw[i - 1, electrode_pos - 1] + .5 * rw[i - 1, electrode_pos + 1]
                # Note: there is no Michaelis-Menten correction for uptake at the electrode.
                rw[i, electrode_pos] <- val

        }
        # 
        # # Smooth the counts at the electrode.
        # for (i in 1:(iters + 1 - smooth + 1)) {
        #         rw[i, bins + 1] <- mean(rw[i:(i + smooth - 1), bins])
        # }
        
        #Return the random walk matrix.
        rw
        
        
}

electrode_distance <- function() {
        # Calculate a bin's distance from the electrode.
        # For reporting, not calculation.
               
}