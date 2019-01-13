setwd("/media/sf_DataScience/Columbia/Maha")

bins <- 7 # Number of distance bins (.5 microns).
iters <- 5 # Number of iterations (time).
mols <- 7000 # Number of molecules released.

build_rwalk <- function(bins = 7, iters = 5, mols = 7000, smooth = 4) {
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
