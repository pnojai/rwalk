library(openxlsx)
library(dplyr)

par_dir <- "./scripts"           # File params need a trackable directory

# File parameters, on GitHub
fil_params <- read.xlsx(paste(par_dir, "file_params_wip.xlsx", sep = "/"))

head(fil_params)

sweep_wt <- subset(fil_params, genotype == "wt")
sweep_ko <- subset(fil_params, genotype == "ko")

sweep_wt
sweep_ko

apply(sweep_wt[1:3, 5:7], 2, mean, na.rm = TRUE)
apply(sweep_wt[4:30, 5:7], 2, mean, na.rm = TRUE)
apply(sweep_ko[1:3, 5:7], 2, mean, na.rm = TRUE)
apply(sweep_ko[4:15, 5:7], 2, mean, na.rm = TRUE)

# Using dplyr

# Before amphetamine
select(fil_params, stimulus:km) %>%
        filter(amph == 0) %>%
        group_by(genotype) %>%
        summarize(mean(release),
                  mean(vmax),
                  mean(km))

# With amphetamine
select(fil_params, stimulus:km) %>%
        filter(amph == 1) %>%
        group_by(genotype) %>%
        summarize(mean(release),
                  mean(vmax),
                  mean(km))
