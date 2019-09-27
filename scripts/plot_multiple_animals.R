library(dplyr)
library(stringr)

plt <- function(stim_low, stim_high, max_sec, gt) {
        dat <- stim_df %>% 
                filter(between(stimulus, stim_low, stim_high),
                       between(stim_time_sec, 0, max_sec),
                       include == TRUE,
                       genotype == gt)
        dat$animal <- factor(dat$animal)
        dat$stimulus <- factor(dat$stimulus)
        dat$genotype <- factor(dat$genotype)
        
        ggplot(dat) +
                geom_line(aes(x = stim_time_sec, y = electrode_concentration,
                              color = animal)) +
                labs(title = str_c(toupper(gt), " by Stimulus")) +
                facet_wrap(. ~ stimulus)
}

plt(1, 3, 5, "wt")
plt(4, 5, 15, "wt")
plt(6, 10, 15, "wt")
plt(11, 15, 15, "wt")
plt(16, 20, 20, "wt")
plt(21, 25, 25, "wt")
plt(26, 30, 25, "wt")

plt(1, 3, 5, "ko")
plt(4, 5, 15, "ko")
plt(6, 10, 15, "ko")
plt(11, 15, 15, "ko")
plt(16, 20, 20, "ko")
plt(21, 25, 25, "ko")
plt(26, 30, 25, "ko")

