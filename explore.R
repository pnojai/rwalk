# Exported sheets to CSV.

# Load data files
data_path <- "./Data/"

amph_test <- read.csv(paste(data_path, "AMPH_test.csv", sep = ""))
timestamps_wt_wt <- read.csv(paste(data_path, "Timestamps_WT_WT.csv", sep = ""))
timestamps_wt_amph <- read.csv(paste(data_path, "Timestamps_WT_AMPH.csv", sep = ""))
timestamps_wt_pre_amph <- read.csv(paste(data_path, "Timestamps_WT_PRE_AMPH.csv", sep = ""))

# What kind of data structure.
class(amph_test)

# Look at the top of each data frame. Tidy, if necessary.
head(amph_test)

# A couple of problems.
# amph_test has no header in the file. read.csv() assumes a header, so the first data point became
# the column name.
# How many observations?
nrow(amph_test)

# The file has 399 rows, but the data frame only has 398. Load it again.
amph_test <- read.csv(paste(data_path, "AMPH_test.csv", sep = ""), header = FALSE)

# Take a look.
head(amph_test)

# That's better. The column name defaults to V1. I want to name this myself when I write the
# data code book. Set something.
# Look at the name of the second dimension 
dimnames(amph_test)[[2]]

# Assign my own name
dimnames(amph_test)[[2]] <- "MyVar"

# Look at the dimension name again.
dimnames(amph_test)[[2]]

# Check the top of the data frame
head(amph_test)

# Good. And the observation count.
nrow(amph_test)

# END: AMPH_TEST

head(timestamps_wt_wt) # OK
head(timestamps_wt_amph) # OK

# BEGIN: TIMESTAMPS_WT_PRE_AMPH
head(timestamps_wt_pre_amph)

# Looks like the first two columns are white space.
# Let's figure this out in code instead of opening the files.
timestamps_wt_pre_amph_cnt <- nrow(timestamps_wt_pre_amph)

timestamps_wt_pre_amph_cnt

# The arithmetic value of TRUE is 1, so you can total up the results of is.na() to see how
# many missing values there are.
sum(is.na(timestamps_wt_pre_amph[[1]]))

# Compare that to the row count. The difference is how many data points there are.
timestamps_wt_pre_amph_cnt - sum(is.na(timestamps_wt_pre_amph[[1]]))

# No data in the first column. How about the second.
timestamps_wt_pre_amph_cnt - sum(is.na(timestamps_wt_pre_amph[[2]]))

# Just to demonstrate the point, how many values are in the third column.
timestamps_wt_pre_amph_cnt - sum(is.na(timestamps_wt_pre_amph[[3]]))

# Let's scrub the first two columns off that data frame
timestamps_wt_pre_amph <- timestamps_wt_pre_amph[[3]]

# Take a look
head(timestamps_wt_pre_amph)

# Looks like a vector now, instead of a data frame.
class(timestamps_wt_pre_amph)

# Yes, it is. Coerce the data type.
timestamps_wt_pre_amph <- as.data.frame(timestamps_wt_pre_amph)

class(timestamps_wt_pre_amph)

# It's a data frame. Look at the top.
head(timestamps_wt_pre_amph)

# Shucks, lost that nice column name in the file header.
# Let's try this again, but instead of extracting the column as a vector, try cbind(),
# which is a function intended for data frames.
timestamps_wt_pre_amph <- read.csv(paste(data_path, "Timestamps_WT_PRE_AMPH.csv", sep = ""))
timestamps_wt_pre_amph <- cbind(timestamps_wt_pre_amph[[3]])

# Class and head
class(timestamps_wt_pre_amph)
head(timestamps_wt_pre_amph)

# Got me a matrix. I could name the matrix column dimension, but I would like to know how to
# read just the third column as data frame, preserving its header. Never had to do that.
# Let's look at the man page for readcsv().
?read.csv()

# You can drop columns with negative indexing.
timestamps_wt_pre_amph <- read.csv(paste(data_path, "Timestamps_WT_PRE_AMPH.csv", sep = ""))
timestamps_wt_pre_amph <- timestamps_wt_pre_amph[ -c(1:2)]

class(timestamps_wt_pre_amph)
head(timestamps_wt_pre_amph)

# That did it. How does that index format know that the vector refers to columns?

# END: TIMESTAMPS_WT_PRE_AMPH

# Check for missing values
nrow(amph_test) - sum(complete.cases(amph_test))
nrow(timestamps_wt_wt) - sum(complete.cases(timestamps_wt_wt))
nrow(timestamps_wt_amph) - sum(complete.cases(timestamps_wt_amph))
nrow(timestamps_wt_pre_amph) - sum(complete.cases(timestamps_wt_pre_amph))

# Check the top of the incomplete cases
head(timestamps_wt_wt[!complete.cases(timestamps_wt_wt), ])

# timestamps_wt_wt appears to have stopped collecting observations for variable WT_170427_T1C1
# after observation 1008.

head(timestamps_wt_amph[!complete.cases(timestamps_wt_amph), ])

# timestamps_wt_amph appears to have stopped collecting observations for variable AMPH_170522_T1C1
# after observation 1008.

