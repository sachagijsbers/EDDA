#### A)
## import dataset:
# library(readr)
# Ice_cream <- read_csv("Ice_cream.csv", col_types = cols(ice_cream = col_skip()))

mean_video = mean(Ice_cream$video)

### TEST FOR NORMALITY
# The data is normally distributed as it is roughly a “bell-shape”
hist(Ice_cream$video, col='lightblue', main='Histogram of video data')
# The data is normally distributed as it is roughly a “bell-shape”

# is normally distributed as the points fall along a straight diagonal line
qqnorm(Ice_cream$video, main='QQ-plot of video data')
qqline(Ice_cream$video)

# The p-value is not less than .03, which indicates that the data is normally distributed.
shapiro.test(Ice_cream$video)

# Construct a bounded 97%-CI for mean.
t.test(Ice_cream$video, conf.level = 0.97)
# make variable for the interval
CI <- t.test(Ice_cream$video, conf.level = 0.97)$conf.int
CI_low <- CI[1]
CI_high <- CI[2]
CI_length <- CI_high - CI_low

# sample size of ice_cream$video
n <- length(Ice_cream$video)

# Evaluate the sample size needed to provide that the length of the 97%-CI is at most 3. 
# ????????



mean_function <- function(data, indices) {
  # Sample the data according to indices and calculate the mean
  sample_data <- data[indices]
  mean(sample_data)
}

# Compute a bootstrap 97%-CI for and compare it to the above CI.
library(boot)
reps <- boot(data=Ice_cream$video, statistic=mean_function, R=1000)

plot(reps)

# Calculate the 97% CI from the bootstrap results
ci <- boot.ci(reps, type="perc", conf=0.97)$percent[4:5]

# Print the CI
print(ci)
CI_low_boot <- ci[1]
CI_high_boot <- ci[2]
CI_length_boot <- CI_high_boot - CI_low_boot

print(CI_length)
print(CI)
print(CI_length_boot)
print(ci)
