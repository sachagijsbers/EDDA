#### A)

mean(Ice_cream$video)

### TEST FOR NORMALITY
# The data is normally distributed as it is roughly a “bell-shape”
hist(Ice_cream$video, col='lightblue', main='Histogram of video data')
# The data is normally distributed as it is roughly a “bell-shape”

# is normally distributed as the points fall along a straight diagonal line
qqnorm(Ice_cream$video, main='QQ-plot of video data')
qqline(Ice_cream$video)

# The p-value is not less than .05, which indicates that the data is normally distributed.
shapiro.test(Ice_cream$video)

### Construct a bounded 97%-CI for mean. 

### Evaluate the sample size needed to provide that the length of the 97%-CI is at most 3. 
### Compute a bootstrap 97%-CI for and compare it to the above CI.
