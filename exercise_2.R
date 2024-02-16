# Import the data from the file "hemoglobin.txt"
# library(readr)
# hemoglobin_df <- read_table("hemoglobin.txt")
View(hemoglobin_df)

# Load necessary libraries
library(ggplot2)
# Create the boxplot to visualize data
ggplot(hemoglobin_df, aes(x = factor(rate), y = hemoglobin, fill = factor(rate))) + 
  geom_boxplot() + 
  facet_wrap(~ method, scales = "free_x") +
  labs(x = "Rate", y = "Hemoglobin (g/100 ml)", fill = "Rate") +
  theme_minimal()


### a) 

# Get all unique values from rate column in hemoglobin
rates <- unique(hemoglobin_df$rate)
I <- length(rates)
# Get all unique values from method column
methods <- unique(hemoglobin_df$method)
J <- length(methods)

# Number of columns in the data
N <- 80/(I*J)

# Randomization in R (from slides)
rbind(rep(1:I, each=N*J), rep(1:J, N*I), sample(1:(N*I*J)))

### b)

boxplot(hemoglobin_df$hemoglobin ~ hemoglobin_df$rate)
boxplot(hemoglobin_df$hemoglobin ~ hemoglobin_df$method)

# Two-Way ANOVA for hemoglobin data
hemoglobin_df$rate <- as.factor(hemoglobin_df$rate)
hemoglobin_df$method <- as.factor(hemoglobin_df$method)

hemoglobin_aov <- lm(hemoglobin ~ rate*method, data = hemoglobin_df)
anova(hemoglobin_aov)

# The p value for testing H0: alpha_i = 0 for all i is 2.404e-09. 
# Therefore, we reject the null hypothesis and conclude that the rate of hemoglobin 
# is not the same for all methods.

# The p value for testing H0: beta_j = 0 for all j is 0.2161.
# Therefore, we fail to reject the null hypothesis and conclude that the two methods 
# of sulfamerazine is the same for all rates.

# The p value for testing H0: gamma_i*j = 0 for all (i, j) is 0.3769.
# So, there is no evidence for interaction.

# Summary of test using treatment parameterization (alpha_1 = 0)
summary(hemoglobin_aov)

# The rate of hemoglobin seems to have a significant effect on the hemoglobin level
# and thereby has a greater influence on the hemoglobin level.  

### c)

# additive model
hemoglobin_aov_2 <- lm(hemoglobin ~ rate+method, data = hemoglobin_df)
anova(hemoglobin_aov_2)

# The p value for testing HA: alpha_i = 0 for all i is 2.02e-09.
# The p value for testing HB: beta_j = 0 for all j is 0.2163.
# So only rate has a main effect in the additive model. 

# Concluding from the summary of hemoglobin_aov, the 
# combination of rate 2 with method B yields the 
# highest hemoglobin level.

# Mean hemoglobin value for rate 3 by using method A 
# from the summary of hemoglobin_aov
7.200 + 1.83

# Mean hemoglobin value for rate 2 by using method B
7.200 + 2.13 - 0.45 + 1.26

# Checking the result
mean(hemoglobin_df$hemoglobin[hemoglobin_df$rate == 2 & hemoglobin_df$method == "B"])

# Rate 2 has the highest hemoglobin level when using any method.

# d) 
