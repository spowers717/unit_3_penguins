# SEP
# 2023-02-23
# Linear Models

# y = mx + b
# y = b0 + b1 * x1 + b2 * x2
# residual = y- - y

# Linear regression has 5 key assumptions:

# Linear relationship
# Normality of the model residuals
# No or little multicollinearity
# No auto-correlation (samples are independent)
# Homoscedasticity (homogeneity of variance in residuals across the independent variables)

library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(broom)  # tidy() augment() #does NOT load with tidyverse

# depth ~ length 

head(penguins)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally:: ggpairs(aes(color=species))

penguins %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

# linear model
lm(bill_depth_mm ~ bill_length_mm, data= penguins)
# intercept = 20.8, slope = -0.085
# not a lot of information, save as a variable name instead

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data= penguins)
class(lm_1)
summary(lm_1) # shows much more information
# std error, t values and p-values (very small)
# just because your p-value is below 0.05 does not mean it makes sense, all of our species are lumped together 
# a hint is in our R squared values 5% 
# r squared adjusted accounts for complexity 5%

ggplot(data = penguins, aes(x=bill_length_mm, y=bill_depth_mm)) + 
  geom_point() +
  geom_smooth(method = "lm")

plot(lm_1)
# homoscedasticity good hetero bad
# when the value of y is cmall then the error is higher
# press enter again and we get a Q-Q plot looks pretty good
# press enter again implies that error is less when beaks are bigger
# 4th plot want it to go from wide to narrow

# Residuals vs Fitted Values, to check homogeneity of variance in residuals and linearity of association between predictors and outcome (look for a relatively straight line and random-looking scatterplot). By default, the 3 points with the highest residuals are labeled (i.e. the row number is printed on the figure).
# Normal Q-Q Plot, to check the assumption of normally distributed residuals.
# Root of Standardized residuals vs Fitted values, this is very similar to number 1, where the Y axis of residuals is in a different metric.
# Residuals vs Leverage, to check if the leverage of certain observations are driving abnormal # residual distributions, thus violating assumptions and biasing statistical tests.


gentoo = penguins %>%
  filter(species== "Gentoo")
gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data= gentoo )
summary(lm_2)
# p-value will be heavily influenced by sample size, this sample size is smaller so the p-value is larger
# r square value is now explaining 41% of the variance 
plot(lm_2)
# 1. smoothing line is pretty flat 
# 2. q-q pretty good
# 3. see a flat smoothing line
# 4. leverage plot, far from the zero line is not also high leverage. 

ggplot(data = gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) + 
  geom_point(data = gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_smooth(data = gentoo, aes(x=bill_length_mm, y=bill_depth_mm), method = "lm")


ggplot(data=gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_point(data = penguins %>% filter(species== "Adelie"), 
             aes(x=bill_length_mm, y=bill_depth_mm, color = species))



ggplot(data=penguins) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method = "lm") +
  geom_smooth(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm), method = "lm", color="black")
# This is an example of Simpson’s paradox, which occurs when trends that appear when a dataset is separated into groups reverse when the data are aggregated. Basically, when doing statistics, you want to use your brain and your gut to build models that make sense.


# Build a linear model predicting Gentoo bill depth as a function of flipper length. Plot the predictions. Which explanatory variable (bill length vs. flipper length) does a better job of predicting bill depth? What is your evidence?

lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data= gentoo )
summary(lm_3)
# Rsquare value of 0.4992 


plot(lm_3)

ggplot(data = gentoo, aes(x=flipper_length_mm, y=bill_depth_mm)) + 
  geom_point(data = gentoo, aes(x=flipper_length_mm, y=bill_depth_mm)) +
  geom_smooth(data = gentoo, aes(x=flipper_length_mm, y=bill_depth_mm), method= "lm")






























