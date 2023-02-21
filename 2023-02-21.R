# SEP
# 2023-02-21
# Intro to t-tests and Correlations

library(palmerpenguins)
library(tidyverse)
library(rstatix)

head(penguins)
ggplot(data = penguins) +
  geom_histogram(aes(x=body_mass_g, fill = species))

# one-sample t-test
gentoo = penguins %>%
  filter(species == "Gentoo")
head(gentoo)

ggplot(data= gentoo) +
  geom_histogram(aes(x=body_mass_g))

ggplot(data = gentoo)+
  stat_qq(aes(sample= body_mass_g))
# a perfect distribution is a 1:1 line

gentoo %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE), 
            sd_body_mass_g = sd(body_mass_g, na.rm = TRUE))
# 5076g + or - 504

# run t test:
# mu is the literature derived value I am comparing my mean to 
t.test(gentoo$body_mass_g, mu=5500)
# t = -9.3276, df = 122, p-value = 6.051e-16
# statistically different than the literature derived value
# Null hypothesis rejected

t_test_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu = 5500)
# results are now in a data frame. A lot easier to pull from. 

# two_sample t-test
# are the gentoo or the adelie penguins heavier or similar
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"), 
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() # gets rid of chinstrap = 0 

summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>% #group before you summarize
  summarize(mean= mean(body_mass_g), 
            sd = sd(body_mass_g))

ggplot(data = data_for_t_test) + 
  stat_qq(aes(sample= body_mass_g)) +
  facet_wrap(~species, scales = "free")
#scales can be free or fixed

# t-test two samples has one assumption: the equality of the variances are the same
# way to test if the variances of the two distributions are similar is the levene's test

data_for_t_test %>%
  levene_test(body_mass_g ~ species)
# body mass as a function of species
# if the p-value is small < 0.05 then the variances are unequal
# if the p-value is larger so we can accept the null that the variance between the two samples are similar
# different statistic is the welches t-test if you have unequal variances

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = TRUE)
# look at the body mass of the species are the means of the distribution similar
# says welch two sample t-test, we can trust it because it is a more conservative test
# if welch is statistical different than the students t-test will be statistical different
# var.equal = TRUE overrides the welchs t-test

# Correlations

ggplot(data = gentoo) +
  geom_point(aes(x= bill_length_mm, y= bill_depth_mm))

ggplot(data= gentoo) +
  stat_qq(aes(sample= bill_length_mm))

ggplot(data= gentoo) +
  stat_qq(aes(sample= bill_depth_mm))

cor(x=gentoo$bill_length_mm, y= gentoo$bill_depth_mm, use = "complete.obs")
# use function is only use rows where none of the columns have NAs
# 0.64 correlation, correlation of 1, you have done something wrong, you have plotted the same data twice.
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use = "complete.obs")
# more complete test for statistics but the correlation is still the same

gentoo %>% 
  cor_test(bill_length_mm, bill_depth_mm)
# this puts your statistics data in a table version for when you have multiple tests

head(gentoo)
#correlation matrix all the rows and columns 3 4 5 and 6
cor(gentoo[,c(3:6)], use="complete.obs")

library(GGally)

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()
# correlation between the variables, scatter plots, and density distribution for each of our variables
# *** means the correlations are statistically significant 

#pretty
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>% # clever way to select variables with names that end in "_mm"
  GGally::ggpairs(aes(color = species))

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color = species))









