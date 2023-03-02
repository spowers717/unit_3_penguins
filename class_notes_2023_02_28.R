# SEP
# 2023-02-28
# Multiple regression 

# categorical variables:
# factor
# nominal 
# discrete
# In R you will assign them to be a factor data type 
# as.factor(penguins$year) treat it as a categorical variable instead of numerical 

library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)


head(penguins)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
dim(penguins_lm_3)

# build model

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)
summary(lm_3)
coef(lm_3)
coef(lm_3)[1]
anova(lm_3) # we have continous not categorical, not best way
broom::tidy(lm_3)
my_results = broom::tidy(lm_3)
my_results$estimate
my_results = broom::tidy(lm_3, conf.int= TRUE, conf.level= 0.95) %>%
  mutate_if(is.numeric, round, 2)
my_results

# Visualize model
ggPredict(lm_3, se=TRUE, interactive = TRUE)

lm_3_predictions = predict(lm_3)
head(lm_3_predictions)
head(penguins_lm_3)

lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
lm_3_predictions

penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

ggplot(data = penguins_lm_3_predict, aes(x= bill_length_mm, y= bill_depth_mm, color=species)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species, color=NULL), alpha= 0.5) +
  geom_point() +
  geom_line(aes(y=fit)) 


# generate new data

newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm), 
                             by= 0.1)
head(newdata_bill_length_mm)
tail(newdata_bill_length_mm)

newdata = expand.grid(bill_length_mm= newdata_bill_length_mm, 
                      species=unique(penguins_lm_3$species))
head(newdata)
tail(newdata)
summary(newdata)


newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata = newdata, interval = "confidence"))
head(newdata_predict_lm_3)

ggplot() +
  geom_point(data = penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(data=newdata_predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, fill = species), alpha=0.5) +
  geom_line(data = newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species))

lm_3_predict = lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=TRUE, interval= "confidence")
glimpse(lm_3_predict)



# generate new data pt 2

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)

lm_3_predict = lm_3 %>%
  broom::augment(newdata = newdata, se_fit= TRUE, interval= "confidence")
head(lm_3_predict)

# visualize 
ggplot() +
  geom_point(data = penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(data=newdata_predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, fill = species), alpha=0.5) +
  geom_line(data = newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species))

# interaction term

lm_4 = lm(bill_depth_mm ~bill_length_mm + species + bill_length_mm:species, 
          data= penguins_lm_3)
lm_4 = lm(bill_depth_mm ~bill_length_mm:species, # bad
          data= penguins_lm_3)
# this code is saying the same thing as the first model
lm_4 = lm(bill_depth_mm ~bill_length_mm * species, 
          data= penguins_lm_3)
summary(lm_4)
# added model complexity does not benefit you as far as improving the predictions adjusted r square is basically the same

AIC(lm_3, lm_4)
# AIC smaller is better
# AIC 2 units smaller to be considered a better model
# lm_3 is 3 AIC units less than lm_4 
step(lm_4)
# removes variables and asks if the model is better. Keeps going to find the best fit model with the lowest complexity. 
best_model = step(lm_4)
summary(best_model)

#plot with interaction

lm_4_predict = lm_4 %>% #recall this is our bad model
  broom::augment(interval = "confidence")
head(lm_4_predict)

ggplot(data= lm_4_predict) +
  geom_point(aes(x= bill_length_mm, y= bill_depth_mm, color = species)) +
  geom_line(aes(y=.fitted, x= bill_length_mm, color = species)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill = species), alpha=0.5)

# depth ~ bill_length + flipper_length

library(car) #VIF() variance inflation factor to make sure we don't have multi-colinearity 

gentoo = penguins %>%
  filter(species=="Gentoo")


lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
# lm_gentoo_3 has the best model
step(lm_gentoo_3)
# says to keep the model
vif(lm_gentoo_3)

head(penguins_lm_3)

newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm = TRUE),
         body_mass_g = median(gentoo$body_mass_g, na.rm = TRUE))

head(newdata)
lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot(data= lm_gentoo_3_predict) +
  geom_point(aes(x= bill_length_mm, y=bill_depth_mm), data = gentoo) +
  geom_line(aes(y= .fitted, x = bill_length_mm)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm), alpha=0.5) +
  annotate("text",  x= 57, y=13.5, 
          label= paste0("flipper length = ", 
          median(gentoo$flipper_length_mm, na.rm = TRUE), " mm")) +
  annotate("text",  x= 57, y=14, 
          label= paste0("body mass = ", 
          median(gentoo$body_mass_g, na.rm = TRUE), " g"))


# 5.3

newdata = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm = TRUE),
         body_mass_g = median(gentoo$body_mass_g, na.rm = TRUE))
head(newdata)

head(newdata)
lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot(data= lm_gentoo_3_predict) +
  geom_point(aes(x= flipper_length_mm, y=bill_depth_mm), data = gentoo) +
  geom_line(aes(y= .fitted, x = flipper_length_mm )) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=flipper_length_mm), alpha=0.5) +
  annotate("text",  x= 225, y=13.5, 
           label= paste0("bill length = ", 
                         median(gentoo$bill_length_mm, na.rm = TRUE), " mm")) +
  annotate("text",  x= 225, y=13.7, 
           label= paste0("body mass = ", 
                         median(gentoo$body_mass_g, na.rm = TRUE), " g")) +
  theme_classic()

# ANOVA
penguin_lm = lm(body_mass_g~species + sex, data = penguins)
anova(penguin_lm)

penguin_anova = aov(body_mass_g ~species + sex, data = penguins)
summary(penguin_anova)

TukeyHSD(penguin_anova)

  



