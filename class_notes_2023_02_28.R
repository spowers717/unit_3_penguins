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







