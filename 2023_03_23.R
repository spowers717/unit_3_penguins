# SEP
# 2023-03-23
# Principle Component Analysis

library(tidyverse)
library(palmerpenguins)

head(penguins)

pen_drop_na = penguins %>%
  drop_na()

# separate the numeric columns from the factor columns

pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(pen_num)

# want the metadata or factor data

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)  

# run the PCA

pen_pca = prcomp(pen_num, scale. = T, center = T)

# scale normalizes our variables (3000 g to swamp the bill depth) contribute equally
# center means that our PCs are going to be centered over zero

pen_pca

# list of standard deviations for each PC's 
# first one will be the biggest because the first one explains the majority of the data
# how each of the values contribute to the loading of the other variables
# each one is a correlation with the PC
# flipper length and body mass do not contribute much to PC2

summary(pen_pca)

# first row has standard deviation 
# SD 1 ^2/ (SD 1 ^2 + SD 2 ^2 ...SD 4 ^2)
# PC 1 explaining 68 % of the variation
# add them all up and they will equal to 1
# proportion of variance is very important and including all of them is important
# plot pc1 and pc2 to explain your data, you are showing 88 % of the data and you have to make that clear

str(pen_pca)

# tells you the structure of an object
# want to access certain things you can access them with $sdev
# used to ask what are the different variables associated with this object
# x is going to be the values of the principle components

pen_pca$sdev

dim(pen_num)
dim(pen_pca$x)
head(pen_pca$x)

# size of your full PC matrix will be the same size as the data set you input
# first row corresponds with the first row in the penguin dataset

str(summary(pen_pca))
summary(pen_pca)$importance
summary(pen_pca)$importance[2,]

# hand calculate proportion of variance from Sdevs

sum(pen_pca$sdev^2)
(pen_pca$sdev)^2/sum(pen_pca$sdev^2)

# scree plot

plot(pen_pca)

pca_scree = data.frame(pc = c(1:4),
                       var = summary(pen_pca)$importance[2,]) 
pca_scree

ggplot(data = pca_scree, aes(x=pc, y=var)) + 
  geom_col() +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Scree Plot") +
  ylab("Proportion of Variance Explained")

# most common way you see a scree plot

# Create biplot
head(pen_pca$x)
pen_pca_meta = cbind(pen_pca$x, pen_meta)
head(pen_pca_meta)

ggplot() +
  geom_point(aes(x=PC1, y=PC2, color = species, shape= sex), data = pen_pca_meta) +
  coord_fixed(ratio = 1)
# looks like a traditional bi-plot
# between these two PCs 88% of the data is explained
# adding color helps us understand the data better
# linear models are better in the end for this data, but this plot is also very compelling 
# seeing a diagonal pattern with sex (clear variation between sex for each of the species)
# distance between 0 and 1 is the same on both axes, ggsave will preserve it that way you are not visually biasing one axis over another

# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

biplot(pen_pca)
# comes from base r stats package
# number corresponds to the row
# plots vectors from the rotation matrix (loadings or weights) how much your original variables relate to the PCs

ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T)
ggbiplot(pen_pca, scale = 1, groups = pen_meta$sex, ellipse = T) # not as successful 
# can customize it with the tools we know from ggplot

ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T, alpha = 0)
# alpha is the transparency 1 is not 0 is transparent

ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T, alpha = 0) +
  geom_point(aes(color = pen_meta$species, shape=pen_meta$sex)) + 
  xlim(-2.5,3)
# three of our pca variables are pointed in the positive direction and one is in the negative direction which compares to the loading's table 
# flipper length has almost zero weight on PC2 so it is close to zero
# PC 2 is explaining most of the variation in sex 
# therefore, most of the variation in sex can be explained by the beak (bill depth and bill length)
# PC 2 also explains the variation in adelie and chinstrap so beak accounts for their variation
pen_pca

# look at PC 3 and PC 4
ggbiplot(pen_pca, scale = 1, groups = pen_meta$species, ellipse = T, choices = c(3,4))
# great example of a lousy PCA
# PCA analysis does not tell us anything interesting about this data
# explaining 12% of the variation in your data












