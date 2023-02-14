# SEP
# 2023-02-14

library(tidyverse)
library(palmerpenguins)

head(penguins)
find("filter")
stats::filter()
dplyr::select() # specifically call the package you want to use for that function

penguins_without_nas = penguins %>%
  filter(!is.na(flipper_length_mm))

ggplot(data = penguins) + 
  geom_point(aes(x= flipper_length_mm, 
                 y=body_mass_g, 
                 color = species, 
                 shape= species, )) #scatter plot, aes= aesthetics 

ggplot(data = penguins) + 
  geom_point(aes(x= flipper_length_mm, 
                 y=body_mass_g, 
                 color = species, 
                 shape= sex )) +
  geom_smooth(aes(x= flipper_length_mm, 
                  y=body_mass_g)) +
  theme_bw() # gets rid of grey background


ggplot(data = penguins) + 
  geom_point(aes(x= flipper_length_mm, 
                 y=body_mass_g, 
                 color = species, 
                 shape= sex )) +
  theme_classic() # gets rid of grey background and grid lines

my_plot= ggplot(data = penguins) + 
  geom_point(aes(x= flipper_length_mm, 
                 y=body_mass_g, 
                 color = species, 
                 shape= species )) +
  geom_smooth(aes(x= flipper_length_mm, 
                  y=body_mass_g)) +
  xlab("Flipper Length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute") +
  theme_classic() # gets rid of grey background

ggsave(my_plot, 
       filename = "figures/flipper_v_mass.png", 
       width = 7, 
       height = 5, 
       units= "in", 
       dpi = 300)

#png keeps the quality of the image and dpi increases the resolution for ppts 

head(penguins)

penguins_ts = penguins %>%
  group_by(year) %>%
  summarize(num_penguins = n()) 
# group by and then give me characteristics of each year

penguins_ts = penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins = n())
# every combination of year and species 

ggplot(data= penguins_ts) +
  geom_line(aes(x=year, y=num_penguins, color= species))
# color function says give me a line for each species instead of one line
# how penguin collection of species changed over time

ggplot(penguins) +
  geom_histogram(aes(x= flipper_length_mm))
# bimodal distribution because of species

ggplot(penguins) +
  geom_histogram(aes(x= flipper_length_mm, fill= species))
# color function outlines the bars and fill function fills the bars with color

ggplot(penguins) +
  geom_histogram(aes(x= flipper_length_mm, fill= species, color = species), 
                 position= "identity", 
                 alpha = 0.5)
# do not stack and fade

ggplot(penguins) +
  geom_histogram(aes(x= flipper_length_mm, fill= species), 
                 position= "identity", 
                 alpha = 0.5) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))
# changing the colors and removing the outline

ggplot(data = penguins) + 
  geom_boxplot(aes(y= flipper_length_mm))

ggplot(data = penguins) + 
  geom_boxplot(aes(y= flipper_length_mm, x= species))
# adding species

ggplot(data = penguins) + 
  geom_boxplot(aes(y= flipper_length_mm, x= species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), 
              width = 0.2) +
  ylab("Flipper Length (mm)") +
  xlab("Species") 
# puts points on our box blot, jitter randomly jiggles a point so we can see each individuals flipper length, width parameter is how wide you want your data to wiggle horizontally

ggplot(data = penguins) +
  geom_bar(aes(x=sex, fill= species))
# similar ratio of male to female for all of our species, fill distinguishes in one single bar

ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1) 
# ~ means function in base r, makes its own panel of something

ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 3) 
# change the layout of the three panels

ggplot(data = penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) + 
  coord_flip()
# add three rows and once column and flip the graph  
  
  

