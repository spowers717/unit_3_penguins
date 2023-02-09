# 2023-02-09
# SEP

library("tidyverse")
# Conflict is when a package is overriding a current package
# Hiding the the same function filter in the stats package
# If we call the function filter, the dplyr package will be used instead

tidyverse_packages()
# Gives a list of the packages loaded with tidyverse

head(iris) 
# Data frame that exists in the dataset's package

install.packages("palmerpenguins")
library("palmerpenguins")
head(penguins)
summary(penguins)
glimpse(penguins)
class(penguins)


mean(penguins$bill_depth_mm, na.rm = TRUE)

filter() #chooses rows based on column values.
arrange() #changes the order of the rows.
select() #changes whether or not a column is included.
rename() #changes the name of columns.
mutate() #changes the values of columns and creates new columns.
summarize() #collapses a group into a single row.
group_by() #group data into rows with the same values.
ungroup() #remove grouping information from data frame.
distinct() #remove duplicate rows.


head(penguins)
# filter by species
gentoo = filter(.data=penguins, species=="Gentoo")
head(gentoo)
summary(gentoo)

gentoo_ladies= filter(penguins, species=="Gentoo", sex=="female")
summary(gentoo_ladies)

# pipe % > % 
# round(mean((dat)^2)) becomes really hard to read
# shorthand it and say h(g(f(x)))
# Hadley's pipe is used to instead say 
# x % > %
# f() % > %
# g() % > %
# h() % > %
# pipes data to the next line

gentoo_ladies = penguins %>%
  filter(species== "Gentoo") %>%
  filter(sex== "female")
summary(gentoo_ladies)

mean_ladies_mass = penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g = mean(body_mass_g))

# base R
mean_ladies_mass = mean(penguins$body_mass_g[penguins$sex=="female"], na.rm=TRUE)
# harder to read and very repetitive 

species_sex_mass = penguins %>%
  filter(!is.na(sex)) %>% # filters out the NAs
  group_by(species, sex) %>% # group by species and sex
  summarize(mean_mass_g = mean(body_mass_g)) # calculate the means

write_csv(x=species_sex_mass, file = "/data/species_sex_mass.csv")

species_sex_count = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(count = n())


species_count = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species) %>%
  summarize(count = n())

species_count = penguins %>%
  #filter(!is.na(sex)) %>%
  group_by(species) %>%
  summarize(count = n())

penguins_for_america = penguins %>% 
  mutate(body_mass_lb = body_mass_g* 0.0022)
glimpse(penguins_for_america)

penguins %>%
  distinct(island)

for_my_advisor = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)
# everything except these two variables

penguins %>%
  arrange(body_mass_g)
# in order of smallest to biggest
penguins %>%
  arrange(desc(body_mass_g))
# larger penguins first

# Exercise 1.3

bill_length_dream_biscoe = penguins %>% 
  filter(species== "Adelie", 
         island %in% c("Biscoe", "Dream"), 
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_inches = bill_length_mm * 0.039) %>%# 0.039 inches/mm
  summarize(mean_bill_length = mean(bill_length_mm), 
            )

  
  
  
 

  

  

















































