library(tidyverse)
library(ggplot2)
library(readr)


setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8")


#########       for multi sac images: make sac and type columns manually   Add a column that is pH6_length
larva_11 <- read_csv(
  "~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8/larva 11, raw.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac) %>% # so the psi x axis can be repeated over it in the other script
  group_by(sac) %>%
  mutate(pH6_area = Area[1]) %>%
  mutate(pH7_area = Area[2]) %>%
  #filter(pH == 8) %>%
  write_csv(
    "~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)/pH8/area results, thresh calc/larva 11.csv") %>%
  print()

# ## to have the pH 6 and 7 sizes somewhere
# larva_2_pre <- read_csv(
#   "~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8/larva 2, raw.csv") %>%
#   rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
#   arrange(sac) %>% # so the psi x axis can be repeated over it in the other script
#   filter(!pH == 8) %>%
#   write_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)/pH8/pH6, 7/larva 2_pre.csv") %>%
#   print()

########     when larva is over two image sets     #######

# bring all sacs from one larva into a single df, arranged by sac 
# so that the psi x axis can still be created along with the larger df
#########       after manually assigning sac number and type       #############
larva_8a <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8/larva 8a, raw, v2.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_8a)

larva_8b <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8/larva 8b, raw, v2.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_8b)

# combine to one df for larva
larva_8 <- rbind(larva_8a, larva_8b) %>%
  group_by(sac) %>%
  mutate(pH6_area = Area[1]) %>%
  mutate(pH7_area = Area[2]) %>%
  #filter(pH == 8) %>%
  write_csv(
    "~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)/pH8/area results, thresh calc/larva 8.csv") %>%
  print()

