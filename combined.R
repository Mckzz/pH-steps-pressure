library(tidyverse)
library(ggplot2)
library(readr)

setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8")

setwd("./area results, thresh calc")


# the list of file names will provide the files for r to read in, as well as 
# the names for the indvd larva column
list_of_results <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(list_of_results)
#rm(list_of_results)

# read in files, modify text in larva column, add psi column for x axis
larvae.df <- read_delim(list_of_results,
                        delim = ",",
                        id = "larva") %>%
  mutate(" " = NULL) %>%
  filter(pH == 8) %>%
  mutate(larva = substr(larva, 1, nchar(larva) - 4)) %>% #drop file extension
  group_by(larva, sac) %>%
  mutate(psi = (row_number() * 30) - 30) %>% # x axis starting from zero
  mutate(area_set_0psi = Area - Area[1]) %>% # still abs size, but zeroed
  mutate(pct.area = ((Area - Area[1]) / Area[1]) * 100) %>%
  mutate(pct.pH6 = ((Area - pH6_area) / pH6_area) * 100) %>% # size as a percentage of size at pH 6
  mutate(pct.pH7 = ((Area - pH7_area) / pH7_area) * 100) %>% # "  pH 7
  ungroup() %>%
  group_by(psi) %>%
  mutate(mean_pct_area = mean(pct.area)) %>% # mean for all sacs at a psi
  mutate(sd_pct_area = sd(pct.area)) %>%
  mutate(sac = as_factor(sac))

print(larvae.df)

# % change
ggplot(data = larvae.df, 
       aes(x = psi,
           group = interaction(larva, sac),
           colour = larva)) +
  geom_point(aes(y= pct.pH6), size = 2, alpha = 1) +
  geom_line(aes(y= pct.pH6), alpha = 1) +
  geom_hline(yintercept = 0) #+
  # geom_point(aes(y= mean_pct_area), size = 3, colour = "black") +
  # geom_line(aes(y= mean_pct_area), colour = "black")



# absolute sizez
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  #  geom_point(aes(y= Area, size = 2)) +
  geom_point(aes(y= Area)) +
  geom_line(aes(y= Area))


# difference from 0 psi
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  #  geom_point(aes(y= area_set_0psi, size = 2)) +
  geom_point(aes(y= area_set_0psi)) +
  geom_line(aes(y= area_set_0psi))



###############   looking at/ comparing individual larvae   ###############

larva_choose <- larvae.df %>%
  filter(larva == "larva 3" | larva == "larva 8")
print(larva_choose)

# % change from pH 6
ggplot(data = larva_choose, 
       aes(x = psi,
           group = interaction(larva, sac),
           colour = larva,
           shape = sac), na.rm = F) +
  geom_point(aes(y= pct.pH6), size = 2, alpha = 1) +
  geom_line(aes(y= pct.pH6), alpha = 1) +
  geom_hline(yintercept = 0)

# absolute sizez
ggplot(data = larva_choose, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= Area), size = 1) +
  geom_line(aes(y= Area))


#####################      model?     ###################

install.packages("ggformula")
library(ggformula)

ggplot(larvae.df, aes(y = pct.pH6, x = psi)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_spline(col = "black", df = 4) +
  theme_minimal()


quad_mod <- lm(pct.pH6 ~ (psi + I(psi^2)) + type, larvae.df)

anova(quad_mod)



ggplot(data = larvae.df,
       aes(x = psi,
           y = pct.pH7,
           shape = type,
           linetype = type)) +
  scale_shape_manual(values=c(1, 6)) +
  geom_jitter(size = 3.5, width = 0.01) +
  geom_smooth(method = "lm", 
              formula = y ~ (x + I(x^2)),
              color = '#555555') +
  #geom_line(aes(x = pH, y = fit), show.legend = FALSE) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, show.legend = FALSE) +
  # annotate("rect",
  #          xmin = sac$pH - sac$pH.sd,
  #          xmax = sac$pH + sac$pH.sd,
  #          ymin = -30, ymax = -25, fill = "black") +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))



