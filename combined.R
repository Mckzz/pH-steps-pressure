library(tidyverse)
library(ggplot2)
library(readr)
library(lmerTest)

setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)\\pH8")

setwd("./area results, thresh calc")


# the list of file names will provide the files for r to read in, as well as 
# the names for the indvd larva column
list_of_results <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(list_of_results)
#rm(list_of_results)

length_stuff.pH.df <- larvae.df.6_7_8 %>%
  select(larva, sac, type, pH, length_calc, displacement, mean.displacement, sd.displacement, frac_length_change, mean.frac_length_change, sd.frac_length_change)

print(length_stuff.pH.df)

# read in files, modify text in larva column, add psi column for x axis
##  altered the force calculations to use airsac mean width, calculated by treating as a rectangle (2025-01-26)
larvae.df <- read_delim(list_of_results,
                        delim = ",",
                        id = "larva") %>%
  filter(!is.na(pH6_area)) %>%
  mutate(" " = NULL) %>%
  filter(pH == 8) %>%
  mutate(larva = substr(larva, 1, nchar(larva) - 4)) %>% #drop file extension
  group_by(larva, sac) %>%
  mutate(psi = (row_number() * 30) - 30) %>% # x axis starting from zero
  mutate(Pa = psi * 6894.76) %>%
  mutate(kPa = Pa/1000) %>%
  mutate(depth_m = Pa / (1000 * 9.8066)) %>%
  mutate(area_set_0psi = Area - Area[1]) %>% # still abs size, but zeroed
  mutate(pct.area = ((Area - Area[1]) / Area[1]) * 100) %>%
  mutate(pct.pH6 = ((Area - pH6_area) / pH6_area) * 100) %>% # size as a percentage of size at pH 6
  mutate(pct.pH7 = ((Area - pH7_area) / pH7_area) * 100) %>% # "  pH 7
  mutate(length_calc = pH6_length * (1 + (pct.area/100))) %>% ####   calculating length based on % area change from pH6
  mutate(sac_mean.width = Area /length_calc) %>% # constant
           #pH6_area / pH6_length) %>% # constant, slightly smaller
  mutate(displacement = length_calc - length_calc[1]) %>%
  mutate(pH.displacement8 = length_stuff.pH.df$mean.displacement[3]) %>%
  mutate(displacement.return_value = -(length_stuff.pH.df$mean.displacement[3])) %>%
  mutate(frac_length_change = displacement/length_calc[1]) %>%
  mutate(frac_length_change.return_value = -(length_stuff.pH.df$mean.frac_length_change[3])) %>%
  mutate(sac.force = (((sac_mean.width / 1000) / 2)^2) * pi * Pa) %>% # Pa calculated above (and width in mm to meters so that F = Kg * m/(s^2))
  mutate(sac.work = sac.force * (displacement/1000)) %>% # (J)
  mutate(norm.work = sac.work / (pH6_length/1000)) %>% # (J/m of pH6 sac length)
  mutate(sac.force_pH.disp = (((sac_mean.width / 1000) / 2)^2) * pi * 550000) %>% # force that would create the pH displacement for that sac. estimated from pressure plot using length change
  mutate(sac.work_pH.disp = sac.force_pH.disp * (pH.displacement8) / 1000) %>%
  mutate(norm.sac.work_pH.disp = sac.work_pH.disp / (pH6_length/1000)) %>%
  #mutate(norm.work_return.value = -((length_stuff.pH.df$mean.displacement[3])/1000) * sac.force) %>%
  ungroup() %>%
  mutate(mean.norm.sac.work_pH.disp = mean(norm.sac.work_pH.disp, na.rm = T)) %>%
  group_by(psi) %>%
  mutate(mean_pct_area = mean(pct.area)) %>% # mean for all sacs at a psi
  mutate(sd_pct_area = sd(pct.area)) %>%
  mutate(sac = as_factor(sac)) %>%
  mutate(mean.sac.force = mean(sac.force, na.rm = T)) %>%
  mutate(mean.norm.work = mean(norm.work, na.rm = T))
  #mutate(norm.work_pH_displacement = (mean.sac.force * (length_stuff.pH.df$mean.displacement[3])/1000) / (pH6_length/1000))

print(larvae.df)

# plotting change
ggplot(data = larvae.df, 
       aes(x = kPa, y= norm.work,
           group = interaction(larva, sac),
           colour = larva)) +
  geom_point(size = 2, alpha = 1) +
  #geom_line(alpha = 1) +
  #geom_spline(col = "black", df = 4, (aes(x = psi, y = pct.pH6)), inherit.aes = F) +
  geom_smooth(method = 'lm', 
              formula = y ~ splines::bs(x, df = 3, knots = 500),
              inherit.aes = F, 
              aes(x = kPa, y= norm.work), 
              color = '#555555') +
  #geom_hline(yintercept = -0.1305705) + ##  return value based on length change of going from pH 6 to 8
  geom_vline(xintercept = 550) +
  geom_hline(yintercept = -0.001815284) +
  #ylim(-25, 20) +
  #theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme(axis.ticks.length=unit(-0.1, "cm")) +
  geom_point(aes(y= mean.norm.work), size = 3, colour = "black")
  # geom_line(aes(y= mean_pct_area), colour = "black")

setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)")
ggsave(file = "depth after pH.pdf",
       units = "cm",
       height = 14,
       width = 18)



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


#install.packages("ggformula")
library(ggformula)

ggplot(larvae.df, aes(y = pct.pH6, x = psi)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_spline(col = "black", df = 4) +
  theme_minimal()


# selecting a linear region of length change
ggplot(data = larvae.df,
       aes(x = depth_m,
           y = pct.pH6,
           # shape = type,
           # linetype = type
       )) +
  #scale_shape_manual(values=c(1, 6)) +
  geom_jitter(size = 3.5, width = 0.01) +
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, df = 3, knots = 60),
              inherit.aes = F,
              aes(x = depth_m,
                  y = pct.pH6,
                  #formula = y ~ (x + I(x^2)),
                  color = '#555555')) +
  #geom_line(aes(x = pH, y = fit), show.legend = FALSE) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, show.legend = FALSE) +
  # annotate("rect",
  #          xmin = sac$pH - sac$pH.sd,
  #          xmax = sac$pH + sac$pH.sd,
  #          ymin = -30, ymax = -25, fill = "black") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 52.0624) +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))

## psi <= 120 (linear region), showing pct.pH6
ggplot(data = filter(larvae.df, psi <= 120), 
       aes(x = depth_m,
           y = pct.pH6)) +
  geom_jitter(size = 3.5, width = 0.01) +
  geom_smooth(method = "lm", # formula = y ~ (x + I(x^2)), # (linear is better here)
              #formula = y ~ splines::bs(x, df = 3, knots = 60),
              #inherit.aes = F,
              aes(x = depth_m,
                  y = pct.pH6,
                  color = '#555555')) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4.991397) +
  geom_vline(xintercept = 52.0624) +
  geom_vline(xintercept = 35.30079) +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))

# looks good, so make a proper lm
ed.lin.model <- lm(depth_m ~ pct.pH6, filter(larvae.df, psi <= 120))
summary(ed.lin.model)
anova(ed.lin.model)

ed.lin.mixmodel.1 <- lmer(depth_m ~ pct.pH6 + (1|larva), filter(larvae.df, psi <= 120))
summary(ed.lin.mixmodel.1)

ed.lin.mixmodel.2 <- lmer(depth_m ~ pct.pH6 + (pct.pH6|larva), filter(larvae.df, psi <= 120))
summary(ed.lin.mixmodel.2)

AIC(ed.lin.mixmodel.1, ed.lin.mixmodel.2) # 2 is better

# find depth value where pct.pH6 = 4.991397, the value that represents an
# equivalent pH (during the pH phase of the edulis experiment) to the final 
# pH reached in the electrochemical experiment

# the work done between this depth and 52.1 meters represents an equivalent pH 
# scale to the one used in the electrochemical experiment

depth.pH_stat <- -3.3581*(4.991397) + 52.0624
depth.pH_stat

# + 4.991397 % of the pH 6 value is reached by compression at a simulated 
# depth of 35.30079 meters. 

# Plotting a spline of work (J) in the linearly SIZE CHANGING region (work is non-linear though, even is size change is linear)
ggplot(data = filter(larvae.df, psi <= 120),
       aes(x = depth_m,
           y = abs(norm.work),
           # shape = type,
           # linetype = type
           )) +
  #scale_shape_manual(values=c(1, 6)) +
  geom_jitter(size = 3.5, width = 0.01) +
  geom_smooth(method = "lm", formula = y ~ (x + I(x^2)),
              #formula = y ~ splines::bs(x, df = 3, knots = 60),
              #inherit.aes = F,
              aes(x = depth_m,
                  y = abs(norm.work),
              color = '#555555')) +
  geom_hline(yintercept = 0.001439484) +
  geom_vline(xintercept = 52.0624) +
  geom_vline(xintercept = 35.30079) +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))

# quadratic model for work specifically
ed.quadmod.work <- lmer(norm.work ~ 
                          (depth_m + I(depth_m^2)) + 
                          ((1|larva)), 
                        filter(larvae.df, psi <= 120))

summary(ed.quadmod.work)
anova(ed.quadmod.work)
plot(ed.quadmod.work)

plot(residuals(ed.quadmod.work), main = "Residuals vs Fitted")
qqnorm(residuals(ed.quadmod.work))
qqline(residuals(ed.quadmod.work))

##  This is now using the mean sac widths

# Here is the work value gotten from the depth at which edulis airsacs have been
# compressed (at pH 8) to a modeled equivalent pH value (using the pH relation
# in the first half of the experiment) of 6.45. 
# this was done by using an equivalent sac size % change from pH 6 size = 4.991397
work.pH_stat.end <- abs((35.30079*(-2.635*(10^-6)) + (35.30079^2)*(-4.974*(10^-7)) + 4.59*(10^-5)))
work.pH_stat.end

# Here is the work value gotten from the depth at which edulis airsacs have been
# compressed (at pH 8) to a size equivalent to that at the unpressurized pH value 
# of 6.00
work.edulis.pH6 <- abs((52.0624*(-2.635*(10^-6)) + (52.0624^2)*(-4.974*(10^-7)) + 4.59*(10^-5)))
work.edulis.pH6

# Work in the edulis experiment done across an equivalent pH scale to that of the 
# electrochemical experiment is given by 
# total work done on edulis sacs down to the pH 6.00 equivalent size (work.edulis.pH6) - 
# the work done on them up to the point equaling pH 6.45 equivalent size (work.pH_stat.end)
work.edulis.pH6 - work.pH_stat.end




###   when the model equation was first figured out... 
###   still normalizing pH by whole experiment

# lm for length change predicts 52.1 meters of water to bring back to pH 6 size
# use this value for depth in the above quadratic lm describing work against depth: 
# all divided by 2 so it's closer to being per 1 pH unit, not two because pH8 - 7 - 6
(52.0624*(-2.635*(10^-6)) + (52.0624^2)*(-4.974*(10^-7)) + 4.59*(10^-5)) /2
# -0.0007197419 J, or 0.7197 mJ/(m x pH)

# lower SE bound
((52.0624-2.4208)*(-2.635*(10^-6)) + ((52.0624-2.4208)^2)*(-4.974*(10^-7)) + 5.5*(10^-5)) /2
# -0.0006507713 J, or 0.650 mJ/(m x pH)

# upper SE bound
((52.0624+2.4208)*(-2.635*(10^-6)) + ((52.0624+2.4208)^2)*(-4.974*(10^-7)) + 5.5*(10^-5)) /2
# -0.0007825274 J, or 0.783 mJ/(m x pH)


# look for actual depth and kPa where pct.6 = zero
larva_size_return <- larvae.df %>%
  mutate(abs.pct.pH6 = abs(pct.pH6)) %>%
  group_by(larva) %>%
  filter(abs.pct.pH6 == min(abs.pct.pH6)) %>%
  select(larva, Area, type, psi, depth_m, kPa, pct.pH6) %>%
  ungroup() %>%
  mutate(mean_depth_pH6.return = mean(depth_m),
         sd_depth_pH6.return = sd(depth_m)) %>%
  print()









#  for Dan
dan_info <- larvae.df %>%
  ungroup() %>%
  select(larva, sac, type, pH6_width) %>%
  unique() %>%
  write_csv("~/student_documents/UBC/Research/Malawi/data/Dan_info.csv")
  
  
  
