### Final Project Code


# Set Working Directory ---------------------------------------------------
setwd("C:/Users/saman/Desktop/PhD/Coursework/BIOL695_DataScience/DS4B695_FinalProject/")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)


# Load Data ---------------------------------------------------------------
dat <- read_csv("./finalProjectData.csv")
dat$Status <- factor(dat$Status, labels = c("Pre", "Post"))
dat$Sex <- factor(dat$Sex)
dat$Group <- factor(dat$Group)
dat$Subject <- factor(dat$Subject)


# ABR ---------------------------------------------------------------------

abr <- dat[1:9] %>% pivot_longer(cols = 5:9, names_to = "freq", values_to = "threshold" ) 
# Rename values in 'category' column
abr <- abr %>%
  mutate(freq = recode(freq,"ABR_thresh_500" = .5,
                       "ABR_thresh_1000" = 1,
                       "ABR_thresh_2000" = 2,
                       "ABR_thresh_4000" = 4,
                       "ABR_thresh_8000" = 8))
abr_sum <- abr %>% group_by(Group, Status, freq) %>% 
  summarise(avg = mean(threshold, na.rm=T), 
            std = sd(threshold, na.rm=T))
abr_fig <- 
  ggplot(
  data = abr) +
  geom_line(size = .8, alpha = .2, aes(x = freq, y=threshold, color=Status, group = interaction(Subject, Status), shape = Sex) ) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=threshold, color=Status, group = interaction(Subject, Status), shape = Sex) ) + 
  xlab("Frequency (kHz)") + 
  ylab("Threshold (dB SPL)") +
  geom_line(size = 1, data = abr_sum, aes(x = freq, y = avg, color = Status, group = Status)) +
  geom_errorbar(data = abr_sum, aes(x = freq, ymax=avg+std, ymin=avg-std, color = Status), width = .05, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) +  
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) +  # Set manual colors
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16)) 
  

# DPOAE -------------------------------------------------------------------

dpoae <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("DP") ) %>% pivot_longer(cols = 5:13, names_to = "freq", values_to = "amplitude" ) 
# Rename values in 'category' column
dpoae <- dpoae %>%
  mutate(freq = recode(freq,"DP_amplitude_707" = .7,
                       "DP_amplitude_1000" = 1,
                       "DP_amplitude_1414" = 1.4,
                       "DP_amplitude_2000" = 2,
                       "DP_amplitude_2828" = 2.8,
                       "DP_amplitude_4000" = 4,
                       "DP_amplitude_5656" = 5.6,
                       "DP_amplitude_8000" = 8, 
                       "DP_amplitude_11313" = 11.3))
dpoae_sum <- dpoae %>% group_by(Group, Status, freq) %>% 
  summarise(avg = mean(amplitude, na.rm=T), 
            std = sd(amplitude, na.rm=T))
dpoae_fig <- 
  ggplot(data = dpoae) +
  geom_line(size = .8, alpha = .2, aes(x = freq, y=amplitude, color=Status, group = interaction(Subject, Status), shape = Sex) ) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=amplitude, color=Status, group = interaction(Subject, Status), shape = Sex) ) + 
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = 1, data = dpoae_sum, aes(x = freq, y = avg, color = Status, group = Status)) +
  geom_errorbar(data = dpoae_sum, aes(x = freq, ymax=avg+std, ymin=avg-std, color = Status), width = .05, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     limits = c(.5, 16),
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) +  # Set manual colors
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16)) 
dpoae_fig

# RAM -------------------------------------------------------------------

ram <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("RAM_amplitude") ) 
# Rename values in 'category' column

ram_sum <- ram %>% group_by(Group, Status) %>% 
  summarise(avg = mean(RAM_amplitude_sum, na.rm=T), 
            std = sd(RAM_amplitude_sum, na.rm=T))
ram_fig <- 
  ggplot(data = ram) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Status, y=RAM_amplitude_sum, color=Status, group = Status)) +
  geom_point(size = 2, alpha = .2, aes(x = Status, y=RAM_amplitude_sum, color=Status, group = interaction(Subject, Status), shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("EFR Amplitude (uV)") +
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) +  # Set manual colors
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16)) 
ram_fig
  

