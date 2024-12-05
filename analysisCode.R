### Final Project Code


# Set Working Directory ---------------------------------------------------
setwd("C:/Users/saman/Desktop/PhD/Coursework/BIOL695_DataScience/DS4B695_FinalProject/")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)

library(khroma)


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
  geom_line(size = .8, alpha = .2, 
            aes(x = freq, y=threshold, 
                color=Status, group = interaction(Subject, Status) )) +
  geom_point(size = 2, alpha = .2, 
             aes(x = freq, y=threshold, 
                 color=Status, group = interaction(Subject, Status), shape = Sex, size = 1) ) + 
  xlab("Frequency (kHz)") + 
  ylab("Threshold (dB SPL)") +
  geom_line(size = 1, data = abr_sum, aes(x = freq, y = avg, color = Status, group = Status)) +
  geom_errorbar(data = abr_sum, aes(x = freq, ymax=avg+std, ymin=avg-std, color = Status), width = .05, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) +  
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom")

abr_fig

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
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) +  
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
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
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
ram_fig
  

# MEMR --------------------------------------------------------------------


memr <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("MEMR") ) 

memr_sum <- memr %>% group_by(Group, Status) %>% 
  summarise(avg_thresh = mean(MEMR_threshold, na.rm=T), 
            std_thresh = sd(MEMR_threshold, na.rm=T), 
            avg_105 = mean(MEMR_deltapow_105, na.rm=T), 
            std_105 = sd(MEMR_deltapow_105, na.rm=T))

memr_fig <- 
  ggplot(data = memr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Status, y=MEMR_threshold, color=Status, group = Status)) +
  geom_point(size = 2, alpha = .2, aes(x = Status, y=MEMR_threshold, color=Status, group = interaction(Subject, Status), shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("MEMR Threshold (dB FPL)") +
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
memr_fig


# Differences  ------------------------------------------------------------

# Calculate differences: 
diff_abr <- abr %>% group_by(Subject, freq, Sex, Group) %>% 
  summarise(diff = threshold[Status == "Pre"] - threshold[Status == "Post"]) 
diff_dpoae <- dpoae %>% group_by(Subject, freq, Sex, Group) %>% 
  summarise(diff = amplitude[Status == "Post"] - amplitude[Status == "Pre"]) 
diff_ram <- ram %>% group_by(Subject,  Sex, Group) %>% 
  summarise(diff = RAM_amplitude_sum[Status == "Post"] - RAM_amplitude_sum[Status == "Pre"]) 
diff_memr <- memr %>% group_by(Subject, Sex, Group) %>% 
  summarise(diff = MEMR_threshold[Status == "Pre"] - MEMR_threshold[Status == "Post"]) 

# MEMR Diff Plot
memr_fig_diff <- 
  ggplot(data = diff_memr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=diff, color=Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=diff, color=Group, shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("Delta MEMR Threshold (dB FPL)") +
  scale_color_bright() +  
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
memr_fig_diff

# ABR Diff Plot
abr_fig_diff <- 
  ggplot(data = diff_abr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=diff, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("Delta ABR Threshold (dB SPL)") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) + 
  scale_color_bright() +  
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
abr_fig_diff

# DPOAE Diff Plot
dpoae_fig_diff <- 
  ggplot(data = diff_dpoae) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=diff, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("Delta DP amp (dB FPL)") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) + 
  theme_minimal() + 
  scale_color_bright() +  
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
dpoae_fig_diff + geom_hline(yintercept = 0, color = "red")

# RAM Diff Plot
ram_fig_diff <- 
  ggplot(data = diff_ram) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=diff, group=Group, color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Status") + 
  ylab("Delta RAM amp (uV)") +
  theme_minimal() + 
  scale_color_bright() +
  theme(text = element_text(size = 16), legend.position = "bottom") 
ram_fig_diff


# Models ------------------------------------------------------------------

mod_ram <- lm(data = ram, RAM_amplitude_sum ~ 0+ Group*Status)
mod_diff_ram <- lm(data = diff_ram, diff ~ 0 + Group)
mod_dpoae <- lmer(data = dpoae, amplitude ~ Group*Status*freq + (1|Subject))
mod_diff_dpoae <- lm(data = diff_dpoae, diff ~ Group*freq)
mod_abr <- lm(data = abr, threshold ~ Group*Status*freq)
mod_diff_abr <- lm(data = diff_abr, diff ~ Group)
mod_memr <- lm(data = memr, MEMR_threshold ~ Group*Status)
mod_diff_memr <- lm(data = diff_memr, diff ~ Group)

summary(mod_ram)
Anova(mod_ram, type = 2)
summary(mod_diff_ram)
Anova(mod_diff_ram, type= 2)
summary(mod_dpoae)
summary(mod_diff_dpoae)
summary(mod_abr)
summary(mod_diff_abr)
summary(mod_memr)
summary(mod_diff_memr)

dat2 <- na.omit(dat)

mod_all <- lm(Group ~ ABR_thresh_4000 + DP_amplitude_4000, data = dat2 )

m1 <- lm(data = abr, threshold ~ freq)
abr <- abr %>% add_residuals(m1)
m2 <- lmer(resid ~ Group*Status + (1|Subject), data = abr)
Anova(m2, test.statistic = "F")

ggplot(aes(x=Group, y=resid, color=Status), data=abr) + geom_boxplot()

