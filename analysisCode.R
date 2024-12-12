### Final Project Code


# Set Working Directory ---------------------------------------------------
setwd("C:/Users/saman/Desktop/PhD/Coursework/BIOL695_DataScience/DS4B695_FinalProject/")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(khroma)

library(embed)
library(ranger)
library(kernlab)
library(tidymodels)
library(lme4)
library(car)


# Load Full Dataset -------------------------------------------------------
dat <- read_csv("./finalProjectData.csv")
dat$Status <- factor(dat$Status, labels = c("Pre", "Post"))
dat$Sex <- factor(dat$Sex)
dat$Group <- factor(dat$Group)
dat$Subject <- factor(dat$Subject)

# Aim 1 -------------------------------------------------------------------

## ABR 
abr <- dat[1:9] %>% pivot_longer(cols = 5:9, names_to = "freq", values_to = "threshold" ) 
# Rename values in 'category' column
abr <- abr %>%
  mutate(freq = dplyr::recode(freq,"ABR_thresh_500" = .5,
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
  geom_line(size = .8, alpha = .1, 
            aes(x = freq, y=threshold, 
                color=Status, group = interaction(Subject, Status) )) +
  xlab("Frequency (kHz)") + 
  ylab("Threshold (dB SPL)") +
  geom_line(linewidth = 1, data = abr_sum, aes(x = freq, y = avg, color = Status, group = Status)) +
  geom_errorbar(data = abr_sum, aes(x = freq, ymax=avg+std, ymin=avg-std, color = Status), width = .05, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) +  
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_bw() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom")

abr_fig

ggsave("./figs/1_ABR.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

### model
mod_abr <- lmer(threshold ~ Group*Status*freq + (1|Subject), data = abr)
Anova(mod_abr, test.statistic = 'F')

## DPOAE 
dpoae <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("DP") ) %>% 
  pivot_longer(cols = 5:13, names_to = "freq", values_to = "amplitude" ) 

# Rename values in 'category' column
dpoae <- dpoae %>%
  mutate(freq = dplyr::recode(freq,"DP_amplitude_707" = .7,
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
  geom_line(size = .8, alpha = .1, aes(x = freq, y=amplitude, color=Status, group = interaction(Subject, Status), shape = Sex) ) +
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
  theme_bw() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
dpoae_fig

ggsave("./figs/1_DPOAE.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

### model
mod_dpoae <- lmer(data = dpoae, amplitude ~ Group*Status*freq + (1|Subject))
Anova(mod_dpoae, test.statistic = 'F')

## RAM 
ram <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("RAM_amplitude") ) 

ram_sum <- ram %>% group_by(Group, Status) %>% 
  summarise(avg = mean(RAM_amplitude_sum, na.rm=T), 
            std = sd(RAM_amplitude_sum, na.rm=T))
ram_fig <- 
  ggplot(data = ram) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Status, y=RAM_amplitude_sum, color=Status, group = Status)) +
  geom_point(size = 2, alpha = .1, aes(x = Status, y=RAM_amplitude_sum, color=Status, group = interaction(Subject, Status), shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("EFR Magnitude (uV)") +
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_bw() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
ram_fig
  
ggsave("./figs/1_RAM.png",
         plot = last_plot(),
         width = 5,
         height = 3.5,
         units = "in",
         dpi = 600)

### model
mod_ram <- lmer(data = ram, RAM_amplitude_sum ~ Group*Status + (1|Subject))
summary(mod_ram)
Anova(mod_ram, test.statistic = "F")

## MEMR 

# gather only MEMR variables
memr <- dat %>% select(c("Subject", "Group", "Status", "Sex") | starts_with("MEMR") ) 

# Look at average statistics
memr_sum <- memr %>% group_by(Group, Status) %>% 
  summarise(avg_thresh = mean(MEMR_threshold, na.rm=T), 
            std_thresh = sd(MEMR_threshold, na.rm=T), 
            avg_105 = mean(MEMR_deltapow_105, na.rm=T), 
            std_105 = sd(MEMR_deltapow_105, na.rm=T))

# plot figure
memr_fig <- 
  ggplot(data = memr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Status, y=MEMR_threshold, color=Status, group = Status)) +
  geom_point(size = 2, alpha = .1, aes(x = Status, y=MEMR_threshold, color=Status, group = interaction(Subject, Status), shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("MEMR Threshold (dB FPL)") +
  scale_color_manual(values = c("Pre" = "black", "Post" = "red")) + 
  facet_grid(~Group) + 
  theme_bw() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
memr_fig

ggsave("./figs/1_memr.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

### models
mod_memr <- lmer(data = memr, MEMR_threshold ~ Group*Status + (1|Subject))
Anova(mod_memr, test.statistic = 'F')


# Aim 2: Post Only  -------------------------------------------------------
# Filter for post only: 
post_abr <- abr %>% filter(Status == "Post")
post_dpoae <- dpoae %>% filter(Status == "Post")
post_ram <- ram %>% filter(Status == "Post")
post_memr <- memr %>% filter(Status == "Post")

# MEMR post Plot
memr_fig_post <- 
  ggplot(data = post_memr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=MEMR_threshold, color=Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=MEMR_threshold, color=Group, shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Group") + 
  ylab("MEMR Threshold (dB FPL)") +
  scale_color_bright() +  
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
memr_fig_post

ggsave("./figs/2_memr.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

# ABR post Plot
abr_fig_post <- 
  ggplot(data = post_abr) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=threshold, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=threshold, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("ABR Threshold (dB SPL)") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) + 
  scale_color_bright() +  
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
abr_fig_post

ggsave("./figs/2_abr.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

# DPOAE post Plot
dpoae_fig_post <- 
  ggplot(data = post_dpoae) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=amplitude, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=amplitude, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("Amplitude (dB FPL)") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) + 
  theme_minimal() + 
  scale_color_bright() +  
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
dpoae_fig_post 

ggsave("./figs/2_dpoae.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

# RAM post Plot
ram_fig_post <- 
  ggplot(data = post_ram) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=RAM_amplitude_sum, group=Group, color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=RAM_amplitude_sum, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Group") + 
  ylab("EFR Magnitude (uV)") +
  theme_minimal() + 
  scale_color_bright() +
  theme(text = element_text(size = 16), legend.position = "bottom") 
ram_fig_post

ggsave("./figs/2_ram.png",
       plot = last_plot(),
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 600)

# models for Aim 2
mod_post_abr <- lm(data = post_abr, threshold ~ Group*freq)
Anova(mod_post_abr, test.statistic = "F")

mod_post_dpoae <- lm(data = post_dpoae, amplitude ~ Group*freq)
Anova(mod_post_dpoae, test.statistic = "F")

mod_post_memr <- lm(data=post_memr, MEMR_threshold ~ Group)
Anova(mod_post_memr, test.statistic = "F")

mod_post_ram <- lm(data = post_ram, RAM_amplitude_sum ~ Group)
Anova(mod_post_ram, test.statistic = "F")


# Aim 3: Profiling --------------------------------------------------------

dat2 <- na.omit(dat)
dat2 <- dat2 %>% filter(Status == "Post")

set.seed(124)

# Classification 1
my_split <- initial_split(dat2, prop=0.75, strata = Group)
my_train <- training(my_split)
my_test <- testing(my_split)

my_cv <- vfold_cv(my_train, v=10, strata = Group)

# Model 1, decision tree
mod1_tree <- decision_tree(cost_complexity = 0.002) %>% 
  set_mode("classification")

mod1_wflow <- workflow(Group ~ Sex + ABR_thresh_500	+ ABR_thresh_1000 +	
                         ABR_thresh_2000 +	ABR_thresh_4000	+ ABR_thresh_8000	+
                         DP_amplitude_707	+ DP_amplitude_1000 +	DP_amplitude_1414	+
                         DP_amplitude_2000	+DP_amplitude_2828+	DP_amplitude_4000	+
                         DP_amplitude_5656	+DP_amplitude_8000+	DP_amplitude_11313 +
                         MEMR_threshold	+	RAM_amplitude_sum, mod1_tree)

mod1_res_fit <- fit_resamples(mod1_wflow, my_cv, 
                              metrics=metric_set(accuracy, roc_auc, brier_class))

mod1_res_fit %>% collect_metrics()

# Model 2, random forest
mod2_randForest <- rand_forest(trees=1000) %>% 
  set_mode("classification")
mod2_wflow <- workflow(Group ~ 	ABR_thresh_2000+	ABR_thresh_4000	+ABR_thresh_8000 +
                         DP_amplitude_2000	+DP_amplitude_2828+	DP_amplitude_4000	+
                         MEMR_threshold	+	RAM_amplitude_sum, mod2_randForest)

mod2_res_fit <- fit_resamples(mod2_wflow, my_cv, 
                              metrics=metric_set(accuracy, roc_auc, brier_class))

mod2_res_fit %>% collect_metrics()

# Model 3, random forest with tuning
mod3_tuneRandForest <- rand_forest(trees=1000,min_n=tune())%>%set_mode("classification")

mod3_wflow <- workflow(Group ~  ABR_thresh_2000+	ABR_thresh_4000	+ABR_thresh_8000	+DP_amplitude_707	+
                         DP_amplitude_2000	+DP_amplitude_2828+	
                         MEMR_threshold	+	RAM_amplitude_sum, mod3_tuneRandForest)

tune_minn_res <- tune_grid(
  mod3_wflow,
  my_cv,
  grid = 5
)

best_minn <- select_best(tune_minn_res)
mod4_wflow <- finalize_workflow(mod3_wflow, best_minn)
final_fit <- last_fit(mod4_wflow, my_split)
collect_metrics(final_fit)
collect_predictions(final_fit)

# Model 5 with importance function
mod5 <-
  rand_forest(trees=1000,min_n=15
  )%>%
  set_engine("ranger",importance="impurity")%>%
  set_mode("classification")
final_fit <- last_fit(mod4_wflow, my_split)
final_fit %>%extract_fit_engine()%>%
  importance()

# Models with limited subset of data
set.seed(145)
dat3 <- dat2 %>% filter(Status == "Post") 
#norms <- dat %>% filter(Status == "Pre")
#norms$Group <- "NH"
#dat3 <- rbind(dat3, norms) 
dat3 <- dat3[,c(2, 7,8,9,13, 14,19, 21)]
#dat3 <- dat3 %>% filter(Group !="TTS")

colnames(dat3)[2:8] <- str_c("p_", colnames(dat3)[2:8])
mysplit <- initial_split(dat3, prop = 0.8, strata = Group)
mytrain <- training(mysplit)
mytest <- testing(mysplit) 

# logistic regression
logreg_spec <- 
  linear_reg() %>% 
  set_mode('classification')

logreg_wf <- workflow(Group ~ 
                        p_ABR_thresh_2000 + p_DP_amplitude_2000 + 
                        p_ABR_thresh_4000 + p_ABR_thresh_8000 + 
                        p_DP_amplitude_2828 +
                        p_MEMR_threshold + p_RAM_amplitude_sum, logreg_spec) 
logreg_fit <- logreg_wf %>%
  fit(data = mytrain)

augment(logreg_fit, new_data = mytrain) %>% 
  conf_mat(truth=Group, estimate=.pred_class) %>% 
  autoplot(type = "heatmap")

final_fit <- last_fit(logreg_wf, mysplit)
collect_metrics(final_fit)
collect_predictions(final_fit)


library(rpart.plot)
logreg_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

# model with recipe and other processing
rec <- recipe(Group ~., data = mytrain) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(starts_with("p_"), num_comp = 5)

mod6_wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(svm_linear(engine = "kernlab",mode="classification",
                       cost=0.02,margin=0.18))

mod6_fit <- mod6_wflow %>% fit(data = mytrain)
augment(mod6_fit, new_data = mytrain) %>% 
  conf_mat(truth=Group, estimate=.pred_class) %>% 
  autoplot(type = "heatmap")

tune_svm <- tune_grid(
  mod6_wflow,
  my_cv,
  grid = 5
)

bestval <- select_best(tune_svm)
mod7_wflow <- finalize_workflow(mod6_wflow, bestval)

final_fit <- last_fit(mod7_wflow, mysplit)
collect_metrics(final_fit)
collect_predictions(final_fit)

# model 7 with resampled train/test data
mod7_res_fit <- fit_resamples(mod7_wflow, mytest, 
                              metrics=metric_set(accuracy, roc_auc, brier_class))


augment(mod7_res_fit, new_data = mytrain) %>% 
  roc_curve(truth = Group, estimate=.pred_class) %>%
  autoplot()

# PCA
diff2 <- na.omit(diff)
pcatest <- princomp(diff2[,5:19])
summary(pcatest)
autoplot(pcatest)

## Classify with Differences
diff <- dat %>% group_by(Subject, Sex, Group) %>% 
  summarise(d_ABR_thresh_500 = ABR_thresh_500[Status == "Pre"] - ABR_thresh_500[Status == "Post"],
            d_ABR_thresh_1000 = ABR_thresh_1000[Status == "Pre"] - ABR_thresh_1000[Status == "Post"],
            d_ABR_thresh_2000 = ABR_thresh_2000[Status == "Pre"] - ABR_thresh_2000[Status == "Post"], 
            d_ABR_thresh_4000 = ABR_thresh_4000[Status == "Pre"] - ABR_thresh_4000[Status == "Post"],
            d_ABR_thresh_8000 = ABR_thresh_8000[Status == "Pre"] - ABR_thresh_8000[Status == "Post"], 
            d_DP_amplitude_707 = DP_amplitude_707[Status == "Pre"] - DP_amplitude_707[Status == "Post"], 
            d_DP_amplitude_1000 = DP_amplitude_1000[Status == "Pre"] - DP_amplitude_1000[Status == "Post"],
            d_DP_amplitude_1414 = DP_amplitude_1414[Status == "Pre"] - DP_amplitude_1414[Status == "Post"],
            d_DP_amplitude_2000 = DP_amplitude_2000[Status == "Pre"] - DP_amplitude_2000[Status == "Post"],
            d_DP_amplitude_2828 = DP_amplitude_2828[Status == "Pre"] - DP_amplitude_2828[Status == "Post"], 
            d_DP_amplitude_4000 = DP_amplitude_4000[Status == "Pre"] - DP_amplitude_4000[Status == "Post"],
            d_DP_amplitude_5656 = DP_amplitude_5656[Status == "Pre"] - DP_amplitude_5656[Status == "Post"],
            d_DP_amplitude_8000 = DP_amplitude_8000[Status == "Pre"] - DP_amplitude_8000[Status == "Post"],
            d_DP_amplitude_11313 = DP_amplitude_11313[Status == "Pre"] - DP_amplitude_11313[Status == "Post"],
            d_MEMR_threshold = MEMR_threshold[Status == "Pre"] - MEMR_threshold[Status == "Post"], 
            d_RAM_amplitude_sum = RAM_amplitude_sum[Status == "Pre"] - RAM_amplitude_sum[Status == "Post"])

mysplit <- initial_split(diff, prop = 0.8, strata = Group)
mytrain <- training(mysplit)
mytest <- testing(mysplit) 
my_cv <- vfold_cv(mytrain, v=10, strata = Group)

rec <- recipe(Group ~., data = mytrain) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(starts_with("d_"), num_comp = 5)

mod6_wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(decision_tree(mode="classification"))

tune_svm <- tune_grid(
  mod6_wflow,
  my_cv,
  grid = 5
)

bestval <- select_best(tune_svm)
mod7_wflow <- finalize_workflow(mod6_wflow, bestval)

final_fit <- last_fit(mod6_wflow, mysplit)
collect_metrics(final_fit)
collect_predictions(final_fit)

mod6_fit <- mod6_wflow %>% fit(data = mytrain)
augment(mod6_fit, new_data = mytrain) %>% 
  conf_mat(truth=Group, estimate=.pred_class) %>% 
  autoplot(type = "heatmap")

logreg_wf <- workflow(Group ~ Sex + d_ABR_thresh_500	+ d_ABR_thresh_1000 +	
                        d_ABR_thresh_2000 +	d_ABR_thresh_4000	+ d_ABR_thresh_8000	+
                        d_DP_amplitude_707	+ d_DP_amplitude_1000 +	d_DP_amplitude_1414	+
                        d_DP_amplitude_2000	+d_DP_amplitude_2828 +	d_DP_amplitude_4000	+
                        d_DP_amplitude_5656	+ d_DP_amplitude_8000 +	d_DP_amplitude_11313 +
                        d_MEMR_threshold	+	d_RAM_amplitude_sum, logreg_spec) 
logreg_fit <- logreg_wf %>%
  fit(data = mytrain)

augment(logreg_fit, new_data = mytrain) %>% 
  conf_mat(truth=Group, estimate=.pred_class) %>% 
  autoplot(type = "heatmap")


# extra analyses ----------------------------------------------------------


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
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=diff, color=Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=diff, color=Group, shape = Sex), position = position_jitter(width=.15))  + 
  xlab("Status") + 
  ylab("\U0394 MEMR Threshold (dB FPL)") +
  scale_color_bright() +  
  theme_minimal() + 
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
memr_fig_diff

# ABR Diff Plot
abr_fig_diff <- 
  ggplot(data = diff_abr) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=diff, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("\U0394 ABR Threshold (dB SPL)") +
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
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
  geom_boxplot(size = .8, alpha = .2, aes(x = freq, y=diff, group=interaction(Group, freq), color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = freq, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Frequency") + 
  ylab("\U0394 DP Amp. (dB FPL)") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8),  
                     labels = c(".5", "1", "2", "4", "8")) + 
  theme_minimal() + 
  scale_color_bright() +  
  theme(strip.text = element_text(size = 16), text = element_text(size = 16), legend.position = "bottom") 
dpoae_fig_diff 

# RAM Diff Plot
ram_fig_diff <- 
  ggplot(data = diff_ram) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
  geom_boxplot(size = .8, alpha = .2, aes(x = Group, y=diff, group=Group, color = Group)) +
  geom_point(size = 2, alpha = .2, aes(x = Group, y=diff, color=Group, shape = Sex), position = position_jitter(width=.1))  + 
  xlab("Status") + 
  ylab("\U0394 RAM Amp. (uV)") +
  theme_minimal() + 
  scale_color_bright() +
  theme(text = element_text(size = 16), legend.position = "bottom") 
ram_fig_diff

# Post Only  ------------------------------------------------------------
