setwd("D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio")
data = read.csv('required_df.csv')
data$diagnosis = as.factor(data$diagnosis)

library(ggplot2)
library(ggpubr)
library(rstatix)
library(tidyverse)

#F0semitoneFrom27.5Hz_sma3nz_amean ~ gender
data %>%
  group_by(gender) %>%
  get_summary_stats(F0semitoneFrom27.5Hz_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ gender)

pwc <- data %>% tukey_hsd(F0semitoneFrom27.5Hz_sma3nz_amean ~ gender)
pwc <- pwc %>% add_xy_position(x = "diagnosis")
pwc

ggboxplot(data,x = "diagnosis", y = "F0semitoneFrom27.5Hz_sma3nz_amean", 
          color = "gender")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#F0semitoneFrom27.5Hz_sma3nz_amean ~ diagnosis
data %>%
  group_by(diagnosis) %>%
  get_summary_stats(F0semitoneFrom27.5Hz_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ diagnosis)

pwc <- data %>% tukey_hsd(F0semitoneFrom27.5Hz_sma3nz_amean ~ diagnosis)
pwc <- pwc %>% add_xy_position(x = "gender")
pwc

ggboxplot(data,x = "gender", y = "F0semitoneFrom27.5Hz_sma3nz_amean", 
          color = "diagnosis")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#F1frequency_sma3nz_amean ~ gender
data %>%
  group_by(gender) %>%
  get_summary_stats(F1frequency_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F1frequency_sma3nz_amean ~ gender)

pwc <- data %>% tukey_hsd(F1frequency_sma3nz_amean ~ gender)
pwc <- pwc %>% add_xy_position(x = "diagnosis")
pwc

ggboxplot(data,x = "diagnosis", y = "F1frequency_sma3nz_amean", 
          color = "gender")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#F1frequency_sma3nz_amean ~ diagnosis
data %>%
  group_by(diagnosis) %>%
  get_summary_stats(F1frequency_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F1frequency_sma3nz_amean ~ diagnosis)

pwc <- data %>% tukey_hsd(F1frequency_sma3nz_amean ~ diagnosis)
pwc <- pwc %>% add_xy_position(x = "gender")
pwc

ggboxplot(data,x = "gender", y = "F1frequency_sma3nz_amean", 
          color = "diagnosis")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#F1amplitudeLogRelF0_sma3nz_amean ~ gender
data %>%
  group_by(gender) %>%
  get_summary_stats(F1amplitudeLogRelF0_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F1amplitudeLogRelF0_sma3nz_amean ~ gender)

pwc <- data %>% tukey_hsd(F1amplitudeLogRelF0_sma3nz_amean ~ gender)
pwc <- pwc %>% add_xy_position(x = "diagnosis")
pwc

ggboxplot(data,x = "diagnosis", y = "F1amplitudeLogRelF0_sma3nz_amean", 
          color = "gender")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#F1amplitudeLogRelF0_sma3nz_amean ~ diagnosis
data %>%
  group_by(diagnosis) %>%
  get_summary_stats(F1amplitudeLogRelF0_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F1amplitudeLogRelF0_sma3nz_amean ~ diagnosis)

pwc <- data %>% tukey_hsd(F1amplitudeLogRelF0_sma3nz_amean ~ diagnosis)
pwc <- pwc %>% add_xy_position(x = "gender")
pwc

ggboxplot(data,x = "gender", y = "F1amplitudeLogRelF0_sma3nz_amean", 
          color = "diagnosis")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#F2frequency_sma3nz_amean ~ gender
data %>%
  group_by(gender) %>%
  get_summary_stats(F2frequency_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F2frequency_sma3nz_amean ~ gender)

pwc <- data %>% tukey_hsd(F2frequency_sma3nz_amean ~ gender)
pwc <- pwc %>% add_xy_position(x = "diagnosis")
pwc

ggboxplot(data,x = "diagnosis", y = "F2frequency_sma3nz_amean", 
          color = "gender")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#F2frequency_sma3nz_amean ~ diagnosis
data %>%
  group_by(diagnosis) %>%
  get_summary_stats(F2frequency_sma3nz_amean, type = "mean_sd")

res.aov <- data%>%anova_test(F2frequency_sma3nz_amean ~ diagnosis)

pwc <- data %>% tukey_hsd(F2frequency_sma3nz_amean ~ diagnosis)
pwc <- pwc %>% add_xy_position(x = "gender")
pwc

ggboxplot(data,x = "gender", y = "F2frequency_sma3nz_amean", 
          color = "diagnosis")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

