setwd("D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio")
data = read.csv('required_df.csv')
data$diagnosis = as.factor(data$diagnosis)

library(ggplot2)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(emmeans)

data%>%sample_n_by(gender, diagnosis, size = 1)

#F0semitoneFrom27.5Hz_sma3nz_amean
bxp_gender <- ggboxplot(
  data, x = "gender", y = "F0semitoneFrom27.5Hz_sma3nz_amean",
  color = "diagnosis", palette = "jco"
)
bxp_gender

bxp_diagnosis <- ggboxplot(
  data, x = "diagnosis", y = "F0semitoneFrom27.5Hz_sma3nz_amean",
  color = "gender", palette = "jco"
)
bxp_diagnosis


  
data%>%
  group_by(gender, diagnosis)%>%
  get_summary_stats(F0semitoneFrom27.5Hz_sma3nz_amean, type = "mean_sd")

outlier = data%>%
  group_by(gender, diagnosis)%>%
  identify_outliers(F0semitoneFrom27.5Hz_sma3nz_amean)
view(outlier)

  # Homogneity of variance assumption
data%>%levene_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ gender*diagnosis)
    #The Levene's test is not significant (p > 0.05). 
    #Therefore, we can assume the homogeneity of variances in 
    #the different groups.

res.aov = data%>%anova_test(F0semitoneFrom27.5Hz_sma3nz_amean 
                            ~ gender*diagnosis)
res.aov
    #no significant interaction between gender & diagnosis 

  #pairwise comparisons
pwc <- data %>% 
  group_by(gender) %>%
  emmeans_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ diagnosis, 
               p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "gender")
bxp_gender +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

pwc <- data %>% 
  group_by(diagnosis) %>%
  emmeans_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ gender, 
               p.adjust.method = "bonferroni") 
pwc
   

pwc <- pwc %>% add_xy_position(x = "diagnosis")
bxp_diagnosis +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#------------------------------------------------------------
#F1frequency_sma3nz_amean
bxp_gender <- ggboxplot(
  data, x = "gender", y = "F1frequency_sma3nz_amean",
  color = "diagnosis", palette = "jco"
)
bxp_gender

bxp_diagnosis <- ggboxplot(
  data, x = "diagnosis", y = "F1frequency_sma3nz_amean",
  color = "gender", palette = "jco"
)
bxp_diagnosis



data%>%
  group_by(gender, diagnosis)%>%
  get_summary_stats(F1frequency_sma3nz_amean, type = "mean_sd")

outlier = data%>%
  group_by(gender, diagnosis)%>%
  identify_outliers(F1frequency_sma3nz_amean)
view(outlier)

# Homogneity of variance assumption
data%>%levene_test(F1frequency_sma3nz_amean ~ gender*diagnosis)
#The Levene's test is not significant (p > 0.05). 
#Therefore, we can assume the homogeneity of variances in 
#the different groups.

res.aov = data%>%anova_test(F1frequency_sma3nz_amean 
                            ~ gender*diagnosis)
res.aov
#no significant interaction between gender & diagnosis 

#pairwise comparisons
pwc <- data %>% 
  group_by(gender) %>%
  emmeans_test(F1frequency_sma3nz_amean ~ diagnosis, 
               p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "gender")
bxp_gender +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

pwc <- data %>% 
  group_by(diagnosis) %>%
  emmeans_test(F1frequency_sma3nz_amean ~ gender, 
               p.adjust.method = "bonferroni") 
pwc


pwc <- pwc %>% add_xy_position(x = "diagnosis")
bxp_diagnosis +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )



#------------------------------------------------------------
#F1amplitudeLogRelF0_sma3nz_amean
bxp_gender <- ggboxplot(
  data, x = "gender", y = "F1amplitudeLogRelF0_sma3nz_amean",
  color = "diagnosis", palette = "jco"
)
bxp_gender

bxp_diagnosis <- ggboxplot(
  data, x = "diagnosis", y = "F1amplitudeLogRelF0_sma3nz_amean",
  color = "gender", palette = "jco"
)
bxp_diagnosis



data%>%
  group_by(gender, diagnosis)%>%
  get_summary_stats(F1amplitudeLogRelF0_sma3nz_amean, type = "mean_sd")

outlier = data%>%
  group_by(gender, diagnosis)%>%
  identify_outliers(F1amplitudeLogRelF0_sma3nz_amean)
view(outlier)

# Homogneity of variance assumption
data%>%levene_test(F1amplitudeLogRelF0_sma3nz_amean ~ gender*diagnosis)
#The Levene's test is not significant (p > 0.05). 
#Therefore, we can assume the homogeneity of variances in 
#the different groups.

res.aov = data%>%anova_test(F1amplitudeLogRelF0_sma3nz_amean 
                            ~ gender*diagnosis)
res.aov
#no significant interaction between gender & diagnosis 

#pairwise comparisons
pwc <- data %>% 
  group_by(gender) %>%
  emmeans_test(F1amplitudeLogRelF0_sma3nz_amean ~ diagnosis, 
               p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "gender")
bxp_gender +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

pwc <- data %>% 
  group_by(diagnosis) %>%
  emmeans_test(F1amplitudeLogRelF0_sma3nz_amean ~ gender, 
               p.adjust.method = "bonferroni") 
pwc


pwc <- pwc %>% add_xy_position(x = "diagnosis")
bxp_diagnosis +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#------------------------------------------------------------
#F2frequency_sma3nz_amean
bxp_gender <- ggboxplot(
  data, x = "gender", y = "F2frequency_sma3nz_amean",
  color = "diagnosis", palette = "jco"
)
bxp_gender

bxp_diagnosis <- ggboxplot(
  data, x = "diagnosis", y = "F2frequency_sma3nz_amean",
  color = "gender", palette = "jco"
)
bxp_diagnosis



data%>%
  group_by(gender, diagnosis)%>%
  get_summary_stats(F2frequency_sma3nz_amean, type = "mean_sd")

outlier = data%>%
  group_by(gender, diagnosis)%>%
  identify_outliers(F2frequency_sma3nz_amean)
view(outlier)

# Homogneity of variance assumption
data%>%levene_test(F2frequency_sma3nz_amean ~ gender*diagnosis)
#The Levene's test is not significant (p > 0.05). 
#Therefore, we can assume the homogeneity of variances in 
#the different groups.

res.aov = data%>%anova_test(F2frequency_sma3nz_amean 
                            ~ gender*diagnosis)
res.aov
#no significant interaction between gender & diagnosis 

#pairwise comparisons
pwc <- data %>% 
  group_by(gender) %>%
  emmeans_test(F2frequency_sma3nz_amean ~ diagnosis, 
               p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "gender")
bxp_gender +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

pwc <- data %>% 
  group_by(diagnosis) %>%
  emmeans_test(F2frequency_sma3nz_amean ~ gender, 
               p.adjust.method = "bonferroni") 
pwc


pwc <- pwc %>% add_xy_position(x = "diagnosis")
bxp_diagnosis +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
