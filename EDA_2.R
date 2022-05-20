setwd('D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio')
library(ggplot2)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(emmeans)
library(cowplot)
data = read.csv('training_functionals.csv')
data$diagnosis = as.factor(data$diagnosis)

View(data)


draw_plots = function(data, x, y, colour)
{
  print(ggplot(data = data, aes_string(x = as.name(x), 
                                 y = as.name(y)))+
    geom_point(aes(colour = as.factor(colour), alpha = 0.3)))
}


draw_plots_2 = function(data, y){
  print(ggplot(data, aes_string(x = data$diagnosis, 
                         y=as.name(y)))+
          geom_violin(aes(fill = factor(gender)), alpha = 0.7)+
          xlab('diagnosis'))
}


for (i in seq(6, 93)) {
  draw_plots(data, 'diagnosis', names(data)[i], data$gender)
}


for (i in seq(6, 93)) {
  draw_plots_2(data, names(data)[i])
  ggsave(filename = paste(i, '.png'), plot = last_plot(), device = "png",
         path = "D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio/images")
}

#x = gender
#"spectralFluxUV_sma3nz_amean" nope
#"F2amplitudeLogRelF0_sma3nz_stddevNorm" nope
#"F1frequency_sma3nz_amean" good 
#"F0semitoneFrom27.5Hz_sma3nz_percentile50.0" nope

#x = diagnosis
#"F3frequency_sma3nz_stddevNorm" nope
#"F2frequency_sma3nz_stddevNorm" nope
#"F2bandwidth_sma3nz_amean" nope
#"HNRdBACF_sma3nz_amean" some results
#"F0semitoneFrom27.5Hz_sma3nz_percentile80.0" some results
#"F0semitoneFrom27.5Hz_sma3nz_percentile50.0" nope 
#"F0semitoneFrom27.5Hz_sma3nz_amean" some results

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


pwc <- data %>% 
  group_by(gender) %>%
  emmeans_test(F0semitoneFrom27.5Hz_sma3nz_amean ~ diagnosis, 
               p.adjust.method = "bonferroni") 

res.aov = data%>%anova_test(F0semitoneFrom27.5Hz_sma3nz_amean 
                            ~ gender*diagnosis)

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

pwc <- pwc %>% add_xy_position(x = "diagnosis")
bxp_diagnosis +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#Looking smth with F1, F2, F3 freq
#No results
draw_diff_frec = function(dataname){
  ggplot(data = dataname, 
         aes(x = dataname$start_time, 
             y = dataname$F2frequency_sma3nz_amean-
               dataname$F1frequency_sma3nz_amean))+
    geom_line()+
    geom_point(aes(y = dataname$F2frequency_sma3nz_amean, color = 'red'))+
    geom_point(aes(y = dataname$F1frequency_sma3nz_amean, color = 'blue'))+
    scale_color_manual(labels = c("F1frequency_sma3nz_amean", 
                                  "F2frequency_sma3nz_amean"), 
                       values = c("blue", "red"))+
    labs(title = deparse(substitute(dataname)), 
         x = 'start time', 
         y = 'difference')+
    theme_bw()
}

S001_sw = read.csv('controls/S001_sliding_window_functionals.csv')
S002_sw = read.csv('controls/S002_sliding_window_functionals.csv')
S055_sw = read.csv('controls/S055_sliding_window_functionals.csv')
S076_sw = read.csv('controls/S076_sliding_window_functionals.csv')

S081_sw = read.csv('dementia/S081_sliding_window_functionals.csv')
S083_sw = read.csv('dementia/S083_sliding_window_functionals.csv')
S093_sw = read.csv('dementia/S093_sliding_window_functionals.csv')
S127_sw = read.csv('dementia/S127_sliding_window_functionals.csv')
S149_sw = read.csv('dementia/S149_sliding_window_functionals.csv')

draw_diff_frec(S001_sw)
draw_diff_frec(S002_sw)
draw_diff_frec(S055_sw)
draw_diff_frec(S076_sw)

draw_diff_frec(S081_sw)
draw_diff_frec(S083_sw)
draw_diff_frec(S093_sw)
draw_diff_frec(S127_sw)
draw_diff_frec(S149_sw)


draw_frec = function(dataname){
  ggplot(data = dataname, 
         aes(x = dataname$start_time, 
             y = dataname$F1frequency_sma3nz_amean))+
    geom_point(aes(color = 'red'))+
    geom_point(aes(y = dataname$F2frequency_sma3nz_amean, color = 'green'))+
    geom_point(aes(y = dataname$F3frequency_sma3nz_amean, color = 'purple'))+
    scale_color_manual(labels = c("F1frequency_sma3nz_amean", 
                                  "F2frequency_sma3nz_amean",
                                  "F3frequency_sma3nz_amean"),
                       values = c("red", "green", "purple"))+
    labs(title = deparse(substitute(dataname)), 
         x = 'start time', 
         y = 'frequences')+
    theme_bw()
}
summary(S001_sw$F3frequency_sma3nz_amean)

draw_frec(S001_sw)
draw_frec(S002_sw)
draw_frec(S055_sw)
draw_frec(S076_sw)

draw_frec(S081_sw)
draw_frec(S083_sw)
draw_frec(S093_sw)
draw_frec(S127_sw)
draw_frec(S149_sw)



