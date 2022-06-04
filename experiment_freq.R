library(ggplot2)
library(ggpubr)

draw_freq_voice = function(dataname){
  F1_F2_freq = ggplot(data = dataname, 
                      aes(x = dataname$start_time,
                          y = dataname$F2frequency_sma3nz_amean, 
                          color = 'red'))+
    geom_point()+
    geom_point(aes(y = dataname$F1frequency_sma3nz_amean, color = 'blue'))+
    scale_color_manual(labels = c("F1", 
                                  "F2"), 
                       values = c("blue", "red"))+
    labs(title = deparse(substitute(dataname)), 
         x = 'start time', 
         y = 'Levels')+
    theme_bw()
  
  volume = ggplot(data = dataname,
                  aes(x = dataname$start_time,
                      y = dataname$loudnessPeaksPerSec))+
    geom_line()
  
  print(ggarrange(F1_F2_freq, volume,
                  ncol = 2, nrow = 1))
}

filepath = "D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio/controls"
setwd(filepath)


Freq_sd = function(dataname, feature, feature_name){
  plot = ggplot(data = dataname, 
                aes(x = dataname$start_time,
                    y = feature))+
    geom_point()+
    geom_hline(yintercept = mean(feature), color = 'red')+
    geom_hline(yintercept = mean(feature) + sd(feature), 
               linetype="dashed",
               color = 'red')+
    geom_hline(yintercept = mean(feature) - sd(feature), 
               linetype="dashed",
               color = 'red')+
    labs(title = feature_name,
           x = 'start time', 
          y = 'Levels')+
    theme_bw()
  
  return(plot)
}

draw_4_freq = function(df, features){
  plot_1 = Freq_sd(df, df[[features[1]]], features[1])
  plot_2 = Freq_sd(df, df[[features[2]]], features[2])
  plot_3 = Freq_sd(df, df[[features[3]]], features[3])
  plot_4 = Freq_sd(df, df[[features[4]]], features[4])
  
  print(ggarrange(plot_1, plot_2, plot_3, plot_4,
                  ncol = 2, nrow = 2))
}
feature_set = c("start_time", 
                "F0semitoneFrom27.5Hz_sma3nz_amean",
                "F1frequency_sma3nz_amean",
                "F2frequency_sma3nz_amean",
                "F3frequency_sma3nz_amean")


dataname = read.csv("S007_sliding_window_functionals.csv")
df = dataname[feature_set]

draw_4_freq(df, feature_set[2:5])



get_mean_freq = function(filepath){
  setwd(filepath)  
  files = list.files(filepath)
  grep("sliding_window_functionals", files)
  files_sw = files[grep("sliding_window_functionals", files)]
  
  mean_f0 = c()
  mean_f1 = c()
  mean_f2 = c()
  mean_f3 = c()
  
  for (i in 1:length(files_sw)) {
    df = read.csv(files_sw[i])
    df = df[feature_set]
    
    mean_f0 = c(mean_f0, mean(df[[2]]))
    mean_f1 = c(mean_f1, mean(df[[3]]))
    mean_f2 = c(mean_f2, mean(df[[4]]))
    mean_f3 = c(mean_f3, mean(df[[5]]))
  }
  
  
  df = data.frame(
    mean_f0,
    mean_f1,
    mean_f2,
    mean_f3
  )
  #df = na.omit(df)
  #df = t(df)
  
  return(df)
}

filepath_controls = "D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio/controls"
filepath_dementia = "D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data/train/extracted_features/audio/dementia"

df_train_controls = get_mean_freq(filepath_controls)
df_train_dementia = get_mean_freq(filepath_dementia)


View(df_train_controls)
View(df_train_dementia)
colnames(df_train_controls) = c("mean_f0_c",
                                "mean_f1_c",
                                "mean_f2_c",
                                "mean_f3_c")
colnames(df_train_dementia) = c("mean_f0_d",
                                "mean_f1_d",
                                "mean_f2_d",
                                "mean_f3_d")

df = cbind(df_train_controls,
           df_train_dementia)

View(df)


summary(df)

