library("reshape2")
library("ggplot2")
library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
library(reshape2)
library(openair)
library(cowplot)
library(patchwork)
library(dplyr)
library(GGally)
library(ggthemes)
library(ggvis)
library(httr)
library(plotly)
library(rio)
library(rmarkdown)
library(shiny)
library(stringr)
library(tidyr)
library(pracma)


age_0 <- c(33,
         34,
         36,
         36,
         38,
         41,
         41,
         43,
         44,
         45,
         46,
         46,
         46,
         47,
         50,
         51,
         52,
         53,
         53,
         54,
         53,
         55,
         56,
         56,
         57,
         58,
         60,
         61,
         61,
         63,
         65,
         64,
         64,
         65,
         66,
         66,
         66,
         67,
         69,
         70,
         70,
         70,
         73,
         73,
         74,
         75,
         75,
         76,
         77,
         76,
         78,
         79,
         80,
         79,
         80,
         80,
         80,
         81,
         82,
         82,
         83,
         83,
         85,
         87,
         88,
         91,
         93,
         96,
         35,
         43,
         51,
         69)

d_age <- c(0,
           1,
           2,
           3,
           4,
           5,
           6,
           7,
           8,
           9,
           10,
           11,
           12,
           13,
           14,
           15,
           16,
           17,
           18,
           19,
           20,
           21,
           22,
           23,
           24,
           25,
           26,
           27,
           28,
           29,
           30,
           31,
           32,
           33,
           34,
           35,
           36,
           37,
           38,
           39,
           40,
           41,
           42,
           43,
           44,
           45,
           46,
           47,
           48,
           49,
           50,
           51,
           52,
           53,
           54,
           55,
           56,
           57,
           58,
           59,
           60,
           61,
           62,
           63,
           64,
           65,
           66,
           67,
           68,
           69,
           70,
           71,
           72,
           73,
           74,
           75,
           76,
           77,
           78,
           79,
           80,
           81,
           82,
           83,
           84,
           85,
           86,
           87,
           88,
           89,
           90,
           91,
           92,
           93,
           94,
           95,
           96,
           97,
           98,
           99,
           100,
           101,
           102,
           103,
           104,
           105,
           106,
           107,
           108,
           109,
           110,
           111,
           112,
           113,
           114,
           115,
           116,
           117,
           118,
           119)
d_Prob <- c(0.006081,
            0.000425,
            0.000260,
            0.000194,
            0.000154,
            0.000142,
            0.000135,
            0.000127,
            0.000116,
            0.000104,
            0.000097,
            0.000106,
            0.000144,
            0.000220,
            0.000323,
            0.000437,
            0.000552,
            0.000675,
            0.000806,
            0.000939,
            0.001079,
            0.001215,
            0.001327,
            0.001406,
            0.001461,
            0.001507,
            0.001557,
            0.001610,
            0.001668,
            0.001732,
            0.001795,
            0.001858,
            0.001923,
            0.001992,
            0.002064,
            0.002145,
            0.002231,
            0.002316,
            0.002398,
            0.002482,
            0.002580,
            0.002697,
            0.002828,
            0.002976,
            0.003146,
            0.003340,
            0.003567,
            0.003833,
            0.004143,
            0.004499,
            0.004890,
            0.005321,
            0.005810,
            0.006363,
            0.006973,
            0.007629,
            0.008322,
            0.009049,
            0.009806,
            0.010595,
            0.011452,
            0.012358,
            0.013255,
            0.014126,
            0.015006,
            0.016001,
            0.017124,
            0.018298,
            0.019519,
            0.020847,
            0.022381,
            0.024185,
            0.026266,
            0.028660,
            0.031401,
            0.034618,
            0.038263,
            0.042190,
            0.046367,
            0.050948,
            0.056237,
            0.062360,
            0.069226,
            0.076884,
            0.085452,
            0.095062,
            0.105829,
            0.117838,
            0.131138,
            0.145751,
            0.161678,
            0.178905,
            0.197408,
            0.217149,
            0.238080,
            0.258821,
            0.278966,
            0.298092,
            0.315762,
            0.331550,
            0.348128,
            0.365534,
            0.383811,
            0.403001,
            0.423151,
            0.444309,
            0.466524,
            0.489851,
            0.514343,
            0.540060,
            0.567063,
            0.595417,
            0.625187,
            0.656447,
            0.689269,
            0.723732,
            0.759919,
            0.797915,
            0.837811,
            0.879701)


##################. 5 Jahre

Death_Prob <- data_frame(d_age, d_Prob)
res_0 <- hist(age_0, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
Hist_data <- data.frame(t(unlist(res_0$counts)))
names(Hist_data) <- c("U30", "U40", "U50", "U60", "U70", "U80", "UE80")
Hist_data$Year <- 2022
Mean_data <- data.frame(mean(age_0))
Mean_data$Year <- 2022

for (q in 1:5){
  test <- data_frame()
  Mean <- data_frame()
  for (l in 1:100){
    age <- age_0
    for(i in 1:(10*q)){
      age <- age + 1
      
      rn <- as.integer(rnorm(n=1, mean=0.333, sd=0.5)) # seit 2008: mean=0.666666667, sd=0.6172134 ,  seit 2017 mean=0.333, sd=0.5 , Nötig: mean=2, sd=0.6172134
      
      if (rn != 0){
        for (j in seq(rn)) {
          rn_age <- as.integer(rnorm(n=1, mean=32.31666667, sd=4.466557834))  #Seit 2008:  mean=30.02861111, sd=2.718177283 , seit 2007 mean=32.31666667, sd=4.466557834
          age <- c(age, rn_age)
        }
      }
      
      new_age <- c()
      for (k in 1:length(age)){
        proba <- as.numeric(Death_Prob[Death_Prob$d_age == age[k], "d_Prob"]) 
        if (!(as.logical(rbinom(1,size=1,prob=proba)))){
          new_age <- c(new_age, age[k])
        }
      }
      age <- new_age
      
    }
    res <- hist(age, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
    test <- rbind(test, res$counts)
    Mean <- rbind(Mean, mean(age))
  }
  
  column_mean <- colMeans(test)
  column_mean <- c(column_mean, (2022+(10*q)))
  Hist_data <- rbind(Hist_data, column_mean)
  Mean_means <- colMeans(Mean)
  Mean_means <- c(Mean_means,(2022+(10*q)))
  Mean_data <- rbind(Mean_data, Mean_means)
}

# barplot(t(as.matrix(Hist_data)))


data_long <- Hist_data                  # Converting data from wide to long format
data_long$Year <- as.character(data_long$Year)
data_long <- melt(data_long, id.vars = "Year")

A <- ggplot(data_long,                  # Stacked barplot using ggplot2
           aes(x = Year,
               y = value,
               fill = variable)) +
      geom_bar(stat = "identity", orientation = "x")+ 
      labs(x = "Jahr", y = "Schiffer Anzahl",  fill = "Alteres Gruppen")+
      ggtitle("Berechnente Alteresentwichlung \n der Schiffer (Werte: 2017-2022)")
  
ggsave("Schiffer_Demographie_Entwicklung_5 Jahre.png", A, path = "/Users/juanbettinelli/Documents/ASV_Statistik/R_Scripts", width = 10, height = 5)

###############################.  14 Jahre


Death_Prob <- data_frame(d_age, d_Prob)
res_0 <- hist(age_0, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
Hist_data <- data.frame(t(unlist(res_0$counts)))
names(Hist_data) <- c("U30", "U40", "U50", "U60", "U70", "U80", "UE80")
Hist_data$Year <- 2022
Mean_data <- data.frame(mean(age_0))
Mean_data$Year <- 2022

for (q in 1:5){
  test <- data_frame()
  Mean <- data_frame()
  for (l in 1:100){
    age <- age_0
    for(i in 1:(10*q)){
      age <- age + 1
      
      rn <- as.integer(rnorm(n=1, mean=0.666666667, sd=0.6172134)) # seit 2008: mean=0.666666667, sd=0.6172134 ,  seit 2017 mean=0.333, sd=0.5 , Nötig: mean=2, sd=0.6172134
      
      if (rn != 0){
        for (j in seq(rn)) {
          rn_age <- as.integer(rnorm(n=1, mean=30.02861111, sd=2.718177283))  #Seit 2008:  mean=30.02861111, sd=2.718177283 , seit 2007 mean=32.31666667, sd=4.466557834
          age <- c(age, rn_age)
        }
      }
      
      new_age <- c()
      for (k in 1:length(age)){
        proba <- as.numeric(Death_Prob[Death_Prob$d_age == age[k], "d_Prob"]) 
        if (!(as.logical(rbinom(1,size=1,prob=proba)))){
          new_age <- c(new_age, age[k])
        }
      }
      age <- new_age
      
    }
    res <- hist(age, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
    test <- rbind(test, res$counts)
    Mean <- rbind(Mean, mean(age))
  }
  
  column_mean <- colMeans(test)
  column_mean <- c(column_mean, (2022+(10*q)))
  Hist_data <- rbind(Hist_data, column_mean)
  Mean_means <- colMeans(Mean)
  Mean_means <- c(Mean_means,(2022+(10*q)))
  Mean_data <- rbind(Mean_data, Mean_means)
}

# barplot(t(as.matrix(Hist_data)))


data_long <- Hist_data                  # Converting data from wide to long format
data_long$Year <- as.character(data_long$Year)
data_long <- melt(data_long, id.vars = "Year")

B <- ggplot(data_long,                  # Stacked barplot using ggplot2
            aes(x = Year,
                y = value,
                fill = variable)) +
  geom_bar(stat = "identity", orientation = "x")+ 
  labs(x = "Jahr", y = "Schiffer Anzahl",  fill = "Alteres Gruppen")+
  ggtitle("Berechnente Alteresentwichlung \n der Schiffer (Werte: 2008-2022)")

ggsave("Schiffer_Demographie_Entwicklung_14_Jahre.png", B, path = "/Users/juanbettinelli/Documents/ASV_Statistik/R_Scripts", width = 10, height = 5)


################################## Wunschwerte

Death_Prob <- data_frame(d_age, d_Prob)
res_0 <- hist(age_0, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
Hist_data <- data.frame(t(unlist(res_0$counts)))
names(Hist_data) <- c("U30", "U40", "U50", "U60", "U70", "U80", "UE80")
Hist_data$Year <- 2022
Mean_data <- data.frame(mean(age_0))
Mean_data$Year <- 2022

for (q in 1:5){
  test <- data_frame()
  Mean <- data_frame()
  for (l in 1:100){
    age <- age_0
    for(i in 1:(10*q)){
      age <- age + 1
      
      rn <- as.integer(rnorm(n=1, mean=2, sd=0.6172134)) # seit 2008: mean=0.666666667, sd=0.6172134 ,  seit 2017 mean=0.333, sd=0.5 , Nötig: mean=2, sd=0.6172134
      
      if (rn != 0){
        for (j in seq(rn)) {
          rn_age <- as.integer(rnorm(n=1, mean=30.02861111, sd=2.718177283))  #Seit 2008:  mean=30.02861111, sd=2.718177283 , seit 2007 mean=32.31666667, sd=4.466557834
          age <- c(age, rn_age)
        }
      }
      
      new_age <- c()
      for (k in 1:length(age)){
        proba <- as.numeric(Death_Prob[Death_Prob$d_age == age[k], "d_Prob"]) 
        if (!(as.logical(rbinom(1,size=1,prob=proba)))){
          new_age <- c(new_age, age[k])
        }
      }
      age <- new_age
      
    }
    res <- hist(age, breaks =c(20, 30, 40, 50, 60, 70, 80, 150), plot = FALSE)
    test <- rbind(test, res$counts)
    Mean <- rbind(Mean, mean(age))
  }
  
  column_mean <- colMeans(test)
  column_mean <- c(column_mean, (2022+(10*q)))
  Hist_data <- rbind(Hist_data, column_mean)
  Mean_means <- colMeans(Mean)
  Mean_means <- c(Mean_means,(2022+(10*q)))
  Mean_data <- rbind(Mean_data, Mean_means)
}

# barplot(t(as.matrix(Hist_data)))


data_long <- Hist_data                  # Converting data from wide to long format
data_long$Year <- as.character(data_long$Year)
data_long <- melt(data_long, id.vars = "Year")

C <- ggplot(data_long,                  # Stacked barplot using ggplot2
            aes(x = Year,
                y = value,
                fill = variable)) +
  geom_bar(stat = "identity", orientation = "x")+ 
  labs(x = "Jahr", y = "Schiffer Anzahl",  fill = "Alteres Gruppen")+
  ggtitle("Berechnente Alteresentwichlung \n der Schiffer (Wunschwerte: Zwei Neue Schiffer)")

ggsave("Schiffer_Demographie_Entwicklung_Wunschwerte.png", C, path = "/Users/juanbettinelli/Documents/ASV_Statistik/R_Scripts", width = 10, height = 5)


