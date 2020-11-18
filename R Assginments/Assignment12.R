library(palmerpenguins)
library(tidyverse)
library(Stat2Data)

#Exercise 1
#dataset
penguins

bill_adelie <- penguins%>%
  filter(species=="Adelie")%>%
  select(bill_length_mm)%>%
  pull()

bill_adelie

effect_size_one_sample_t_test<-function(x,mu){
  return((mean(x,na.rm = 1)-mu)/sd(x,na.rm = 1))
}

effect_size_one_sample_t_test(bill_adelie,40)

#exercise 2
#install.packages("PairedData") 
library(PairedData)
data("Corn")

stat<-wilcox.test(Corn$Crossed,Corn$Self,paired = TRUE,conf.level = 0.95)
V_stat<-as.double(stat$statistic)
V_stat
n<-length(Corn$Crossed)
T_stat<-2*V_stat-n*(n+1)/2
T_stat
R_stat<-T_stat/(n*sqrt((n+1)*(2*n+1)/6))
R_stat

#Exercise 3
#install.packages("rstatix") 
library(rstatix)
#install.packages('coin')
Corn%>%wilcox_effsize(formula = Crossed ~ Self,paired = TRUE)


#Exercise 4
wilcoxon_stats<-function(data,col1,col2){
  a<-data[[col1]]
  b<-data[[col2]]
  stat<-wilcox.test(a,b,paired = TRUE,conf.level = 0.95)
  V_stat<-as.double(stat$statistic)
  n<-length(data[[col1]])
  T_stat<-2*V_stat-n*(n+1)/2
  R_stat<-T_stat/(n*sqrt((n+1)*(2*n+1)/6))

  result<-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("T_stat", "sample_size", "V_stat","effect_size"))
  result<-result %>% add_row(T_stat = T_stat, 
                     sample_size = n,
                     V_stat=V_stat,
                     effect_size=R_stat)

  return(result)
}

res<-wilcoxon_stats(Corn,"Crossed","Self")
print(res)
