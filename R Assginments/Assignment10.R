library(tidyverse)
library(Stat2Data)

student_t_plot<-data.frame(x=numeric(),Density=numeric(),dof=character())

x=seq(-5,5,0.01)
for(dof in c(1,2,5)){
  student_t_plot<-student_t_plot%>%
    rbind(data.frame(
      x=x,Density=dt(x,df=dof),dof=as.character(dof)))
}


student_t_plot%>%
  ggplot(aes(x=x,y=Density,color=dof))+geom_line()+theme_bw()+
  labs(color="Degrees of\n freedom")

pt(qt(seq(0,1,0.1),df=3),df=3)

#exercise 2
library(palmerpenguins)
penguins

adelie_flippers<-penguins%>%data_frame()%>%
                         filter(species=="Adelie")%>%
                         select(flipper_length_mm) 

adelie_flippers<-adelie_flippers$flipper_length_mm

alpha<-0.05
sample_size<-length(adelie_flippers) 
sample_mean<-mean(adelie_flippers,na.rm = 1)
sample_mean

sample_sd<-sd(adelie_flippers,na.rm = 1)
sample_sd<-10

t<-qt(1-alpha/2,df=sample_size-1)
confidence_interval_l<-sample_mean-t*sample_sd/sqrt(sample_size)
confidence_interval_u<-sample_mean+t*sample_sd/sqrt(sample_size)
confidence_interval<-c(confidence_interval_l,confidence_interval_u)
confidence_interval

data("Hawks")

Rt_weight<-Hawks%>%
  filter(Species=="RT")%>%
  select(Weight) 

Rt_weight<-Rt_weight$Weight

alpha<-0.01
sample_size<-length(Rt_weight) 
sample_mean<-mean(Rt_weight,na.rm = 1)
sample_sd<-sd(Rt_weight,na.rm = 1)
t<-qt(1-alpha/2,df=sample_size-1)
confidence_interval_l<-sample_mean-t*sample_sd/sqrt(sample_size)
confidence_interval_u<-sample_mean+t*sample_sd/sqrt(sample_size)
confidence_interval<-c(confidence_interval_l,confidence_interval_u)
confidence_interval

ggplot(data=filter(Hawks,Species=="RT"),aes(sample=Weight))+theme_bw() +
  stat_qq()+stat_qq_line(color="blue") 

#Exercise 3

library(boot) # load the library
set.seed(123) # set random seed

#first define a function which computes the mean of a column of interest
compute_mean<-function(df,indicies,col_name){
  sub_sample<-df%>%slice(indicies)%>%pull(all_of(col_name)) # extract subsample
  return(mean(sub_sample,na.rm=1))}# return median

# use the boot function to generate the bootstrap statistics
results<-boot(data = penguins,statistic =compute_mean,col_name="body_mass_g",R = 1000)

# compute the 95%-level confidence interval for the mean
boot.ci(boot.out = results, type = "basic",conf=0.95)


#first define a function which computes the median of a column of interest
compute_median<-function(df,indicies,col_name){
  sub_sample<-df%>%slice(indicies)%>%pull(all_of(col_name)) # extract subsample
  return(median(sub_sample,na.rm=1))}# return median

# use the boot function to generate the bootstrap statistics
results<-boot(data = Hawks,statistic =compute_median,col_name="Weight",R = 1000)

# compute the 99%-level confidence interval for the median
boot.ci(boot.out = results, type = "basic",conf=0.99)

#Exercise 4

#install.packages("devtools")
#devtools::install_github("shearer/PropCIs")
library(PropCIs)

driving_test_results<-c(1,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,0,1,0)
alpha<-0.01 # failure probability
num_successes<- sum(driving_test_results) # total passes
sample_size<-length(driving_test_results)
scoreci(x=num_successes, n=sample_size, conf.level=1-alpha) # compute Wilson's confidence intervals

Rt_weight<-Hawks%>%
  filter(Species=="RT")%>%
  select(Weight) 


Rt_weight<-Rt_weight$Weight

bernuli_w<-function(Rt_weight,a){
  for(i in Rt_weight){
    if(i>1000 && !is.na(i)){
      a <- append(a,1)
    }else{
      a <- append(a,0)
    } 
  }
  return(a)
}

b<-bernuli_w(Rt_weight,a)

alpha<-0.05 # failure probability
num_successes<- sum(b) # total passes
sample_size<-length(b)
scoreci(x=num_successes, n=sample_size, conf.level=1-alpha) # compute Wilson's confidence intervals

#Exercise 5

getwd()
setwd("~/Desktop/R Assginments/")
getwd()

library(readxl) # load the readxl library
folder_path<-"CalCOFI_Database_194903-201911_csv_10Jul2020/" # set this to the name of the directory containing "HockeyLeague.xlsx"
file_name<-"194903-201911_Bottle.csv" # set the file name
file_path<-paste(folder_path,file_name,sep="") # create the file_path 

data<-read.csv(file_path,fileEncoding = "UCS-2LE") # read data
head(data)
