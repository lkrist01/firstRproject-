library(palmerpenguins)
library(tidyverse)
library(Stat2Data)

#Exercise 2
#dataset
penguins

bill_adelie <- penguins%>%
                        filter(species=="Adelie")%>%
                        select(bill_length_mm)%>%
                        pull()

bill_adelie
#test for null hypothesis 40mm length
t.test(bill_adelie,mu=40,conf.level = 0.99) 

#Exercise 3
library(Stat2Data)
data("Airlines")
Airlines
data<-Airlines%>%
              filter(airline=="Delta")%>%
              select(IndOHare)%>%
              pull()
data
binom.test(sum(data),n=length(data),p = 0.875)

#Exercise 4

two_sided_test<-function(x,m){
  sample_size<-length(x)
  a<-0.05
  sample_mean<-mean(x,na.rm = 1)
  sample_sd<-sd(x,na.rm=1)
  test_statistics<-(sample_mean-m)/(sample_sd/sqrt(sample_size))
  test_statistics
  p_value<-2*(1-pt(abs(test_statistics),df=sample_size-1))
  
  if(p_value<a){
    return("Reject H0, Accept H1")
  }else{
    return("Accept H0")
  }  
}

two_sided_test(bill_adelie,40)
t.test(x, mu = 40)


  