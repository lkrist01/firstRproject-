library(tidyverse)
library(Stat2Data)

sample_mean_Bernoulli<-function(q,n){
  
  bernoulli_sample<-rbinom(n,1,q) # generate Bernoulli sample
  
  return(mean(bernoulli_sample)) # return the mean
  
}


q<-0.7
n<-10
num_trials<-1000

sample_mean_Bernoulli_vect<-map_dbl(seq(num_trials),~sample_mean_Bernoulli(n=n,q=q))
sample_mean_bias <- mean(sample_mean_Bernoulli_vect)-q
sample_mean_variance <-var(sample_mean_Bernoulli_vect)
sample_mean_mse <- mean((sample_mean_Bernoulli_vect-q)**2)  

sample_mean_mse # mean squared error of our estimate

sample_mean_variance+sample_mean_bias**2 # variance plus squared bias 

#exercise 1 
sample_std <- sqrt( (1/(n-1))*(sample_mean_Bernoulli_vect-q)**2 )
population_std<-sqrt(q*(1-q))

bias_estimate <- mean(sample_std)-population_std  
variance_estimate <- mean(sample_mean_Bernoulli_vect-mean(sample_std)**2)
mse<-mean(sample_std-population_std)
 
sample_mean_mse # mean squared error of our estimate
variance_estimate+bias_estimate**2 # variance plus squared bias 


#Exercise 4  
library(Stat2Data)
data("Hawks")

RedTailedDf<- data_frame(Hawks)%>%
                  filter(Species=="RT")%>%
                  select(Weight,Tail,Wing)

#mean
mean_weight<-mean(RedTailedDf$Weight,na.rm=1)
mean_tail<-mean(RedTailedDf$Tail,na.rm=1)
mean_wing<-mean(RedTailedDf$Wing,na.rm=1)

#variance
var_weight<-var(RedTailedDf$Weight,na.rm=1)
var_tail<-var(RedTailedDf$Tail,na.rm=1)
var_Wing<-var(RedTailedDf$Wing,na.rm=1)

res<-data_frame(varWe= var(RedTailedDf$Weight,na.rm=1),
                varT=var(RedTailedDf$Tail,na.rm=1),
                varWi=var(RedTailedDf$Wing,na.rm=1))


cov(RedTailedDf)


RedTailedDf1<-data.frame(Weight=rnorm(length(RedTailedDf$Weight), mean=mean_weight, sd=sqrt(var_weight)), 
                        Tail= rnorm(length(RedTailedDf$Tail), mean=mean_tail, sd=sqrt(var_tail)) ,
                        Wing= rnorm(length(RedTailedDf$Wing), mean=mean_wing, sd=sqrt(var_Wing)) 
                        )

res1<-data_frame(varWe= var(RedTailedDf1$Weight,na.rm=1),
              varT=var(RedTailedDf1$Tail,na.rm=1),
              varWi=var(RedTailedDf1$Wing,na.rm=1))
res
res1

cov(RedTailedDf)
cov(RedTailedDf1)

