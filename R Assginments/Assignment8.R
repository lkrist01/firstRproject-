
gauss <- function(x,n,s){
  result<-(1/s*sqrt(2*pi)) * exp(-1/2 * ( ((x-n)/s)*((x-n)/s) ))
  return(result)
} 

s1<-gauss(x,1,1)
s2<-gauss(x,1,2)
s3<-gauss(x,1,3)

library(tidyverse)
library(Stat2Data)
library(ggplot2)

#probability density function

  num<-seq(-4,6,1)
  s1<-dnorm(num,mean = 1,sd = sqrt(1))
  s2<-dnorm(num,mean = 1,sd = sqrt(2))
  s3<-dnorm(num,mean = 1,sd = sqrt(3))
  
  data <-data.frame(num,s1,s2,s3)
  data <- data%>% pivot_longer(!num,names_to="Variance",values_to="Value")
  data

  # Density plot
  geo <-ggplot(data,aes(num,y = Value, color = Variance)) +xlab("variance") +
    geom_line()+theme_bw() + ylab("Density") 
  
  geo
  
  plot(num, s1, type="o", col="blue", pch="o", lty=1, ylim=c(0,1) )
  lines(num, s2, col="red",)
  lines(num, s3, col="dark red")
  

  
  #Comulative distribuiton
  s1<-pnorm(num,mean = 1,sd = sqrt(1))
  s2<-pnorm(num,mean = 1,sd = sqrt(2))
  s3<-pnorm(num,mean = 1,sd = sqrt(3))
  
  data <-data.frame(num,s1,s2,s3)
  data <- data%>% pivot_longer(!num,names_to="Variance",values_to="Value")
  data
  
  
  # plot the first curve by calling plot() function
  # First curve is plotted
  plot(num, s1, type="o", col="blue", pch="o", lty=1, ylim=c(0,1) )
  lines(num, s2, col="red")
  lines(num, s3, col="dark red")
  
  # Density plot
  geo1 <-ggplot(data,aes(num,y = Value, color = Variance)) +xlab("variance") +
    geom_smooth()+theme_bw() + ylab("Density") 
  
  geo1
  
 #quantile function
 data<- data %>%
    pivot_wider(names_from = Variance, values_from = Value)

  s1<-qnorm(data$s1,mean = 1,sd = sqrt(1))
  s2<-qnorm(data$s2,mean = 1,sd = sqrt(2))
  s3<-qnorm(data$s3,mean = 1,sd = sqrt(3))
  
  #plot another way
  plot(num, s1, type="o", col="blue", pch="o", lty=1 )
  lines(num, s2, col="red")
  lines(num, s3, col="dark red")
  
  
  data <-data.frame(num,s1,s2,s3)
  data <- data%>% pivot_longer(!num,names_to="Variance",values_to="Value")
  data
  
  # Density plot
  geo2 <-ggplot(data,aes(num,y = Value, color = Variance)) +xlab("variance") +
    geom_smooth()+theme_bw() + ylab("Density") 
  
  geo2
  
  set.seed(1)
  standardGaussianSample <- rnorm(100,0,1)
  standardGaussianSample
  
  mean1Var3GaussianSampleA<-standardGaussianSample*sqrt(3)
  mean1Var3GaussianSampleA
  
  set.seed(1)
  mean1Var3GaussianSampleB <-rnorm(100,1,3)
  mean1Var3GaussianSampleB
  
  s1<-standardGaussianSample[1:11]
  s2<-mean1Var3GaussianSampleA[1:11]
  s3<-mean1Var3GaussianSampleB[1:11]
  
  popullation_density<-dnorm(num,mean = 1,sd = sqrt(3))
  popullation_density
  
  plot(density(mean1Var3GaussianSampleA), col="blue",ylim=c(0,0.25))
  lines(num,popullation_density,col="red")
  abline(v=mean(mean1Var3GaussianSampleA), col="yellow")
  abline(v=mean(popullation_density), col="green")
  
  popullation_density<-dnorm(num,mean = 1,sd = 3)
  popullation_density
  sample_kernel_density<-dnorm(mean1Var3GaussianSampleA[1:11],mean = 1,sd = 3)
  sample_kernel_density
  
  
  data <-data.frame(num,popullation_density,sample_kernel_density)
  data <- data%>% pivot_longer(!num,names_to="Legend",values_to="Value")
  data
  
  # Density plot
  graphic <-ggplot(data,aes(num,y = Value, color = Legend)) +xlab("X") +
    geom_smooth()+theme_bw() + ylab("Density") 
  graphic
  
  
  #exercise 5
  sample = rbinom(n = 1000,size=50 ,prob = 0.7)
  mean = dnorm(sample,mean = 700,sd = 210)

  data <-data.frame(sample,mean)
  data

  hist(x = sample,freq = FALSE,xlim = c(20, 50),ylab ="count",xlab="Z") 
  lines(x = density(x=sample), col = "red",ylim = c(0, 250))
  
  df<-data_frame(sample)
  
  p<-ggplot(df, aes(x=sample)) + 
    geom_histogram( color = "darkred") + 
    geom_density(color="steelblue", linetype="twodash") 
  p
  
  # crease the base plot object
  p <- ggplot(data, aes(x = sample))
  p + geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=0.5) 
  p <- ggplot(data, aes(x = mean))
  p + geom_density(colour="black", adjust=4) 
  
  p<-ggplot(data=data, aes(x=sample, color='red')) +
    geom_histogram(bins = 50000, fill="white", show.legend = FALSE, size=1.1) +
    geom_line(data = data, aes(y=mean), color= 'blue', size=1.1) +
    labs(title= 'My histogram', x = 'N(0,1)', y='Count') 
  p  
    
  
  #Exercise 6
  total_sample_size<-5000 # set the total sample size
  
  num_trials<-8 # set the number of trials
  
  set.seed(123) # set the random seed 
  
  # generate a sample standard deviations as function of the sample size for some randomly generated Gaussian data
  gaussian_sample_sd_by_sample_size<-function(){ 
    
    return(
      data.frame(sample_size=seq(total_sample_size),X=rnorm(total_sample_size))%>% #generate normal data
        mutate(sd=map_dbl(row_number(),~sd(X[1:.x]))) # compute sd of the initial segment
    )
  }
  
  gaussian_sample_sd_by_sample_size_new<-function(){ 
    
    return(
      data.frame(sample_size=seq(total_sample_size),X=rnorm(total_sample_size))%>% #generate normal data
        mutate(sd=map_dbl(row_number(),~mad(X[1:.x],constant = 1))) # compute sd of the initial segment
    )
  }
  
  df<-data.frame(trial=seq(num_trials))%>%
    
    mutate(data=map(trial, ~gaussian_sample_sd_by_sample_size()))%>% # apply simulation for each trial
    unnest(cols=data) # unnest over the different trials
  
  
  
    p<-ggplot(df%>%
           filter(sample_size>25),aes(x=sample_size,y=sd,color=as.character(trial)))+
    geom_line()+theme_bw()+labs(color="Trial",x="Sample size", y="Standard deviation")
  # Plot results
    p
    
    df<-data.frame(trial=seq(num_trials))%>%
      
      mutate(data=map(trial, ~gaussian_sample_sd_by_sample_size_new()))%>% # apply simulation for each trial
      unnest(cols=data) # unnest over the different trials
    
    
    
    p<-ggplot(df%>%
                filter(sample_size>25),aes(x=sample_size,y=sd,color=as.character(trial)))+
      geom_line()+theme_bw()+labs(color="Trial",x="Sample size", y="Standard deviation")
    # Plot results
    
    
  p<-p+geom_segment(aes(x=0,xend=5000,y=0.6744908,yend=0.6744908),colour="black")  
  p  
  
  