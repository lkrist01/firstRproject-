library(tidyverse)
library(Stat2Data)
data("Hawks")

result <- Hawks %>%
                summarise(Wing_mean=mean(Hawks$Wing,na.rm = 1),
                          Wing_t_mean=mean(Hawks$Wing,na.rm = 1,trim=0.1) ,
                          Wing_med= median(Hawks$Wing,na.rm = 1), 
                          Weight_mean=mean(Hawks$Weight,na.rm = 1) , 
                          Weight_t_mean=mean(Hawks$Weight,na.rm = 1,trim=0.1) , 
                          Weight_med=median(Hawks$Weight,na.rm = 1) )
result

#grouped by
result1 <- Hawks %>%
           group_by(Species) %>%
  summarise(Wing_mean=mean(Wing,na.rm = 1),
            Wing_t_mean=mean(Wing,na.rm = 1,trim=0.1) ,
            Wing_med= median(Wing,na.rm = 1), 
            Weight_mean=mean(Weight,na.rm = 1) , 
            Weight_t_mean=mean(Weight,na.rm = 1,trim=0.1) , 
            Weight_med=median(Weight,na.rm = 1) )
result1

hal<-Hawks$Hallux # Extract the vector of hallux lengths
hal<-hal[!is.na(hal)] # Remove any nans

outlier_val<-100
num_outliers<-10
corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))

mean(hal)
mean(corrupted_hal)

#mean
num_outliers_vect<-seq(0,1000)
means_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  means_vect<-c(means_vect,mean(corrupted_hal))
}

#median
num_outliers_vect<-seq(0,1000)
medians_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  medians_vect<-c(medians_vect,median(corrupted_hal))
}

#Trimed mean
t_means_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  t_means_vect<-c(t_means_vect,mean(corrupted_hal,trim = 0.1))
}

df_means_medians<-data.frame(num_outliers=num_outliers_vect,
                             mean=means_vect,t_mean=t_means_vect,
                             median=medians_vect)
df_means_medians

df_means_medians%>%
  pivot_longer(!num_outliers, names_to = "Estimator", values_to = "Value")%>%
  ggplot(aes(x=num_outliers,color=Estimator,
             linetype=Estimator,y=Value))+
  geom_line()+xlab("Number of outliers")  


# Density plot
geo_box <-ggplot(Hawks, aes(x=Species,y=Weight)) +xlab("Species") +
  geom_boxplot() + ylab("Weight")

geo_box


#quantile function for finding the outliers in every group
num_outliers <- function(num){
  count<-0
  q25<-quantile(num,probs = 0.25,na.rm = 1)
  q75<-quantile(num,probs = 0.75,na.rm=1)
  iqr<-q75-q25
  
  for( i in num) {
    if( (i < q25-(1.5*iqr)) | (i > q75+(1.5*iqr)) ){
        count<-count+1
    }
  }
  return(count)
}

res <- Hawks %>%
  group_by(Species) %>%
  summarise(num_outliers_weight=num_outliers(Weight[!is.na(Weight)]))

res





