install.packages("tidyverse")
library(tidyverse)

install.packages("Stat2Data")
library(Stat2Data)
data("Hawks")

hawksSmall<-drop_na(select(Hawks,Age,Day,Month,Year,CaptureTime,Species,Wing,Weight,Tail))

#check how row columns
dim(hawksSmall)

#describe datasets
head(hawksSmall)

# basic histogram
geo_hist <-ggplot(hawksSmall, aes(x=Weight)) +xlab("Weight(gm)") +
                    geom_histogram(binwidth = 10) + ylab("count")

# Density plot
geo_density <-ggplot(hawksSmall, aes(x=Tail)) +xlab("Tail (mm)") +
  geom_density() + ylab("Density")

# Density plot with adjusted range
geo_density1 <-ggplot(hawksSmall, aes(x=Tail)) +xlab("Tail (mm)") +
  geom_density(adjust = 0.5) + ylab("Density")

geo_density2 <-ggplot(hawksSmall, aes(x=Tail)) +xlab("Tail (mm)") +
  geom_density(adjust = 1) + ylab("Density")

#geo_density with aes 2 param color
geo_density_col <-ggplot(data=hawksSmall, aes(x=Tail,color=Species)) +xlab("Tail (mm)") +
  geom_density()+theme_bw() + ylab("Density")

#geo_violin with aes 2 param fill
geo_violin_col <-ggplot(data=hawksSmall, aes(x=Tail,y=Species,fill=Species)) +xlab("Tail (mm)") +
  geom_violin()+theme_bw() + ylab("Species")

#Scatter plot with aes 4 params 
geo_scatter <-ggplot(hawksSmall, aes(x=Tail,y=Weight,color=Species,shape=Species)) +xlab("Tail (mm)") +
  geom_point()+theme_bw() + ylab("Weight (gm)")+ guides(color=guide_legend("Species"))

#Scatter plot with facet and smoth function 
geo_facet_smooth <-ggplot(hawksSmall, aes(x=Tail,y=Weight,color=Species)) +xlab("Tail (mm)")+ 
  geom_point()+facet_wrap(~Species)+ geom_smooth(method = "lm") + theme_bw() + ylab("Weight (gm)")+ guides(color=guide_legend("Species"))

#finding max weight
max(filter(hawksSmall)$Weight,na.rm=TRUE)
geo_scatter_1 <-ggplot(hawksSmall, aes(x=Tail,y=Weight)) +xlab("Tail (mm)") +
  geom_point()+theme_bw() + ylab("Weight (gm)")+ guides(color=guide_legend("Species"))

res <- geo_scatter_1+ geom_curve(x=230,xend=200,y=2030,yend=2030,
       arrow = arrow(length=unit(0.5,"cm")),curvature =0.1)+
       geom_text(x=250,y=1900,label="The heavyist hawak \n weights 2kg")


