library(tidyverse)
library(Stat2Data)
data("Hawks")

#filter and select data
#variables 3
#create a model that will train in RT species and do categorization
#Examples 333
hSF <- Hawks %>%
             filter(Species=="RT" & Weight>1000)%>%
             select(Wing,Weight,Tail)  

#using unique function for distinct            
uniq_wing<- unique(hSF$Wing)
uniq_Weight<- unique(hSF$Weight)
uniq_tail<- unique(hSF$Tail)

#sort with arrange
result <-  hSF %>%
               arrange(Wing)
head(result,5)

#create the names
species_name_full <- c("Red-tailed","Cooper's","Sharp-shinned")
#get the unique species
species_code <- unique(Hawks$Species)
#create a dataframe with species and names
full_names <- data_frame(species_code,species_name_full)

#left join
hawksFullName <- Hawks %>%
                       left_join(full_names,by = c("Species"="species_code")) %>%
                       select(species_name_full,Wing,Weight) %>%
                       rename(Species=species_name_full)
head(hawksFullName)
#No it will not be different cause we have same data in the two tables to join

#mutate function to add a column and arrange for sorting
Bird_BMI <- Hawks %>% 
                  mutate(bird_BMI=(1000*Weight)/(Wing*Wing)) %>%
                  arrange(desc(bird_BMI))%>%
                  select(Species,bird_BMI)
head(Bird_BMI)       

result<-filter(Bird_BMI,bird_BMI<100)
bird_violin <-ggplot(data=result, aes(x=bird_BMI,y=Species,fill=Species))+xlab("Bird BMI") +
  geom_violin()+theme_bw() + ylab("Species")

#Group by with summarize
hawksFullName1 <- Hawks %>%
  left_join(full_names,by = c("Species"="species_code")) %>%
  select(species_name_full,Wing,Weight,Tail) %>%
  rename(Species=species_name_full)

bird_sum <- hawksFullName1 %>% 
              group_by(Species) %>%
              summarize(num_row=n(),min_wing=min(Wing,na.rm = TRUE),
                        avg_wing=mean(Wing,na.rm = TRUE),
                        max_wing=max(Wing,na.rm = TRUE),
                        avg_tail_wing_ratio=mean(Wing/Tail,na.rm = TRUE))
bird_sum            

#group by for missing values in all rows
bird_miss <- Hawks %>%
                   left_join(full_names,by = c("Species"="species_code")) %>%
                   select(species_name_full,Wing,Weight,Culmen,Hallux,Tail,
                          StandardTail,Tarsus,Crop) %>%
                   rename(Species=species_name_full) %>%
                   group_by(Species) %>%
                   summarize(across(everything(),~sum(is.na(.x))))
              
bird_miss
                   
