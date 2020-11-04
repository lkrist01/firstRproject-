library(tidyverse)

impute_by_mean<-function(x){
  
  mu<-mean(x,na.rm=1) # first compute the mean of x
  
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

impute_by_median<-function(x){
  
  mu<-median(x,na.rm=1) # first compute the mean of x
  
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

v<-c(1,2,NA,4)
impute_by_median(v)

x <- seq(0,10,by = 0.1)
for (i in x) {
  y <-c(y,5*i+1)
}

df_xy<-data_frame(x,y)
df_xy%>%head(5)

#add a column sum of x+y 
df_xy%>%
  mutate(z=map2_dbl(x,y,~.x+.y))%>%
  head(5)


sometimes_missing<- function(index,value){
  if(index %% 5==0){
    return(NA)
  }else
    return(value)
}
sometimes_missing(15,25)

x<-data_frame(x)

#maping take params first and then name of function
df_xy_missing <- x%>% 
     mutate(y=map2_dbl(row_number(y),y,sometimes_missing))

df_xy_missing%>%
  head(10)

df_xy_impute <- x%>% 
  mutate(y=impute_by_median(df_xy_missing$y))

head(df_xy_impute)


df_xy<-df_xy%>%
  mutate(source="original")


df_xy_missing<-df_xy_missing%>%
  mutate(source="corrupted")

df_xy_impute<-df_xy_impute%>%
  mutate(source="imputed")

df_combined<-rbind(df_xy,df_xy_missing,df_xy_impute)

ggplot(df_combined,aes(x=x,y=y,color=source))+geom_point()+
  facet_wrap(~source)+geom_smooth(method="lm")


install.packages("readxl")

library(readxl) # load the readxl library
library(dplyr)
library(tidyr)



folder_path<-"~/Desktop/R Assginments/" # set this to the name of the directory containing "HockeyLeague.xlsx"

file_name<-"HockeyLeague.xlsx" # set the file name

file_path<-paste(folder_path,file_name,sep="") # create the file_path 

wins_data_frame<-read_excel(file_path,sheet="Wins") # read of a sheet from an xl file

wins_data_frame %>%
            select(1:5)%>%
            head(3)

colnames(wins_data_frame)

wins_tidy<- wins_data_frame %>%
                            rename(Team=...1)%>%
                            pivot_longer(cols=!Team,
                            names_to="Years",values_to="Wins")%>%
                            separate(Wins,into=c("Wins","Total"),sep=" of ")
        
                            
  
wins_tidy%>% dim() # check the dimensions
wins_tidy%>%head(5) # inspect the top 5 rows


losses_data_frame<-read_excel(file_path,sheet="Losses") # read of a sheet from an xl file

losses_data_frame %>%
  select(1:5)%>%
  head(3)

colnames(losses_data_frame)

losses_tidy<- losses_data_frame %>%
  rename(Team=...1)%>%
  pivot_longer(cols=!Team,
               names_to="Years",values_to="Losses")%>%
  separate(Losses,into=c("Losses","Total"),sep=" of ")



losses_tidy%>% dim() # check the dimensions
losses_tidy%>%head(5) # inspect the top 5 rows

hockey_df <- inner_join(wins_tidy,losses_tidy,by=c("Team","Years","Total"))

hockey_df<-hockey_df %>% 
          mutate(Draws=as.numeric(Total)-(as.numeric(Wins)+as.numeric(Losses)),
                 Wins_rt= as.numeric(Wins)/as.numeric(Total),
                 Losses_rt= as.numeric(Losses)/as.numeric(Total),
                 Draws_rt= as.numeric(Draws)/as.numeric(Total)
                 )

hockey_df%>% head(5)


hockey_df%>%
  select(-Wins,-Draws,-Losses)%>%
  group_by(Team)%>%
  summarise(across(starts_with(c("Wins","Losses","Draws")),list(md=median,mn=mean),
                   .names="{substring(.col,1,1)}_{.fn}"))%>%
  arrange(desc(W_md))

#correlation function

max_cor_var<-function(df,col_name){ # function to determine the variable with maximal correlation
  
  v_col<-df%>%select(all_of(col_name)) # extract variable based on col_name
  
  df_num<-df%>%
    select_if(is.numeric)%>%
    select(-all_of(col_name)) # select all numeric variables excluding col_name
  
  correlations<-unlist(map(df_num,
                           function(x){cor(x,v_col,use="complete.obs")})) # compute correlations with all other numeric variables
  
  max_abs_cor_var<-names(which(abs(correlations)==max(abs(correlations)))) # extract the  variable name
  cor<-as.double(correlations[max_abs_cor_var]) # compute the correlation
  
  return(data.frame(var_name=max_abs_cor_var,cor=cor)) # return dataframe
}

library(palmerpenguins)

max_cor_var(penguins,"bill_length_mm")
head(penguins)

top_correlates_by_var <- function(df){
  df<-df%>%select(bill_length_mm,bill_depth_mm,flipper_length_mm,body_mass_g,year) 
  
  new_df=data.frame(bill_length_mm=NA, bill_depth_mm=NA, flipper_length_mm=NA
                    ,body_mass_g=NA,year=NA)
  
  for (i in 1:5) {
   col_name<-names(df)[i]
   res<- max_cor_var(df,col_name)
   new_df[i] <- res$var_name[1]
   }
  
  return(new_df)
 
} 
  
result<-penguins%>%
  top_correlates_by_var()

result

res<- penguins %>%
      group_by(species)%>%
      nest() %>%
      mutate(new_data=map(data,top_correlates_by_var))%>%
      select(-data)%>%
      unnest(cols=new_data)
res
