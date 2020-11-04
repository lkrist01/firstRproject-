#Assignment 1

animals <- c( "Snake"   ,"Ostrich", "Cat"     ,"Spider") 
num_len <- c(0, 2, 4, 8)
df <-data.frame(animals,num_len)

city_name <- c( "Bristol", "Manchester", "Birmingham", "London") # vector of city names
population <- c(0.5,0.5,1,9) # vector of populations
cities_populations_df <-data.frame(city_name,population) # we can generate a data frame like this

x <- seq(12,2,by = -2)

X <- matrix(x,2,3)

Y <- matrix(1:4,2,2)

#inverse Y 
res<- solve(Y)%*%X


myfirstfunc <- function(n){
  sum=0
  for (i in 0:n-1) {
    if (i%%2==0 | i%%7==0) {
      sum <-sum+i
    }
  }
  return(sum)
}


#Assignment 2






