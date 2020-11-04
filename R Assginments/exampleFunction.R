is_prime <- function(num){
  
  # This example function takes as input a positive integer and outputs Boolean - TRUE if and only if the input is prime.
  
  stopifnot(is.numeric(num),num%%1==0,num>=0) # Stop if the input is not a non-negative integer
  
  t_val <- TRUE # Initialise truth value output with TRUE
  
  if(num<2){
    
    t_val<-FALSE # Output FALSE if input is either 0 or 1
  
    }else if(num>2){
      
      for(i in 2:sqrt(num)){ # Check possible divisors i no greater than sqrt(num)
      
        if(num%%i==0){
          t_val<-FALSE
          break     # if i divides num then num is not prime
          
      }
    }
  }
  
  return(t_val) # return the truth value which says whether or not num is prime
}

## Now let's look at how the cumulative number of primes cp less than or equal x varies with x

n<- 100# number of iterations
x<-seq(n) # a sequence of length n
cp<-integer(n) # a vector of zeros of length n

s=0 # initilise s

for(i in x){
  
  if(is_prime(i)){
    s=s+1
  } # if i is prime then add one to the cumulative sum
  
  cp[i]=s # update the vector
  
}

plot(x,cp)
