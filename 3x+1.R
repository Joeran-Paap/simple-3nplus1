#rm(list=ls()) #clear workspace

three_n_plus_one <- function(N){                            #N = starting value
  n <- list()                         #create placeholder for list of sequence
  s <- 0                              #set up iteration counter
  n[[length(n)+1]] = list(s = s, N=N)
repeat{                         # start of loop        
  s <- s+1                      # iteration counter
  if(N %% 2 ==0){ N <- N/2      # if number is even, divide by 2
  } else{N <- 3*N+1             # else, apply formular 3*n+1
  }
  n[[length(n)+1]] = list(s = s, N=N) # saves sequence in a list
  end <- N                         # store last calculated value in end variable
  #print(N)                       # outcomment if you want the sequence to be printed
  
  if(end==1) {                  # Break condition of repeat-loop
                                # if the value that was calculated last is equal to 1, the problem is stuck in 3x+1 problem
    break                       # i.e., if last value = 1, repeat loop is entering the sequence: 1 -> 4 -> 2 -> 1
  }
}
  return(dplyr::bind_rows(n))   #return sequence as data frame
}

sequence <- three_n_plus_one(9)
#simple plot of sequence
plot(sequence$N~sequence$s,
     xlab="iteration",
     ylab="n",
     xlim=c(min(sequence$s),max(sequence$s)),
     main=paste("Start = ",as.character(sequence$N[1])));lines(sequence$N~sequence$s); points(sequence$N[1]~sequence$s[1],col="blue")


