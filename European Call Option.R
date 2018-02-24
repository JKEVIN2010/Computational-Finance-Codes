#This program computes and returns  the price of a European Call option (VECO)
#The program takes in the inputs r = interest rate, s0 = stock price at time t = 0
#S1.h = stock price in good condition at t = 1, s1.t = stock price in bad conditions
#at t = 1 and K = strike price of the option

rm(list = ls()) #Clear the workspace

exotic_option <- function(r,s0, s1.h, s1.t,k){
  
  u <- s1.h/s0  
  
  d <- s1.t/s0
  
  v1.h <- s1.h - k
  
  p <- ((1+r) - d)/(u - d)
  
  q <- (u - (1+r))/(u - d)
  
  v1.h <- s1.h - k
  
  v.eco <- (1/(1+r))*(p*v1.h)
  
  return(v.eco)
}

