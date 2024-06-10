?dbinom

  

# excercise b
  x0 <- dbinom(5, 5, 0.6) # P all head
  x5 <- dbinom(0,5,0.6) # p of all tail

  
  
  # plot the returs
  
  
  
  # exercise c
  
  
  # exerceise d: expected # tosses until get 1st head, if p=0.5, and 0=p=0.1 if samples = 100 times
  
  # use rgoem(n=100, p=0.5)
  ?rgeom
  ?poisson
  
  # excercise 2 b . its exponential
  rexp(1, 0.0000000125)
  
  #chimp = poisson
  rpois(1, 0.0000000125)
  
  #chimp = poisson
  rpois(30000, 0.0000000125)
  
  

  # Part 3
  #a- dpois
  
  
  ?dpois

  
  ratep1 <- 2*0.0000000125 * 10000 * 5000
  ratep2 <-  2*0.0000000125 * 10000 * 10000  # 0.21
  ratep3 <-   2*0.0000000125 * 10000 * 20000 # 0.14
  ratep4 <- 2*0.0000000125 * 10000 * 15000   # 0.2066
  ratep5 <- 2*0.0000000125 * 10000 * 12000    # 0.224
  ratep6 <-  2*0.0000000125 * 10000 * 50000 
  ratep7  <-  2*0.0000000125 * 10000 * 30000 
  
 p1 <-  dpois(3, ratep1)
 p2 <-  dpois(3, ratep2)
 p3 <-  dpois(3, ratep3)
 p4 <-  dpois(3, ratep4)
 p5  <-  dpois(3, ratep5)
 # play around with the value of TMRCA to see which one gets the largest p, thus far is p2.
 # 12000 workd better than `10000
 
 # `c how many times higher,
 p6  <-  dpois(3, ratep6)
 p7 <-   dpois(3, ratep7)
 p12.p31 = p5/p7  # 5.76
 p12.p50 = p5/p6 # 184
 
 
 