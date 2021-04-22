

#Question 2:
#(a) 
  #Answer: in pdf file.
#(b)
  #Answer: in pdf file.
#(c) 
  #Answer: in pdf file.
  
# (d) Calculate the variance of both the Monte Carlo and importance sampling
# estimators of I, assuming you can take n samples. Which estimator would
# be better? [7 points]
  # Answer:

  # Starting with Monte Carlo method:
   n<-1000
   u21<-runif(n,min=1,max=2)
   h21<-function(x){
     x^2
   }
   h.fx21<-h21(u21)
   #Calculate the estimate
   i.hat21<-mean(h.fx21)
   
   #the variance of the Monte Carlo estimators of I:
   var.ihat21<-var(h.fx21)/n
   
   ## Important sampling methods##:
   
   #h(x)=x^2
   #f(x)=1 (for(1<=x<=2))
   #g(x)=4/15*x^3
   
   hfg2<-function(x){
     15/4/x
   }
   
   #simulate x from g(x),
   sim.g<-function(n){
     u<-runif(n)
     (15*u+1)^(1/4)
   }
   
   x22<-sim.g(n)
   h.fx22<-hfg2(x22)

   
   #The variance of estimate I of important samping is :
     var.ihat22<-var(h.fx22)/n  
     
   
   #Comments: The estimator of important sampling is better,because
     #it has smaller variance than Monte Carlo method.
     .
     
  #(e) In R calculate the importance sampling estimate of I along with a 95% 
     #confidence interval. 
     #Answer:
     
     #Calculate the estimate I of important sampling is :
     i.hat22<-mean(h.fx22)
     
     sd.h.xi22 <- sd(h.fx22)
     #95% confidence interval of estimator using important sampling is:
     ci22.low <- i.hat22 - 1.96*sd.h.xi22/sqrt(n)
     ci22.high <- i.hat22 + 1.96*sd.h.xi22/sqrt(n)
     c(ci22.low, ci22.high)
     
  ##Question 3##
     #(a) 
     #Answer: in pdf file.
     #(b)
     #Answer: in pdf file.
     
     #c)
     n<-1000
     #simulate x from g(x),
     sim.g<-function(n){
       u<-runif(n)
       (15*u+1)^(1/4)
     }
     #function of f(x)/g(x)=45/28/x:
     x3<-sim.g(n)
     w<-45/(28*x3)
    #Normalizing weights
     w.norm<-w/sum(w)
     # Sampling with replacement
     s<-sample(x3, size=n, replace=TRUE, prob=w.norm)
     
     #checking the distribution
     
     hist(s,probability=TRUE)
     curve(3/7*x^2,add=TRUE,col="blue",lwd=2)
   
     #comments:The line fit well on the histogram chart.
     
   # d) 
     #Answer: in pdf file.
     
     
     # e) 
     #Answer: in pdf file.
     
    # • Inverse transform method
     n<-40
     u3<-runif(n)
     x3<-(7*u3+1)^(1/3)
     
     hist(x3,probability=TRUE)
     curve(3/7*x^2,add=TRUE,col="blue",lwd=2)
     
     
     #• Accept-reject sampling

          
 # Question 4
# In R simulate data as X1, . . . , Xn ∼ Exponential(λ), where λ = 5. The statistic we
 # want to estimate is the logarithm of the rate parameter θ = log(λ) and we’ll use
 # the estimator θb = − log(x)     

#(a) Perform non-parametric bootstrap to estimate the variance of the estimator
#θb. [5 points]
     
     #Answer: 
     
     n1<-1000
     lamba4<-5
     
     x4<-rexp(n1,rate=lamba4)
     
     B4 <- 1000 # Number of bootstrap samples to take
     
     # our estimate of lambda
     theta_hat1 =-log(mean(x4))
     
     ##### Non-parametric bootstrap #####
     theta.star4 <- rep(0, B4)
     for (i in 1:B4) {
       # Subsample with replacement from vector x
       x.sub4 <- sample(x4, size=n1, replace=TRUE)
       # Calculate mean of subsample
       theta.star4[i] <--log(mean(x.sub4))
     }
     
     theta.star4
     
     # the variance of the estimator:
        theta_var<- var(theta.star4)
     
     
# (b) Explain how you would modify your code in part (a) to perform parametric
# bootstrap. State any additional assumptions that you would need to make
# to do this. You don’t need to write the code for this part. [3 points]
     
     #Answer: 
        #to perform parametric bootstrap,
      #First, we need to estimate lambda: 
        lambda_hat_1<-1/mean(x4)
        
    # then we replace :
        x.sub4 <- sample(x4, size=n1, replace=TRUE)
      by:
        x.sub4<-rexp(n1,rate=lambda_hat_1)
     
       
 # (c) Construct a normal and pivot confidence interval using the estimated variance
 # from part (a). Do you think the normal confidence interval is appropriate?
 #   Justify your answer. [3 points]
    
     #Answer:
      se.boot1<-sd(lambda.star4)
    # Normal confidence interval for non-parametric procedures:
      c(theta_hat1-1.96*se.boot1, theta_hat1+1.96*se.boot1) 
      
    # Pivot CI for non-parameter:
      alpha <- 0.05
      q1 <- quantile(theta.star4, probs = c(1-alpha/2, alpha/2))
      c(2*theta_hat1-q1[1],2*theta_hat1-q1[2])     
      
      
      qqnorm(theta.star4)  
#We can see when we plot those estimate theta sample, the plot has a linear shape.
# It means it follow normal distribution,so the normal confidence interval is appropriate     
      

##Question 5 ##
#Suppose you want to optimize the following function:
      # a)
      #Answer: in pdf file.

  # (b) Write R code to optimize h(x) on the interval [0, 1] using Newton-Raphson.
  # [4 points]
      
  # Answer:
  #function h(x)
     h<-function(x){
       sin(x)-x^4/4
     } 
     
  curve(h(x), xlim=c(0,1), xname = "x")  
  # the first derivative of h(x):
  h.prime<-function(x){
    cos(x)-x^3
  }
  # the second derivative of h(x):
  h.prime2<-function(x){
    -sin(x)-3*x^2
  }
  
  # Now doing the loop for Newton-Raphson
  l <- 0.01
  l.new <- l + 0.01
  count <- 1
  max.iter <- 100
  
  while(abs((l.new-l)/l)>1e-6) {
    if (count>max.iter) stop("Maximum number of iterations reached")
    cat("l =", l, "\n")
    l <- l.new
    # Newton-Raphson step (also could have done 2*l - l^2*xbar as seen in lecture 7)
    l.new <- l - h.prime(l)/h.prime2(l)
    count <- count + 1
  }
  
  #when
  l
  #The optimization of h(x):
  h(l)
  
# (c) Write R code to optimize h(x) on the interval [0, 1] using a stochastic search
# with a Uniform(0, 1) distribution. [3 points]
  
  # Answer:
  n.u <- 1000
  u5 <- runif(n.u,0,1)
  
  h.u <- h(u5)

  wm <- which.max(h.u)
  
  # Extracting the value of u that corresponds to the max of h(u)
  max.u <- u5[wm]
  max.u # this is our esimate of argmax h(x)
  
  h(max.u)
# (d) Write R code to optimize h(x) on the interval [0, 1] using a stochastic search
# with a Beta(2, 5/4) distribution. [3 points]
  # Answer:
  n.u <- 1000
  b5<-rbeta(n.u,shape1 = 2,shape2=5/4)
  h.ub<-h(b5)
  
  wmb<-which.max(h.ub)
  
  max.ub <- b5[wmb]
  max.ub # this is our esimate of argmax h(x)
  
# (e) Which of the distributions in (c) vs (d) do you think is preferable for a
# stochastic search in this case? Explain. [2 points]
  
  #beta shape
  curve(dbeta(x,shape1 = 2,shape2=5/4), xlim=c(0,1), xname = "x")  
  
  
  # comments: I think d) is more preferabel for stochastic search in this case,
  #because the shape of beta is close to h(x).
  
  # (f) Suppose now that h(x) = (x − 3)(x + 6)[1 + sin(60x)] on x ∈ (0, 10). Plot
  # a curve of h(x). Do you think Newton-Raphson would work well to find the
  # maximum? Is there another algorithm that you think would work better?
  #   Explain your answers. [3 points]
  
  # Answer:
  
  h1<-function(x){
    (x-3)*(x + 6)*(1 + sin(60*x))
  }
  # Plot a curve of h(x):    
  curve(h1(x), xlim=c(0,10), xname = "x",n=500)
  
  
  #Comments: I think Newton-Raphson would not work well to find the
# maximum,because there is many local maximums and many global maximum.
  
  #Yes,there is,the easy method is a stochastic search with Uniform(0,10),but 
  # we need to have a large n to estimate the maximum. However,Simulated annealing
  # is very hard to program.
  
  # by using the math :60*10/2/pi=95.49297.There is 95 cycles, up and down.
  #--- the second derivative of (x-3)(x+6) is 2*x+3, so on [0,10] is a increasing
  #function.
  #Let A=(x-3)(x+6) , and B=1+sin(60*x)-> 2>=B>=0
  # h(x)=A*B. 
  
  # we can find the exact x is by calculate x at the last cycle when sin(60*x)=1
  #=> 60*x = pi/2+k*2*pi, when k is a integer number,find the largest k,with  x in [0,10]
  # => x= (pi/2+k*2*pi)/60
  #when max k= 95, x=9.974557, max h(x) is 222.8309
  # 
  
  ####Using a stochastic search###
  
  n.u <- 200000
  u6 <- runif(n.u,0,10)
  h.ub6<-h1(u6)
  wmb6<-which.max(h.ub6)
  
  max.ub6 <- u6[wmb6]
  max.ub6 # this is our esimate of argmax h(x)
  
  # the maximum h(x)  is:
  h1(max.ub6)  
  
  
  ###Simulated annealing####
  
  rho <-2*pi/60
  max.iter <- 4000
  x <- 4
  x.new <- x + 2*pi/60
  temp <- 100
  iter <- 1
  trace <- c(x, rep(0, max.iter-1))
  
  while(abs((x.new-x)/x) > 1e-6) {
    if (iter>max.iter) { 
      # If we get to maximum number of iterations, stop.
      break 
    }
    x <- x.new
    if (iter%%4000==0) {
      cat("iter =", iter, ":\t x =", x, "\n")
    }
    
    # Simulate from g(x)
    gx <- runif(1, -rho, rho)
    # Calculate proposal value
    x.p <- x + gx
    
    ### We need to set this one, to make sure, x.p is not larger than 10.
    if(x.p>10){
      
      x.p=10
    }
    # Update temperature
    temp <- temp/(log(iter+1))
    
    # Calculate Delta h
    dh <- h1(x.p) - h1(x)
    # Calculate probability of accepting new sample
    p.acc <- min(exp(dh/temp), 1)
    acc <- rbinom(n=1, size=1, prob=p.acc)
    if (acc==1) {
      # If we accept, set x.new to x.p
      x.new <- x.p
    } else{
      # If we don't accept, temporarily change x to not end the loop
      # (It will get reset at the beginning of the next iteration of the loop)
      x <- x.new + 2*pi/60
    }
   
    trace[iter+1] <- x.new
    iter <- iter + 1
  }
  
  plot(trace, type="l")
  x.new
  h1(x.new)
 
    ## comment,We need to set rho as 2*r/60 ( a cycle of sin 60*x), 
  # and the number of iter has to be large , ( i suggest n>4000), 
  #because there are many local maximum,which spent our iter.
  
  