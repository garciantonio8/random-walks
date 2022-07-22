library("ggplot2")

n = 3 #1,2,3,5
x = c()
f.step = c()
for(j in 1:10000){

  for(i in 1:n){
    
    x[i] = runif(1, min = -0.5, max = 0.5)
    
  }
  
  x.final = sum(x)
  f.step[j] = x.final 
  

}

hist(f.step,50)