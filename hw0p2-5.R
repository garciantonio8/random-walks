library("ggplot2")

#---------------1-D walks xt vs t---------------#

d1 = data.frame()
t1 = data.frame()
x1 = c()
t = c()


x1[1] = 0 
t[1] = 0

for (k in 1:100){
  
  Q = 10
  
  x1[k+1] = x1[k] + runif(Q) - 0.5
  t[k+1] = t[k] + 1
  
  d1 = data.frame(x1)
  t1 = data.frame(t)
  
}

hist(x1,50)

plot1d = rbind(data.frame(t1,d1))
plot(plot1d)
#print(plot1d)

g1 = ggplot(plot1d, aes(t, x1)) + geom_point(col='darkgreen',size = 0.2) + theme_bw()
g1 = g1 + theme_bw(base_size=18)+xlab(expression(paste('t'))) + ylab(expression(paste('x'[t])))
g1 = g1 + ggtitle("Random Walk: 10,000 Steps") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth()
print(g1)

#--------Random walk 2D: x and y---------#


walk = data.frame()
x = c()
y = c()

x[1] = 0
y[1] = 0

for (j in 1:1000000) {
  
  M = 1
  
  x[j+1] = x[j] + runif(M)-0.5
  y[j+1] = y[j] + runif(M)-0.5
  
  walk = data.frame(x,y)
  
}
#walk
g = ggplot(walk, aes(x,y)) + geom_point(col='purple', size = 0.005) + theme_bw() 
g = g + theme_bw(base_size=18)+xlab(expression(paste('x'))) + ylab(expression(paste('y')))
g = g + ggtitle("Random Walk: 1000000 Steps") + theme(plot.title = element_text(hjust = 0.5))
print(g)
#plot(walk)


#--------"Distribution" of random walks----------#
result1 = data.frame()
resultN = data.frame()

for(i in 1:10000) {

N = 1 
r.x = runif(N)-0.5 #runif(N), N is the dimension of the random # array
r.y = runif(N)-0.5

x.final = sum(r.x)
y.final = sum(r.y)


result1 = rbind(result1, data.frame(x.final,y.final))
#resultN = rbind(resultN, data.frame(x.final,y.final))

}

#result


g2 = ggplot(resultN, aes(x.final, y.final)) + geom_point(col='red',size = 0.2) 
g2 = g2 + theme_bw() + geom_point(data = result1, col = "green", size = 0.2)
g2 = g2 + theme_bw(base_size=18)+xlab(expression(paste('x'))) + ylab(expression(paste('y')))
g2 = g2 + ggtitle("End Points of Random Walks") + theme(plot.title = element_text(hjust = 0.5))
print(g2)

