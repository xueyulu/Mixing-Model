library(ggplot2)
getwd()
setwd("D:/0_code_R_/Monte Carlo Simulation")
rm(list=ls())

m=0 #D200Hg value of sample
min=m-0.03 #value-SD
max=m+0.03 #value+SD
x=1 #counter
y=1 #counter
z=1 #counter
n=10000 #number of simulations

#creating a data frame
columns <- c('Hg0','HgII','Sample','f1')
dat <- data.frame(matrix(nrow=n,ncol=length(columns)))
colnames(dat) <- columns

#pseudorandom number generation
#Hg(0): isotope signature of Hg(0) end-member
for (i in 1:1e+10){
  Hg0 <- -0.07 + rnorm(1)*0.02
  if(Hg0 <= -0.05 & Hg0 >= -0.09){
    dat[x,1] = Hg0
    x=x+1
  }
  if(x>n){
    break
  }
}

#Hg(II): isotope signature of Hg(II) end-member
for (i in 1:1e+10){
  HgII <- 0.07 + rnorm(1)*0.08
  if(HgII <= 0.15 & HgII >= -0.01){
    dat[y,2] = HgII
    y=y+1
  }
  if(y>n){
    break
  }
}

#Sample: isotope signature of each sample
for (i in 1:1e+10){
  Sample <- m + rnorm(1)*0.03
  if(Sample >= min & Sample<= max){
    dat[z,3] = Sample
    z=z+1
  }
  if(z>n){
    break
  }
}

#Calculation of f1
#f1:fraction of Hg(0)
for (i in 1:n) {
  if (dat[i,3] < dat[i,1]) dat[i,4] <- 1
  else if (dat[i,3] < dat[i,2] & dat[i,3] > dat[i,1]) dat[i,4] <- (dat[i,2]-dat[i,3])/(dat[i,2]-dat[i,1])
  else if (dat[i,3] > dat[i,2]) dat[i,4] <- 0
}

#plot
p <- ggplot(mapping = aes(x = dat$f1, y = ..density..)) +
  geom_histogram(fill = "steelblue3", alpha = .8,
                 col = "steelblue3", bins = 100) +
  xlab("f1") +
  theme(axis.text = element_text(size = 10))
p

#mean and SD of f1
round(mean(dat$f1),2)
round(sd(dat$f1),2)
