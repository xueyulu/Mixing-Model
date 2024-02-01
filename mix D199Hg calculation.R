library(ggplot2)
getwd()
setwd("D:/0_code_R_/Monte Carlo Simulation")
rm(list=ls())

f1=0.46 #f1
SD=0.22 #SD
min=f1-SD #f1-SD
max=f1+SD #f1+SD
x=1 #counter
y=1 #counter
z=1 #counter
n=10000 #number of simulations

#creating a data frame
columns <- c('Hg0','HgII','f1','Mix')
dat <- data.frame(matrix(nrow=n,ncol=length(columns)))
colnames(dat) <- columns

#pseudorandom number generation
#Hg(0): isotope signature of Hg(0) end-member
for (i in 1:1e+10){
  Hg0 <- -0.29 + rnorm(1)*0.08
  if(Hg0 <= -0.21 & Hg0 >= -0.37){
    dat[x,1] = Hg0
    x=x+1
  }
  if(x>n){
    break
  }
}

#Hg(II): isotope signature of Hg(II) end-member
for (i in 1:1e+10){
  HgII <- 0.21 + rnorm(1)*0.42
  if(HgII <= 0.63 & HgII >= -0.21){
    dat[y,2] = HgII
    y=y+1
  }
  if(y>n){
    break
  }
}

#Sample: isotope signature of each sample
for (i in 1:1e+10){
  f <- f1 + rnorm(1)*SD
  if(f <= 1 & f >= 0 & f >= min & f<= max){
      dat[z,3] = f
      z=z+1
  }
  if(z>n){
    break
  }
}

#Sample: isotope signature of each sample
dat$mix <- dat$Hg0*dat$f1+dat$HgII*(1-dat$f1)

#plot
p <- ggplot(mapping = aes(x = dat$mix, y = ..density..)) +
  geom_histogram(fill = "steelblue3", alpha = .8,
                 col = "steelblue3", bins = 100) +
  xlab("D199Hg") +
  theme(axis.text = element_text(size = 10))
p

#mean and SD of calculated D199Hg
round(mean(dat$mix),2)
round(sd(dat$mix),2)
