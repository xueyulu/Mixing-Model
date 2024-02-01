library(ggplot2)
getwd()
setwd("D:/0_code_R_/Monte Carlo Simulation")
rm(list=ls())

D199Hg=-0.01 #sample
SD1=0.03 #SD
min1=D199Hg-SD1 
max1=D199Hg+SD1

mix=0.05 #mix
SD2=0.17 #SD
min2=mix-SD2 
max2=mix+SD2 

x=1 #counter
y=1 #counter
n=10000 #number of simulations

#creating a data frame
columns <- c('Sample','Mix','Excess')
dat <- data.frame(matrix(nrow=n,ncol=length(columns)))
colnames(dat) <- columns

#pseudorandom number generation
#sample: isotope signature of each sample
for (i in 1:1e+10){
  m <- D199Hg + rnorm(1)*SD1
  if(m <= max1 & m >= min1){
    dat[x,1] = m
    x=x+1
  }
  if(x>n){
    break
  }
}

#mix: isotope signature of conservative source mixing
for (i in 1:1e+10){
  m2 <- mix + rnorm(1)*SD2
  if(m2 <= max2 & m2 >= min2){
    dat[y,2] = m2
    y=y+1
  }
  if(y>n){
    break
  }
}

#Excess
dat$Excess <- dat$Sample-dat$Mix

#plot
p <- ggplot(mapping = aes(x = dat$Excess, y = ..density..)) +
  geom_histogram(fill = "steelblue3", alpha = .8,
                 col = "steelblue3", bins = 100) +
  xlab("Excess") +
  theme(axis.text = element_text(size = 10))
p

#mean and SD of Excess
round(mean(dat$Excess),2)
round(sd(dat$Excess),2)