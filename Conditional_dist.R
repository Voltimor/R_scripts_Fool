setwd("~/Python/DurakAI-master/09112016_Durak_v.4.2/R_scripts")

u <- read.csv('Utility_1M.csv', header = FALSE);
t <- read.csv('Trumps_1M.csv', header = FALSE);

suits <- c(rep('C',9),rep('D',9),rep('H',9),rep('S',9))
card_names <- c(
  '6 clubs', '7 clubs', '8 clubs', '9 clubs', '10 clubs', 'J clubs', 'Q clubs', 'K clubs', 'A clubs',
  '6 diamonds', '7 diamonds', '8 diamonds', '9 diamonds', '10 diamonds', 'J diamonds', 'Q diamonds', 'K diamonds', 'A diamonds',
  '6 hearts', '7 hearts', '8 hearts', '9 hearts', '10 hearts', 'J hearts', 'Q hearts', 'K hearts', 'A hearts',
  '6 spades', '7 spades', '8 spades', '9 spades', '10 spades', 'J spades', 'Q spades', 'K spades', 'A spades'
)

N<-1000000
utility_cond<-matrix(,nrow=N, ncol=36)

for (j in 1:36)
  {
    s<-suits[j]
    for (i in 1:N)
      {
      z<-substr(t[i,j],2,2);
      if (s!=z) utility_cond[i,j]<-u[i,j]
      }
  } 

pdf("Cross_conditional_Utility_1M.pdf")
for (j in 1:9){
  
  p1 <- hist(utility_cond[,j],col=rgb(0,0,1,1/4),xlim=c(-40,40),ylim=c(0,200000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j], "\n Mean = ", mean(utility_cond[,j],na.rm = TRUE), "\n Std.Dev = ", sd(utility_cond[,j],na.rm = TRUE) ));                     
  p2 <- hist(utility_cond[,j+9],col=rgb(1,0,0,1/4),xlim=c(-40,40),ylim=c(0,200000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+9], "\n Mean = ", mean(utility_cond[,j+9],na.rm = TRUE), "\n Std.Dev = ", sd(utility_cond[,j+9],na.rm = TRUE) ));
  p3 <- hist(utility_cond[,j+18],col=rgb(0,1,0,1/4),xlim=c(-40,40),ylim=c(0,200000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+18], "\n Mean = ", mean(utility_cond[,j+18],na.rm = TRUE), "\n Std.Dev = ", sd(utility_cond[,j+18],na.rm = TRUE) ));
  p4 <- hist(utility_cond[,j+27],col=rgb(0,1,1,1/4),xlim=c(-40,40),ylim=c(0,200000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+27], "\n Mean = ", mean(utility_cond[,j+27],na.rm = TRUE), "\n Std.Dev = ", sd(utility_cond[,j+27],na.rm = TRUE) ));
  plot( p1, col=rgb(0,0,1,1/4),xlab='Change in Utility',ylab='Frequency', 
        xlim=c(-40,40),ylim=c(0,200000), main=paste("Cross-Histograms of \n",card_names[j],',',card_names[j+9],',',card_names[j+18],',',card_names[j+27]))  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(-40,40),ylim=c(0,200000),add=T)  # second
  plot( p3, col=rgb(0,1,0,1/4), xlim=c(-40,40),ylim=c(0,200000),add=T)
  plot( p4, col=rgb(1/4,0,0,1/4), xlim=c(-40,40),ylim=c(0,200000),add=T)
  
}
dev.off()


delta_mean<-c(rep(0,36))
for(j in 1:36)
{
  delta_mean[j]<-mean(u[,j])-mean(utility_cond[,j],na.rm = TRUE)
}

plot(delta_mean,type = 'l')
barplot(delta_mean)